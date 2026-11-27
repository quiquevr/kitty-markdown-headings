;;; kitty-markdown-headings.el --- True-to-size Markdown headers in Kitty via OSC 66 -*- lexical-binding: t; -*-

;;; Commentary:
;; Extends `kitty-graphics.el' so `markdown-mode' and `gfm-mode' buffers
;; render ATX (`#'..`######') and Setext (`===' / `---') headings at true
;; physical typographic scales inside the Kitty terminal using the Kitty
;; Text Sizing Protocol (OSC 66).
;;
;; Live-editing behavior:
;; - Headings render at scaled physical font sizes (H1 2.0x, H2 1.6x,
;;   H3 1.33x, H4 1.15x) while reading and navigating vertically.
;; - Moving the cursor inside a heading (`C-f', `<right>', `C-e', etc.) or
;;   modifying it reveals the raw `# ...' Markdown source in place (keeping
;;   row height reserved so surrounding text does not jump).
;; - Moving the cursor off the edited line (e.g. `RET', `<down>', `<up>')
;;   automatically re-renders the updated or newly typed heading at scale.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'markdown-mode)
(require 'kitty-graphics)

(defgroup kitty-markdown-headings nil
  "True-to-size Markdown headings in Kitty terminal via OSC 66."
  :group 'kitty-graphics
  :group 'markdown)

(defcustom kitty-markdown-heading-scales
  '((1 . 2.0)
    (2 . 1.6)
    (3 . 1.33)
    (4 . 1.15))
  "Alist mapping Markdown heading level (1-6) to OSC 66 visual scale factor.
Levels omitted or <= 1.0 render at normal 1x cell height using their
`markdown-header-face-N' styling."
  :type '(alist :key-type integer :value-type number)
  :group 'kitty-markdown-headings)

(defcustom kitty-markdown-headings-auto t
  "When non-nil, automatically enable OSC 66 scaled headings in `markdown-mode'."
  :type 'boolean
  :group 'kitty-markdown-headings)

(defcustom kitty-markdown-headings-live-reveal t
  "When non-nil, reveal raw Markdown while the cursor edits inside a heading."
  :type 'boolean
  :group 'kitty-markdown-headings)

(defvar-local kitty-markdown--dirty-beg nil
  "Start position of un-scanned buffer edits in current Markdown buffer.")

(defvar-local kitty-markdown--dirty-end nil
  "End position of un-scanned buffer edits in current Markdown buffer.")

(defvar-local kitty-markdown--pending-line-marker nil
  "Marker on a line currently being typed by the user, scanned once point leaves.")

(defconst kitty-markdown--vertical-commands
  '(next-line
    previous-line
    line-move
    scroll-up-command
    scroll-down-command
    cua-scroll-up
    cua-scroll-down
    evil-next-line
    evil-previous-line
    evil-next-visual-line
    evil-previous-visual-line)
  "Commands that step vertically past headings without entering edit-reveal.")

;;;; Heading parsing & styled segment extraction

(defun kitty-markdown--heading-scale (level)
  "Return configured scale factor for Markdown heading LEVEL."
  (alist-get level kitty-markdown-heading-scales))

(defun kitty-markdown--extract-segments (title-beg title-end level)
  "Extract `(TEXT . SEGMENTS)' from TITLE-BEG..TITLE-END for heading LEVEL.
Strips inline delimiters marked with `invisible' = `markdown-markup'
\(such as `**', `` ` ``, and link URLs) while preserving per-run faces."
  (save-excursion
    (goto-char title-beg)
    (skip-chars-forward " \t" title-end)
    (setq title-beg (point))
    (goto-char title-end)
    (skip-chars-backward " \t" title-beg)
    (setq title-end (point)))
  (when (< title-beg title-end)
    (let ((base-face (intern (format "markdown-header-face-%d" (min level 6))))
          (p title-beg)
          (segments nil))
      (while (< p title-end)
        (let* ((next-inv (next-single-property-change p 'invisible nil title-end))
               (next-face (next-single-property-change p 'face nil title-end))
               (next (min next-inv next-face))
               (inv (get-text-property p 'invisible)))
          (unless (eq inv 'markdown-markup)
            (let ((chunk (buffer-substring-no-properties p next))
                  (face (or (get-text-property p 'face) base-face)))
              (when (> (length chunk) 0)
                (if (and segments (equal (cdar segments) face))
                    (setcar (car segments) (concat (caar segments) chunk))
                  (push (cons chunk face) segments)))))
          (setq p next)))
      (setq segments (nreverse segments))
      (let ((text (mapconcat #'car segments "")))
        (when (> (string-width text) 0)
          (cons text segments))))))

(defun kitty-markdown--parse-heading-at (pos)
  "If POS starts or sits on a Markdown heading, return plist of heading info.
Assumes `font-lock-ensure' has run on the surrounding region."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (when (markdown-heading-at-point (point))
      (let ((beg (match-beginning 0))
            (end (match-end 0))
            level title-beg title-end)
        (cond
         ;; ATX heading: # .. ######
         ((match-beginning 4)
          (setq level (length (string-trim (match-string-no-properties 4)))
                title-beg (match-beginning 5)
                title-end (match-end 5)))
         ;; Setext H1: ===
         ((match-beginning 2)
          (setq level 1
                title-beg (match-beginning 1)
                title-end (match-end 1)))
         ;; Setext H2: ---
         ((match-beginning 3)
          (setq level 2
                title-beg (match-beginning 1)
                title-end (match-end 1))))
        (when (and level title-beg title-end)
          (let ((extracted (kitty-markdown--extract-segments title-beg title-end level)))
            (when extracted
              (list :beg beg
                    :end end
                    :level level
                    :text (car extracted)
                    :segments (cdr extracted)))))))))

;;;; Overlay creation & region scanning

(defun kitty-markdown--point-actively-editing-span-p (beg end)
  "Return non-nil if point is actively inside BEG..END for editing."
  (and kitty-markdown-headings-live-reveal
       (eq (current-buffer) (window-buffer (selected-window)))
       (or (< beg (point) end)
           (and (= (point) end)
                (> end beg)
                (not (memq this-command kitty-markdown--vertical-commands))))))

(defun kitty-markdown--apply-heading-sizes (&optional beg end)
  "Scan Markdown headings in BEG..END and create scaled OSC 66 overlays."
  (when (and kitty-graphics-mode
             kitty-graphics--heading-sizes-enabled
             (derived-mode-p 'markdown-mode))
    (let ((scan-beg (save-excursion
                      (goto-char (or beg (point-min)))
                      (forward-line -1)
                      (point)))
          (scan-end (save-excursion
                      (goto-char (or end (point-max)))
                      (forward-line 2)
                      (point)))
          (count 0))
      (with-silent-modifications
        (font-lock-ensure scan-beg scan-end))
      (save-excursion
        (goto-char scan-beg)
        (while (< (point) scan-end)
          (let ((info (kitty-markdown--parse-heading-at (point))))
            (if (not info)
                (forward-line 1)
              (let* ((hbeg (plist-get info :beg))
                     (hend (plist-get info :end))
                     (level (plist-get info :level))
                     (scale (kitty-markdown--heading-scale level))
                     (text (plist-get info :text))
                     (segments (plist-get info :segments)))
                (when (and (kitty-graphics--heading-scaled-p scale)
                           (not (cl-some (lambda (ov)
                                           (overlay-get ov 'kitty-graphics-heading))
                                         (overlays-in hbeg hend))))
                  (if (kitty-markdown--point-actively-editing-span-p hbeg hend)
                      ;; Defer scaling until cursor leaves this line
                      (setq kitty-markdown--pending-line-marker (copy-marker hbeg))
                    (let ((ov (kitty-graphics--make-heading-overlay
                               hbeg hend text scale level)))
                      (overlay-put ov 'kitty-graphics-heading-segments
                                   (and kitty-graphics-heading-fontify-keywords segments))
                      (cl-incf count))))
                (goto-char hend)
                (forward-line 1))))))
      (when (> count 0)
        (kitty-graphics--schedule-refresh t)))))

;;;; Live editing & cursor-reveal lifecycle

(defun kitty-markdown--erase-emitted-overlay (ov)
  "Erase OV's emitted OSC 66 multicell block from the terminal immediately."
  (when (and (overlay-get ov 'kitty-graphics-heading-emitted)
             (overlay-get ov 'kitty-graphics-last-row)
             (overlay-get ov 'kitty-graphics-last-col)
             (not (display-graphic-p)))
    (kitty-graphics--sync-begin)
    (unwind-protect
        (kitty-graphics--erase-heading-at
         (overlay-get ov 'kitty-graphics-last-row)
         (overlay-get ov 'kitty-graphics-last-col)
         (or (overlay-get ov 'kitty-graphics-cols) 0)
         (or (overlay-get ov 'kitty-graphics-rows) 1))
      (kitty-graphics--sync-end))))

(defun kitty-markdown--sync-editing-padding (ov)
  "Keep vertical row height stable while OV displays raw Markdown source."
  (when (overlay-buffer ov)
    (let* ((beg (overlay-start ov))
           (end (overlay-end ov))
           (cell-s (or (overlay-get ov 'kitty-graphics-heading-cell-s) 1))
           (internal-nl (save-excursion
                          (goto-char beg)
                          (how-many "\n" beg end)))
           (pad-rows (max 0 (- cell-s 1 internal-nl)))
           (pad (when (> pad-rows 0)
                  (make-string pad-rows ?\n))))
      (unless (equal (overlay-get ov 'after-string) pad)
        (overlay-put ov 'after-string pad)))))

(defun kitty-markdown--reveal-overlay (ov)
  "Switch heading overlay OV into raw-Markdown live-editing reveal state."
  (kitty-markdown--erase-emitted-overlay ov)
  (overlay-put ov 'kitty-graphics-heading-stale t)
  (overlay-put ov 'kitty-markdown-editing t)
  (overlay-put ov 'kitty-graphics-heading-emitted nil)
  (overlay-put ov 'kitty-graphics-last-row nil)
  (overlay-put ov 'kitty-graphics-last-col nil)
  (overlay-put ov 'display nil)
  (kitty-markdown--sync-editing-padding ov)
  (kitty-graphics--invalidate-window-signatures)
  (force-window-update (selected-window)))

(defun kitty-markdown--on-overlay-modified (orig-fn ov after &rest args)
  "Ensure terminal multicell block is cleanly erased as soon as OV is edited."
  (when (and (not after)
             (derived-mode-p 'markdown-mode)
             (overlay-buffer ov)
             (overlay-get ov 'kitty-graphics-heading-emitted))
    (kitty-markdown--erase-emitted-overlay ov)
    (overlay-put ov 'kitty-graphics-heading-emitted nil))
  (apply orig-fn ov after args)
  (when (and after
             (derived-mode-p 'markdown-mode)
             (overlay-buffer ov))
    (overlay-put ov 'kitty-markdown-editing t)
    (kitty-markdown--sync-editing-padding ov)))

(defun kitty-markdown--after-change (beg end _len)
  "Record modified region BEG..END for live Markdown heading detection."
  (when (and kitty-graphics--heading-sizes-enabled
             (derived-mode-p 'markdown-mode))
    (setq kitty-markdown--dirty-beg
          (if kitty-markdown--dirty-beg (min kitty-markdown--dirty-beg beg) beg)
          kitty-markdown--dirty-end
          (if kitty-markdown--dirty-end (max kitty-markdown--dirty-end end) end))))

(defun kitty-markdown--post-command ()
  "Manage live reveal on cursor entry and re-scaling when leaving edited lines."
  (when (and kitty-graphics-mode
             kitty-graphics--heading-sizes-enabled
             (derived-mode-p 'markdown-mode)
             (not (display-graphic-p)))
    ;; 1. Reveal heading if cursor stepped inside its text to edit
    (when kitty-markdown-headings-live-reveal
      (dolist (ov kitty-graphics--overlays)
        (when (and (overlay-buffer ov)
                   (overlay-get ov 'kitty-graphics-heading)
                   (not (overlay-get ov 'kitty-markdown-editing)))
          (let ((beg (overlay-start ov))
                (end (overlay-end ov)))
            (when (kitty-markdown--point-actively-editing-span-p beg end)
              (kitty-markdown--reveal-overlay ov))))))

    ;; 2. Check stale / editing overlays: keep revealed while cursor remains on
    ;;    their line(s), and re-scale cleanly as soon as cursor moves away.
    (dolist (ov (copy-sequence kitty-graphics--overlays))
      (when (and (overlay-get ov 'kitty-graphics-heading)
                 (or (overlay-get ov 'kitty-graphics-heading-stale)
                     (overlay-get ov 'kitty-markdown-editing)))
        (if (not (overlay-buffer ov))
            (kitty-graphics--remove-overlay ov)
          (let* ((line-beg (save-excursion
                             (goto-char (overlay-start ov))
                             (line-beginning-position)))
                 (line-end (save-excursion
                             (goto-char (overlay-end ov))
                             (line-end-position))))
            (if (and kitty-markdown-headings-live-reveal
                     (<= line-beg (point) line-end))
                (kitty-markdown--sync-editing-padding ov)
              (kitty-graphics--remove-overlay ov)
              (kitty-markdown--apply-heading-sizes line-beg line-end))))))

    ;; 3. Check pending newly-typed heading line once cursor leaves that line
    (when (and kitty-markdown--pending-line-marker
               (marker-buffer kitty-markdown--pending-line-marker))
      (let* ((mpos (marker-position kitty-markdown--pending-line-marker))
             (lbeg (save-excursion (goto-char mpos) (line-beginning-position)))
             (lend (save-excursion (goto-char mpos) (line-end-position))))
        (unless (<= lbeg (point) lend)
          (set-marker kitty-markdown--pending-line-marker nil)
          (setq kitty-markdown--pending-line-marker nil)
          (kitty-markdown--apply-heading-sizes lbeg lend))))

    ;; 4. Process buffer edits on previously un-instrumented lines
    (when (and kitty-markdown--dirty-beg kitty-markdown--dirty-end)
      (let ((dbeg (max (point-min) (min kitty-markdown--dirty-beg (point-max))))
            (dend (max (point-min) (min kitty-markdown--dirty-end (point-max)))))
        (setq kitty-markdown--dirty-beg nil
              kitty-markdown--dirty-end nil)
        ;; Remove any non-editing heading overlays inside dirty region whose
        ;; underlying line is no longer a valid heading (e.g. multi-line delete/undo)
        (let ((check-beg (save-excursion (goto-char dbeg) (forward-line -1) (point)))
              (check-end (save-excursion (goto-char dend) (forward-line 2) (point))))
          (with-silent-modifications
            (font-lock-ensure check-beg check-end))
          (dolist (ov (copy-sequence (overlays-in check-beg check-end)))
            (when (and (overlay-get ov 'kitty-graphics-heading)
                       (not (overlay-get ov 'kitty-markdown-editing)))
              (let ((info (kitty-markdown--parse-heading-at (overlay-start ov))))
                (unless (and info
                             (= (plist-get info :beg) (overlay-start ov))
                             (= (plist-get info :end) (overlay-end ov))
                             (equal (plist-get info :text)
                                    (overlay-get ov 'kitty-graphics-heading-text)))
                  (kitty-markdown--erase-emitted-overlay ov)
                  (kitty-graphics--remove-overlay ov)))))
          (kitty-markdown--apply-heading-sizes check-beg check-end))))))

;;;; Integration with `kitty-graphics'

(defun kitty-markdown--heading-sgr-advice (orig-fn level)
  "Use `markdown-header-face-N' SGR in `markdown-mode' buffers."
  (if (derived-mode-p 'markdown-mode)
      (kitty-graphics--face-sgr
       (intern (format "markdown-header-face-%d" (min level 6))))
    (funcall orig-fn level)))

(defun kitty-markdown--scan-window-advice (win)
  "Extend `kitty-graphics--heading-scan-window' to `markdown-mode' buffers."
  (when (window-live-p win)
    (with-current-buffer (window-buffer win)
      (when (and kitty-graphics--heading-sizes-enabled
                 (derived-mode-p 'markdown-mode))
        (let ((margin (* 2 (max 1 (window-body-height win)))))
          (save-excursion
            (goto-char (window-start win))
            (forward-line (- margin))
            (let ((beg (point)))
              (goto-char (or (window-end win t) (point-max)))
              (forward-line margin)
              (kitty-markdown--apply-heading-sizes beg (point)))))))))

(defun kitty-markdown--apply-initial-advice (orig-fn)
  "Support full-buffer initial scan in `markdown-mode' when visible-only is nil."
  (if (and (derived-mode-p 'markdown-mode)
           (not kitty-graphics-heading-scan-visible-only))
      (kitty-markdown--apply-heading-sizes)
    (funcall orig-fn)))

(defun kitty-markdown--enable-buffer ()
  "Enable OSC 66 scaled headings in the current `markdown-mode' buffer."
  (when (and (derived-mode-p 'markdown-mode)
             kitty-graphics-mode
             (eq kitty-graphics--text-sizing-support 'scale)
             (not (display-graphic-p))
             (not kitty-graphics--heading-sizes-enabled))
    (setq kitty-graphics--heading-sizes-enabled t)
    (kitty-graphics--heading-disable-conflicting)
    (add-hook 'after-change-functions #'kitty-markdown--after-change nil t)
    (add-hook 'post-command-hook #'kitty-markdown--post-command nil t)
    (let ((win (get-buffer-window nil 'visible)))
      (if (and kitty-graphics-heading-scan-visible-only win)
          (kitty-graphics--heading-scan-window win)
        (kitty-markdown--apply-heading-sizes)))))

(defun kitty-markdown--disable-buffer ()
  "Disable OSC 66 scaled headings in the current `markdown-mode' buffer."
  (remove-hook 'after-change-functions #'kitty-markdown--after-change t)
  (remove-hook 'post-command-hook #'kitty-markdown--post-command t)
  (when kitty-markdown--pending-line-marker
    (set-marker kitty-markdown--pending-line-marker nil)
    (setq kitty-markdown--pending-line-marker nil))
  (setq kitty-markdown--dirty-beg nil
        kitty-markdown--dirty-end nil)
  (dolist (ov kitty-graphics--overlays)
    (when (overlay-get ov 'kitty-graphics-heading)
      (kitty-markdown--erase-emitted-overlay ov)))
  (kitty-graphics--org-remove-heading-sizes)
  (kitty-graphics--heading-restore-modes))

;;;###autoload
(defun kitty-markdown-heading-sizes (&optional arg)
  "Toggle true-to-size OSC 66 Markdown headings in the current buffer.
With prefix ARG, force disable."
  (interactive "P")
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not a markdown-mode buffer"))
  (unless (eq kitty-graphics--text-sizing-support 'scale)
    (user-error "Terminal does not support OSC 66 text sizing (requires Kitty >= 0.40.0)"))
  (if (or arg kitty-graphics--heading-sizes-enabled)
      (progn
        (kitty-markdown--disable-buffer)
        (message "Kitty Markdown true-to-size headings disabled"))
    (kitty-markdown--enable-buffer)
    (message "Kitty Markdown true-to-size headings enabled")))

(defun kitty-markdown--mode-hook ()
  "Auto-enable scaled headings when entering `markdown-mode'."
  (when (and kitty-markdown-headings-auto
             (not (display-graphic-p)))
    (let ((buf (current-buffer)))
      (run-at-time 0 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (kitty-markdown--enable-buffer))))))))

(defun kitty-markdown--after-text-sizing-probe (&rest _)
  "Enable scaled headings in already-open Markdown buffers once probe completes.
Fixes the startup race when opening `emacs -nw file.md' directly from the CLI,
where `markdown-mode-hook' fires before the idle-timer OSC 66 CPR probe runs."
  (when (and kitty-markdown-headings-auto
             kitty-graphics-mode
             (eq kitty-graphics--text-sizing-support 'scale))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'markdown-mode)
                     (not kitty-graphics--heading-sizes-enabled))
            (kitty-markdown--enable-buffer)))))))

;;;###autoload
(defun kitty-markdown-headings-setup ()
  "Install Kitty OSC 66 true-to-size heading support for `markdown-mode'."
  (advice-add 'kitty-graphics--heading-sgr :around
              #'kitty-markdown--heading-sgr-advice)
  (advice-add 'kitty-graphics--heading-scan-window :after
              #'kitty-markdown--scan-window-advice)
  (advice-add 'kitty-graphics--heading-apply-initial :around
              #'kitty-markdown--apply-initial-advice)
  (advice-add 'kitty-graphics--heading-modified :around
              #'kitty-markdown--on-overlay-modified)
  (advice-add 'kitty-graphics--query-text-sizing-support :after
              #'kitty-markdown--after-text-sizing-probe)
  ;; Cleanly erase/refresh headings on Markdown TAB outline cycling
  (advice-add 'markdown-cycle :before #'kitty-graphics--on-org-cycle)
  (advice-add 'markdown-shifttab :before #'kitty-graphics--on-org-cycle)
  (add-hook 'markdown-mode-hook #'kitty-markdown--mode-hook)
  (define-key markdown-mode-map (kbd "C-c C-x h") #'kitty-markdown-heading-sizes))

(provide 'kitty-markdown-headings)
;;; kitty-markdown-headings.el ends here
