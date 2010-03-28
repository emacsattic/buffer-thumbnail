;;; buffer-thumbnail.el
;;
;; Copyright (C) 2005 Thien-Thi Nguyen
;;
;; This file is part of ttn's personal elisp library, released under GNU
;; GPL with ABSOLUTELY NO WARRANTY.  See the file COPYING for details.
;;
;;; Commentary:
;;
;; Maintain an XPM interpretation of buffer contents.

(require 'xpm-fulmination)

(defvar buffer-thumbnail nil
  "XPM data for a buffer, set by `buffer-thumbnail-update'.")

;;;###autoload
(defun buffer-thumbnail-update (beg end)
  "Make variable `buffer-thumbnail' buffer-local and update it."
  (interactive "r")
  (let* ((width (save-excursion
                  (let ((w (progn (goto-char beg)
                                  (end-of-line)
                                  (current-column))))
                    (while (< (point) end)
                      (end-of-line 2)
                      (setq w (max w (current-column))))
                    w)))
         (height (1+ (count-lines (point-min) (point-max))))
         (up (ceiling (/ (sqrt (+ width (* height width))) width)))
         (fw (* up width))
         (fh (ceiling (/ (* 1.0 height) up)))
         (curbuf (current-buffer))
         (fulmbuf (xpmfulm-buffer "bufferthumb" fw fh
                                  '((32 . "#000000000000")
                                    (?# . "#FFFF0000FFFF"))))
         (row 0) (col 0)
         p q)
    ;; In the following, the "abstraction violation!" warnings are actually a
    ;; call to improve the abstraction.  At least, that's what the enfeebled
    ;; drunk programmer mumbled to me.  "But why don't you fix that in the
    ;; source straight away?", I asked sincerely.  He laughed and said, "Your
    ;; two minds are ready to filter the polarization, fool!  When you get
    ;; your ninety degrees properly settled, then we'll talk!"  I hesitated,
    ;; wary of vomit and invective, but there was no other sound in the dark.
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (setq p (point)
              q (progn (end-of-line) (point)))
        (with-current-buffer fulmbuf
          ;; abstraction violation!
          (goto-char (+ 1 (* row (1+ fw)) col))
          (insert-buffer-substring-no-properties curbuf p q)
          (delete-char (- q p)))
        (forward-line 1)
        (incf row)
        (when (= 0 (mod row fh))
          (setq row 0 col (+ width col)))))
    (with-current-buffer fulmbuf
      ;; more abstraction violation!
      (let ((v (apply 'vector (mapcar (lambda (n)
                                        (make-string n ?#))
                                      (number-sequence 0 width))))
            (cookie-loc (- (point-max) 3)))
        (goto-char (point-min))
        (while (re-search-forward "[^ \n]+" (- (point-max) 3) t)
          (replace-match (aref v (- (match-end 0) (match-beginning 0))) t t))))
    (set (make-local-variable 'buffer-thumbnail)
         (xpmfulm-as-xpm fulmbuf))))

;;;###autoload
(defun buffer-thumbnail-browse (&optional beg end)
  (interactive)
  (unless (and beg end)
    (setq beg (point-min)
          end (point-max)))
  (let ((start (current-time))
        xpm diff)
    (buffer-thumbnail-update beg end)
    (setq xpm buffer-thumbnail)
    (switch-to-buffer
     (generate-new-buffer
      (concat "*buffer thumbnail* " (buffer-name))))
    (set (make-local-variable 'buffer-thumbnail) xpm)
    (setq major-mode 'buffer-thumbnail-browse
          mode-name "Buffer Thumbnail Browse")
    (use-local-map (let ((m (make-sparse-keymap)))
                     (define-key
                       m "w" (lambda (filename)
                               (interactive "FWrite XPM to file: ")
                               (let ((data (plist-get (cdr buffer-thumbnail)
                                                      :data)))
                                 (with-temp-buffer
                                   (insert data)
                                   (write-file filename)))))
                     m))
    (erase-buffer)
    (insert-image xpm)
    (forward-char -1)
    (unless (member (setq diff (time-since start)) '((0 0) (0 1)))
      (message (format-time-string "%s seconds" diff)))))

(provide 'buffer-thumbnail)
;;; buffer-thumbnail.el ends here
