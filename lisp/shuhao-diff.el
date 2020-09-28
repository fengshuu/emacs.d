;; {{ diff region SDK
(defun diff-region-exit-from-certain-buffer (buffer-name)
  (bury-buffer buffer-name)
  (winner-undo))

(defmacro diff-region-open-diff-output (content buffer-name)
  `(let ((rlt-buf (get-buffer-create ,buffer-name)))
    (save-current-buffer
      (switch-to-buffer-other-window rlt-buf)
      (set-buffer rlt-buf)
      (erase-buffer)
      (insert ,content)
      (diff-mode)
      (goto-char (point-min))
      ;; Evil keybinding
      (if (fboundp 'evil-local-set-key)
          (evil-local-set-key 'normal "q"
                              (lambda ()
                                (interactive)
                                (diff-region-exit-from-certain-buffer ,buffer-name))))
      ;; Emacs key binding
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (diff-region-exit-from-certain-buffer ,buffer-name)))
      )))

;; }}

;; {{ Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b'"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a'.
If no region is selected. You will be asked to use `kill-ring' or clipboard instead.
`simpleclip' need be installed to read clipboard."
  (interactive)
  (let* (rlt-buf
         diff-output
         ;; file A
         (fa (make-temp-file (expand-file-name "scor"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory))))
         ;; file B
         (fb (make-temp-file (expand-file-name "scor"
                                               (or small-temporary-file-directory
                                                   temporary-file-directory)))))
    (when (and fa (file-exists-p fa) fb (file-exists-p fb))
      (cond
       ((region-active-p)
        ;; text from selected region
        (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
        (write-region (car tmp) (cadr tmp) fb))
       (t
        ;; text from `kill-ring' or clipboard
        (unless (featurep 'ido) (require 'ido))
        (let* ((choice (ido-completing-read "Since no region selected, compare text in:"
                                            '("kill-ring" "clipboard")))
               (txt (cond
                     ((string= choice "kill-ring")
                      (car kill-ring))
                     ((string= choice "clipboard")
                      (unless (featurep 'simpleclip) (require 'simpleclip))
                      (simpleclip-get-contents)))))
          (with-temp-file fb
            (insert txt)))))
      ;; save region A as file A
      (save-current-buffer
        (set-buffer (get-buffer-create "*Diff-regionA*"))
        (write-region (point-min) (point-max) fa))
      ;; diff NOW!
      ;; show the diff output
      (if (string= (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb))) "")
          ;; two regions are same
          (message "Two regions are SAME!")
        ;; show the diff
        (diff-region-open-diff-output diff-output
                                      "*Diff-region-output*"))
      ;; clean the temporary files
      (if (and fa (file-exists-p fa))
          (delete-file fa))
      (if (and fb (file-exists-p fb))
          (delete-file fb)))))
;; }}


(provide 'shuhao-diff)
