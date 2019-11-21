;;; chunyang-eshell-ext.el --- Xu Chunyang's Eshell extension  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'eshell)

(defun eshell/cat-with-syntax-highlight (filename)
  "Like cat(1) but with syntax highlighting."
  (let ((existing-buffer (get-file-buffer filename))
        (buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
           (font-lock-ensure)
         (with-no-warnings
           (font-lock-fontify-buffer)))
       (buffer-string)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))

(advice-add 'eshell/cat :override #'eshell/cat-with-syntax-highlight)

(defun eshell/imgcat (&rest args)
  "Display image files."
  (unless args (error "Usage: imgcat FILE ..."))
  (dolist (img (eshell-flatten-list args))
    (eshell-printn
     (propertize " " 'display (create-image img)))))

;; Eshell command name completion for tldr man pages <http://tldr-pages.github.io>
(defvar tldr-commands nil)

(defun pcomplete/tldr ()
  (unless tldr-commands
    (setq tldr-commands
          (split-string
           (nth 1 (split-string
                   (shell-command-to-string "tldr --list")
                   "\n" t))
           ", ")))
  (pcomplete-here* tldr-commands))

(provide 'chunyang-eshell-ext)
;;; chunyang-eshell-ext.el ends here
