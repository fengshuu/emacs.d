;;; eval-before-load.el --- Like `eval-after-load' but s/after/before  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2018  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>

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
;; There is a Emacs built-in function `eval-after-load', which lets you to
;; execute some code right _after_ a library is loaded, while this package
;; provides function `eval-before-load', which lets you to execute some code
;; right _before_ a library will be loaded.
;;
;; Like the handy macro `with-eval-after-load', one `with-eval-before-load' is
;; also provided.
;;
;; If you are using `use-package', a `:pre-config' use-package keyword is
;; provided, I hope it is not buggy, according to my current Emacs Lisp
;; knowledge, how `use-package' works is still a mystery in many ways.
;;
;;
;; NOTICE
;;
;; Unlike `eval-after-load', currently `eval-before-load' works only for
;; feature, that means, the first argument FEATURE of `eval-before-load' must be
;; a feature, i.e., a symbol, such as 'helm-config.
;;
;; As you can see, `eval-before-load' is better to renamed as
;; `eval-before-require', but for its expansibility and I like shorter name,
;; I didn't do it.
;;
;; To learn what a feature is, see (info "(elisp) Named Features")
;;
;;
;; TODO
;; ====
;;
;; 1. Find out some practical use cases, if any

;;; Code:
(require 'cl-lib)

;;*---------------------------------------------------------------------*/
;;*    Internal                                                         */
;;*---------------------------------------------------------------------*/
(defvar before-load-alist nil)

(defun require--eval-before-load (orig-fun &rest args)
  "Evaluate something right before `require'."
  (let ((feature (car args)))
    (when (not (featurep feature))
      (let ((func-list (assoc-default feature before-load-alist)))
        ;; Warning Error in `*Warning*' buffer if any
        (condition-case err
            (mapc #'funcall func-list)
          (error
           (ignore
            (display-warning 'eval-before-load
                             (format "%s: %s"
                                     feature (error-message-string err))
                             :error)))))))
  (apply orig-fun args))

(advice-add 'require :around #'require--eval-before-load)

;;;###autoload
(defun eval-before-load (feature form)
  "Arrange that if FEATURE'll be loaded, FROM'll be run immediately beforehand.
If FEATURE is already loaded, evaluate FORM right now.
FEATURE is a symbol like 'helm-config."
  ;; Add this FORM into before-load-alist (regardless of whether we'll be
  ;; evaluating it now).
  (let* ((feature (if (stringp feature) (intern feature)
                    feature))
         (elt (assoc feature before-load-alist))
         (func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding))))
    (unless elt
      (setq elt (list feature))
      (push elt before-load-alist))
    ;; Is feature already loaded?
    (prog1 (if (featurep feature)
               (funcall func))
      (unless (member func (cdr elt))
        (nconc elt (list func))))))
(put 'eval-before-load 'lisp-indent-function 1)

;;;###autoload
(defmacro with-eval-before-load (feature &rest body)
  "Execute BODY right before FEATURE will be loaded."
  (declare (indent 1))
  `(eval-before-load ,feature (lambda () ,@body)))

;;*---------------------------------------------------------------------*/
;;*    Utility functions                                                */
;;*---------------------------------------------------------------------*/
(defun eval-before-load--append-nth (list element n)
  "Return a new list by appending ELEMENT at Nth of LIST."
  (if (<= n 0)
      (cons element list)
    (nconc (cl-subseq list 0 n)
           (cons element (nthcdr n list)))))

;;*---------------------------------------------------------------------*/
;;*    `use-package' integration                                        */
;;*---------------------------------------------------------------------*/
(require 'use-package nil t)

(when (featurep 'use-package)
  ;; First step: Add the keyword
  (unless (memq :pre-config use-package-keywords)
    (setq use-package-keywords
          (eval-before-load--append-nth
           use-package-keywords
           :pre-config (or (cl-position :config use-package-keywords)
                           (length use-package-keywords)))))
  ;; Second step: Create a normalizer
  (defalias 'use-package-normalize/:pre-config 'use-package-normalize-forms))

;; Third step: Create a handler
(defun use-package-handler/:pre-config (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     `((eval-before-load ',name
         ,@arg))
     body)))

(provide 'eval-before-load)
;;; eval-before-load.el ends here
