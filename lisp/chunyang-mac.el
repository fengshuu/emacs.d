;;; chunyang-mac.el --- macOS Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;; Some macOS supports

;;; Code:


;;; Terminal.app

(defun chunyang-mac-escape-quote (s)
  "Convert \" in S into \\\"."
  (replace-regexp-in-string "\"" "\\\\\"" s))

(defun chunyang-mac-Terminal-send-string (s)
  "Run STR in Terminal.app."
  (do-applescript
   (format (concat
            "tell application \"Terminal\"\n"
            "activate\n"
            "do script \"%s\" in window 1\n"
            "end tell")
           (chunyang-mac-escape-quote s))))

(defun chunyang-mac-Terminal-send-region (start end)
  "Send the current region to Terminal.app."
  (interactive "r")
  (chunyang-mac-Terminal-send-string (buffer-substring start end)))

(defun chunyang-mac-Terminal-cd (dir)
  "Open Terminal.app and cd to DIR in it."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  (chunyang-mac-Terminal-send-string (format "cd '%s'" dir)))


;;; iTerm.app

(defun chunyang-mac-iTerm-send-string (string)
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    tell current session of current window\n"
    "        write text \"" string "\"\n"
    "        end tell\n"
    "end tell")))

(defun chunyang-mac-iTerm-send-region (start end)
  "Send the current region to iTerm.app."
  (interactive "r")
  (chunyang-mac-iTerm-send-string (buffer-substring start end)))

;;;###autoload
(defun chunyang-mac-iTerm-cd (dir)
  "Switch to iTerm and change directory there to DIR."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  ;; NOTE `shell-quote-argument' doesn't work with `do-applescript'
  (let ((cmd (format "cd '%s'" dir)))
    (chunyang-mac-iTerm-send-string cmd)))


;;; Finder.app

;; IDEA: Reveal multiple files
(defun chunyang-mac-Finder-reveal (file)
  "Reveal (select/highlight) FILE in Finder."
  (interactive (list (or (buffer-file-name) ".")))
  ;; FIXME: It is better and easier to use 'open -R'
  (do-applescript
   (format (concat
            "tell application \"Finder\"\n"
            "	activate\n"
            "	reveal POSIX file \"%s\"\n"
            "end tell")
           (expand-file-name file))))


;;; Tags

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defun chunyang-mac-edit-file-tags (file)
  "Edit the macOS file Tags of FILE.

For usage of the macOS file Tags, see 
URL `https://support.apple.com/kb/PH25325?locale=en_US'."
  (interactive
   (let* ((default (cond ((eq major-mode 'dired-mode)
                          (dired-get-filename nil t))
                         (t (thing-at-point 'filename))))
          (prompt (if default
                      (format "File (default %s): " default)
                    "File: "))
          (file (read-file-name prompt nil default)))
     (list file)))
  (unless (eq system-type 'darwin)
    (user-error "The OS '%s' is not supported" system-type))
  (unless (executable-find "tag")
    (user-error "The program 'tag' is not found"))
  (let* ((old-tags
          (with-temp-buffer
            (call-process "tag" nil t nil "--no-name" file)
            (goto-char (point-min))
            (buffer-substring (line-beginning-position) (line-end-position))))
         (new-tags
          (read-string
           (format "Set Tags of '%s': "
                   (abbreviate-file-name file))
           old-tags)))
    (call-process "tag" nil nil nil "--set" new-tags file)))

(defun chunyang-mac-all-tags ()
  (require 'seq)
  (seq-uniq
   (split-string
    (shell-command-to-string
     ;; the short version: tag -f '*' -t -g -N -0
     "tag --find '*' --tags --garrulous --no-name --nul")
    "\0" t)))

(defun chunyang-mac-search-tags (tags)
  (interactive
   (list (completing-read-multiple "Tags: " (chunyang-mac-all-tags))))
  (shell-command (concat "tag --find " (mapconcat #'identity tags ","))))

(require 'helm)
(require 'helm-utils)                   ; `helm-open-dired'

(defun helm-chunyang-mac-tags (tags)
  (interactive
   (list
    (helm-comp-read
     "Tags: "
     (chunyang-mac-all-tags)
     :must-match t
     :marked-candidates t
     :fc-transformer 'helm-adaptive-sort
     :buffer "*helm mac tags*")))
  (helm :sources
        (helm-build-in-buffer-source (concat "File with tag "
                                             (mapconcat #'identity tags ","))
          :data (split-string
                 (shell-command-to-string
                  (concat "tag --nul --find " (mapconcat #'identity tags ",")))
                 "\0" t)
          :action (helm-make-actions
                   "Open" (lambda (file)
                            (shell-command (concat "open " (shell-quote-argument file))))
                   "Reveal with Finder" #'chunyang-mac-Finder-reveal
                   "Reveal with Dired" #'helm-open-dired))
        :buffer "*helm find file by tags*"))


;;; Google Chrome

(defun chunyang-chrome-refresh ()
  "Refresh the current tab of Chrome."
  (do-applescript
   "tell application \"Chrome\" \
to tell the active tab of its first window to reload"))

(defun chunyang-chrome-url ()
  "Return the URL of the current tab of Chrome."
  (replace-regexp-in-string
   (rx (or (and string-start ?\")
           (and ?\" string-end)))
   ""
   (do-applescript
    "tell application \"Google Chrome\" to return URL of active tab of first window")))


;;; Misc

;; [[https://stackoverflow.com/questions/16064957/how-to-check-in-applescript-if-an-app-is-running-without-launching-it-via-osa][osx - How to check in AppleScript if an app is running, without launching it - via osascript utility - Stack Overflow]]
(defun chunyang-mac-app-running-p (app)
  "Return non-nil if APP is running."
  (do-applescript
   (format (concat "if application \"%s\" is running then\n"
                   "	return \"Running!\"\n"
                   "end if")
           app)))

;; https://emacs-china.org/t/topic/5393
(defun chunyang-mac-switch-to-app (app)
  (interactive
   (let (apps)
     (with-temp-buffer
       (call-process "osascript" nil t nil "-e"
                     (concat
                      "tell application \"Finder\"\n"
                      "  get the name of every process whose visible is true\n"
                      "end tell"))
       (setq apps (split-string (buffer-string) "[,\n]" t " ")))
     (list (completing-read "Switch to Application: " apps))))
  (do-applescript (format "tell application \"%s\" to activate" app)))

;; https://emacs.stackexchange.com/questions/40829/cannot-open-new-chrome-tab-by-browse-url-chrome-newtab
;; chrome://about/
(defun chunyang-browse-url-mac-chrome (url &optional _new-window)
  "Browse URL in Chrome.

Chrome protocol URL such as chrome://newtab is supported,
unlike `browse-url-default-macosx-browser'."
  (interactive (browse-url-interactive-arg "URL: "))
  (do-applescript
   (mapconcat
    #'identity
    ;; https://apple.stackexchange.com/a/271709/132365
    `("set myLink to \"" ,url "\""
      "tell application \"Google Chrome\""
      "    activate"
      "    tell front window to make new tab at after (get active tab) with properties {URL:myLink}"
      "end tell")
    "\n")))


;; https://emacs-china.org/t/topic/5518
(defun chunyang-chrome-tabs ()
  "返回 Chrome 标签."
  (let ((script
         (mapconcat
          #'identity
          '("set titleString to return"
            ""
            "tell application \"Google Chrome\""
            "  set window_list to every window"
            "  set window_counter to 0"
            ""
            "  repeat with the_window in window_list"
            "    set window_counter to window_counter + 1"
            "    set tab_list to every tab in the_window"
            "    set tab_counter to 0"
            ""
            "    repeat with the_tab in tab_list"
            "      set tab_counter to tab_counter + 1"
            "      set coordinate to window_counter & \" \" & tab_counter"
            "      set the_title to the title of the_tab"
            "      set titleString to titleString & coordinate & \" \" & the_title & return"
            "    end repeat"
            "  end repeat"
            "end tell")
          "\n")))
    (thread-first script
      (do-applescript)
      (string-trim "\"\n" "\n\"")
      (split-string "\n"))))

;; (chunyang-chrome-tabs)
;; => ("1 1 Google" "1 2 Home - BBC News")
;; 1 - 第一个窗口
;; 1 - 第一个标签
;; Google - 标题

(defun chunyang-chrome-switch-tab-1 (window-id tab-id)
  ;; FIXME 不知道如何处理多余一个窗口的情况
  (do-applescript
   (concat "tell application \"Google Chrome\"\n"
           (format "  set active tab index of first window to %s\n" tab-id)
           "  activate\n"
           "end tell\n")))

(defun chunyang-chrome-switch-tab (window-id tab-id)
  (interactive
   (let* ((tabs (chunyang-chrome-tabs))
          (input
           (completing-read
            "Open Chrome Tab: "
            (mapcar
             (lambda (s)
               (and (string-match
                     (rx string-start
                         (1+ num) " " (1+ num) " "
                         (group (1+ not-newline))
                         string-end)
                     s)
                    (match-string 1 s)))
             tabs)
            nil t)))
     (seq-some (lambda (s)
                 (and (string-match
                       (rx-to-string
                        `(and
                          string-start
                          (group (1+ num)) " " (group (1+ num)) " "
                          ,input
                          string-end))
                       s)
                      (list (match-string 1 s)
                            (match-string 2 s))))
               tabs)))
  (chunyang-chrome-switch-tab-1 window-id tab-id))


;; https://matthewbilyeu.com/blog/2018-04-09/setting-emacs-theme-based-on-ambient-light
;; https://emacs-china.org/t/topic/5536
(defun auto-theme-mode-filter (_proc output)
  (let ((current-light-sensor-reading (string-to-number output))
        (current-theme (car custom-enabled-themes))
        (dark-theme 'sanityinc-tomorrow-eighties)
        (light-theme 'sanityinc-tomorrow-day))
    (cond ((/= (length output) 8))      ; printf("%8lld", values[0]);
          ((and (< current-light-sensor-reading 100000)
                (not (eq current-theme dark-theme)))
           (disable-theme current-theme)
           (enable-theme dark-theme))
          ((and (>= current-light-sensor-reading 100000)
                (not (eq current-theme light-theme)))
           (disable-theme current-theme)
           (enable-theme light-theme)))))

(define-minor-mode auto-theme-mode
  "Automatically set Emacs theme based on ambient light."
  :global t
  (let* ((buf " *auto-theme-mode*")
         (proc (get-buffer-process buf)))
    (if auto-theme-mode
        (or (and proc (eq 'run (process-status proc)))
            (let ((process-connection-type nil))
              (set-process-filter
               (start-process
                "lmutracker"
                buf
                "/bin/sh"
                "-c"
                "while true; do lmutracker && sleep 1; done")
               #'auto-theme-mode-filter)))
      (and proc (kill-process proc)))))

(provide 'chunyang-mac)
;;; chunyang-mac.el ends here
