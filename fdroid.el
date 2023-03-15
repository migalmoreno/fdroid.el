;;; fdroid.el --- An Emacs interface to F-Droid  -*- lexical-binding: t; -*-

;; Copyright © 2022, 2023 conses <contact@conses.eu>

;; Author: conses <contact@conses.eu>
;; Version: 0.1.0
;; Keywords: tools, processes
;; URL: https://git.sr.ht/~conses/fdroid.el
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a minibuffer-based interface to F-Droid.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup fdroid nil
  "Manage F-Droid packages through `fdroidcl'."
  :group 'external
  :prefix "fdroid-"
  :tag "F-Droid")

(defcustom fdroid-program (executable-find "fdroidcl")
  "The executable path for `fdroidcl'."
  :group 'froid
  :type 'string)

(defcustom fdroid-log-events nil
  "Select whether to log the execution of `fdroid-program' events."
  :group 'fdroid
  :type 'boolean)

(defcustom fdroid-package-format "%a [%v]: %d"
  "The format for each entry in the list of F-Droid packages.

The following %-escapes will be expanded using `format-spec':

%n      The package name.
%v      The package version.
%a      The package appid.
%d      The package description."
  :type 'string
  :group 'fdroid)

(defcustom fdroid-sans-device nil
  "If non-nil, `fdroid' commands should override the device connection check."
  :group 'fdroid
  :type 'boolean)

(defvar fdroid-packages nil
  "The list of cached packages from the current F-Droid repository.")

(defvar fdroid-map nil
  "Keymap to bind `fdroid' commands to.")

(defvar fdroid-minibuffer-actions
  (let ((map (make-sparse-keymap)))
    (define-key map [?i] #'fdroid-install)
    (define-key map [?d] #'fdroid-download)
    (define-key map [?u] #'fdroid-uninstall)
    (define-key map [?s] #'fdroid-show)
    map)
  "Keymap for `fdroid' mini-buffer actions.")

(defvar fdroid-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q] #'kill-this-buffer)
    map)
  "Keymap for *fdroid-show* buffers.")

(cl-defmacro fdroid-with--fdroidcl (commands message &body body)
  "Execute `fdroid-program' with COMMANDS.
Then, run BODY in the context of the result, and show MESSAGE after completion."
  `(with-current-buffer (get-buffer-create "*fdroid-output*")
     (erase-buffer)
     (call-process fdroid-program nil t nil "devices")
     (goto-char (point-min))
     (if (or (re-search-forward
              (rx bol (+ alphanumeric) " - " (+ any)) (pos-eol) t)
             fdroid-sans-device)
         (make-process
          :name "fdroid.el"
          :buffer (current-buffer)
          :command (append (list fdroid-program)
                           ,commands)
          :sentinel (lambda (p _e)
                      (cond
                       ((and (= (process-exit-status p) 0)
                             fdroid-log-events
                             ,message)
                        (cl-typecase ,message
                          (cons (apply #'message ,message))
                          (t (message ,message))))
                       ((= (process-exit-status p) 0)
                        (with-current-buffer (process-buffer p)
                          ,@body
                          (kill-buffer (process-buffer p)))))))
       (when (and fdroid-log-events ,message)
         (message "Launching fdroidcl..."))
       (user-error "No device connected"))))

(defun fdroid--list-packages (&optional keywords)
  "List all packages in current F-Droid repository.
Optionally, filter packages by KEYWORDS and return a list of matching results."
  (let ((command (if keywords (list "search" keywords) (list "search")))
        (results (make-hash-table :test 'equal)))
    (or fdroid-packages
        (with-temp-buffer
          (apply #'call-process fdroid-program nil t nil command)
          (goto-char (point-min))
          (while (not (eobp))
            (when (re-search-forward (rx bol (group (+ (or alpha punct)))
                                         (+ blank)
                                         (or "- " (group (* anychar)))
                                         " - " (group (+ any))
                                         "\n" (+ blank) (group (+ any)))
                                     (pos-eol 2) t)
              (puthash (match-string 1) (list
                                         :name (match-string 2)
                                         :version (match-string 3)
                                         :description (match-string 4))
                       results))
            (forward-line 1))
          (setq fdroid-packages results)))))

(defun fdroid--format-package (key value table)
  "Embellish package entry with KEY and VALUE from TABLE for user completion."
  (let* ((name (cl-getf value :name))
         (version (cl-getf value :version))
         (description (cl-getf value :description))
         (spec `((?n . ,name)
                 (?v . ,version)
                 (?a . ,key)
                 (?d . ,description))))
    (puthash (format-spec fdroid-package-format spec) key table)))

(defun fdroid--build-candidate-list (&optional keywords)
  "Build candidate list with KEYWORDS to be leveraged on completion functions."
  (let ((completion-hash (make-hash-table :test 'equal)))
    (cl-loop for k being the hash-keys
             in (if keywords
                    (fdroid--list-packages keywords)
                  (fdroid--list-packages))
             using (hash-value v)
           collect (fdroid--format-package k v completion-hash))
    completion-hash))

(cl-defun fdroid--prompt-completion (&key (multiple nil) (keywords nil))
  "Prompt the user for a package from the full list of packages.
Optionally, prompt from the filtered list drawn from KEYWORDS if provided.
If specified, prompt the user for MULTIPLE package selection."
  (interactive)
  (let ((candidates (if keywords
                        (fdroid--build-candidate-list keywords)
                      (fdroid--build-candidate-list))))
    (if multiple
        (completing-read-multiple
         "Package(s): "
         candidates)
      (completing-read
       "Package: "
       (lambda (string pred action)
         (if (eq action 'metadata)
             `(metadata
               (category . fdroid)
               (display-sort-function . ,#'identity))
           (complete-with-action action candidates string pred)))))))

;;;###autoload
(defun fdroid-list-packages ()
  "Main point of entry to the user facing interactive commands."
  (interactive)
  (fdroid--prompt-completion))

;;;###autoload
(defun fdroid-update ()
  "Clear and update current F-Droid repository package index."
  (interactive)
  (fdroid-with--fdroidcl
   (list "update")
   "Repositories updated"))
  (setq fdroid-packages nil)

;;;###autoload
(defun fdroid-search (keywords)
  "Search for packages with KEYWORDS in current F-Droid repository."
  (interactive "sKeywords: ")
  (fdroid--prompt-completion keywords))

;;;###autoload
(defun fdroid-install (package)
  "Install or upgrade a single PACKAGE on the device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "install" package)
   `("Package \"%s\" successfully installed on device" ,package)))

;;;###autoload
(defun fdroid-install-multiple (packages)
  "Install or upgrade multiple PACKAGES on the device."
  (interactive
   (list (mapcar (lambda (e)
                   (gethash e (fdroid--build-candidate-list)))
                 (fdroid--prompt-completion :multiple t))))
  (let ((packages (mapconcat #'identity packages " ")))
    (fdroid-with--fdroidcl
     `("install" ,@(split-string packages))
     `("Packages \"%s\" successfully installed on device" ,packages))))

;;;###autoload
(defun fdroid-uninstall (package)
  "Uninstall PACKAGE from device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "uninstall" package)
   `("Package \"%s\" successfully uninstalled from device" ,package)))

;;;###autoload
(defun fdroid-download (package)
  "Download PACKAGE to the device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "download" package)
   `("Package \"%s\" successfully downloaded to device" ,package)))

;;;###autoload
(defun fdroid-show (package)
  "Show detailed information about PACKAGE."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "show" package)
   nil
   (let ((result (buffer-substring (point-min) (point-max))))
     (switch-to-buffer
      (with-current-buffer (get-buffer-create "*fdroid-show*")
        (fdroid-output-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert result))
        (current-buffer))))))


(define-derived-mode fdroid-output-mode special-mode "F-Droid Output"
  "Major mode for *fdroid-show* buffers.")

(when (require 'embark nil t)
  (add-to-list 'embark-keymap-alist '(fdroid . fdroid-minibuffer-actions)))

(define-prefix-command 'fdroid-map)
(define-key fdroid-map [?l] #'fdroid-list-packages)
(define-key fdroid-map [?i] #'fdroid-install)
(define-key fdroid-map [?d] #'fdroid-download)
(define-key fdroid-map [?u] #'fdroid-uninstall)
(define-key fdroid-map [?s] #'fdroid-show)
(define-key fdroid-map [?I] #'fdroid-install-multiple)

(provide 'fdroid)
;;; fdroid.el ends here
