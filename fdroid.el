;; -*- lexical-binding: t; -*-
(require 'consult)
(require 'embark)
(require 'cl-lib)

(defgroup fdroid nil
  "Manage F-Droid packages through `fdroidcl'."
  :group 'external
  :prefix "fdroid-"
  :tag "F-Droid")

(defcustom fdroid-program (executable-find "fdroidcl")
  "Holds the executable path for `fdroidcl'."
  :group 'froid
  :type 'string)

(defcustom fdroid-log-events nil
  "Selects whether to log the execution of `fdroid-program' events."
  :group 'fdroid
  :type 'boolean)

(defvar fdroid--packages nil
  "Holds the list of cached packages from the current F-Droid repository.")


(defvar fdroid-map nil
  "Map to bind `fdroid' commands to.")

(cl-defmacro fdroid-with--fdroidcl (commands message &body body)
  "Executes `fdroid-program' with COMMANDS, runs BODY in the context of the result,
and shows MESSAGE after completion."
  `(with-current-buffer (get-buffer-create "*fdroid-output*")
     (erase-buffer)
     (call-process fdroid-program nil t nil "devices")
     (goto-char (point-min))
     (if (re-search-forward (rx (: bol (+ alphanumeric) " - " (+ any))) (point-at-eol) t)
         (let ((process (make-process
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
                                         (kill-buffer (process-buffer p)))))))))
           (when (and fdroid-log-events ,message)
             (message "Launching fdroidcl...")))
       (user-error "No device connected."))))

(defun fdroid--list-packages (&optional keywords)
  "Lists all packages in current F-Droid repository. Optionally, filter packages by KEYWORDS
 and returns a list of matching results."
  (let ((command (if keywords (list "search" keywords) (list "search")))
        (results (make-hash-table :test 'equal)))
    (or fdroid--packages
        (with-temp-buffer
          (apply #'call-process fdroid-program nil t nil command)
          (goto-char (point-min))
          (while (not (eobp))
            (when (re-search-forward (rx (: bol (group (+ (or alpha punct)))
                                            (+ blank)
                                            (or "- " (group (* anychar)))
                                            " - " (group (+ any))
                                            "\n" (+ blank) (group (+ any))))
                                     (point-at-eol 2) t)
              (puthash (match-string 1) (list
                                         :name (match-string 2)
                                         :version (match-string 3)
                                         :description (match-string 4))
                       results))
            (forward-line 1))
          (setf fdroid--packages results)))))

(defun fdroid--format-package (key value table)
  "Embellishes package entry with KEY and VALUE from TABLE for user completion."
  (let ((name (cl-getf value :name))
        (version (cl-getf value :version))
        (description (cl-getf value :description)))
    (if (not (string-empty-p name))
        (puthash (format "%s - %s (%s): %s" name version key description)
                 key table)
      (puthash (format "%s - %s: %s" key version description)
               key table))))

(defun fdroid--build-candidate-list (&optional keywords)
  "Builds candidate list with KEYWORDS to be leveraged on completion functions."
  (let ((completion-hash (make-hash-table :test 'equal)))
    (cl-loop for k being the hash-keys
             in (if keywords
                    (fdroid--list-packages keywords)
                  (fdroid--list-packages))
             using (hash-value v)
           collect (fdroid--format-package k v completion-hash))
    completion-hash))

(cl-defun fdroid--prompt-completion (&key (multiple nil) (keywords nil))
  "Prompts the user for a package from the full list of packages, or
from the filtered list drawn from KEYWORDS if provided. If specified, prompt the user
for a MULTIPLE package selection."
  (interactive)
  (let ((candidates (if keywords
                        (fdroid--build-candidate-list keywords)
                      (fdroid--build-candidate-list))))
    (if multiple
        (consult-completing-read-multiple
         "Package(s): "
         candidates)
      (consult--read
       candidates
       :prompt "Package: "
       :sort nil
       :category 'fdroid))))

;;;###autoload
(defun fdroid-list-packages ()
  "Main point of entry to the user facing interactive commands."
  (interactive)
  (fdroid--prompt-completion))

;;;###autoload
(defun fdroid-update ()
  "Clears and updates current F-Droid repository package index."
  (interactive)
  (setf fdroid--packages nil)
  (fdroid-with--fdroidcl
   (list "update")
   "Repositories updated."))

;;;###autoload
(defun fdroid-search (keywords)
  "Searches for packages with KEYWORDS in current F-Droid repository."
  (interactive "sKeywords: ")
  (fdroid--prompt-completion keywords))

;;;###autoload
(defun fdroid-install (package)
  "Installs or upgrades a single PACKAGE on the device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "install" package)
   `("Package \"%s\" successfully installed on device." ,package)))

;;;###autoload
(defun fdroid-install-multiple (packages)
  "Installs or upgrades multiple PACKAGES on the device."
  (interactive
   (list (mapcar (lambda (e)
                   (gethash e (fdroid--build-candidate-list)))
                 (fdroid--prompt-completion :multiple t))))
  (let ((packages (mapconcat #'identity packages " ")))
    (fdroid-with--fdroidcl
     `("install" ,@(split-string packages))
     `("Packages \"%s\" successfully installed on device." ,packages))))

;;;###autoload
(defun fdroid-uninstall (package)
  "Uninstalls PACKAGE from device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "uninstall" package)
   `("Package \"%s\" successfully uninstalled from device." ,package)))

;;;###autoload
(defun fdroid-download (package)
  "Downloads PACKAGE to the device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "download" package)
   `("Package \"%s\" successfully downloaded to device." ,package)))

;;;###autoload
(defun fdroid-show (package)
  "Shows detailed information about PACKAGE."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (fdroid-with--fdroidcl
   (list "show" package)
   nil
   (let ((result (buffer-substring (point-min) (point-max))))
     (switch-to-buffer
      (with-current-buffer (get-buffer-create "*fdroid-show*")
        (insert result)
        (current-buffer))))))

(embark-define-keymap embark-fdroid-actions
  "Keymap for `fdroid' actions which take F-Droid package identifiers."
  ("i" fdroid-install)
  ("d" fdroid-download)
  ("u" fdroid-uninstall)
  ("s" fdroid-show))

(define-prefix-command 'fdroid-map)
(define-key fdroid-map [?l] #'fdroid-list-packages)
(define-key fdroid-map [?i] #'fdroid-install)
(define-key fdroid-map [?d] #'fdroid-download)
(define-key fdroid-map [?u] #'fdroid-uninstall)
(define-key fdroid-map [?s] #'fdroid-show)
(define-key fdroid-map [?I] #'fdroid-install-multiple)

(add-to-list 'embark-keymap-alist '(fdroid . embark-fdroid-actions))

(provide 'fdroid)
