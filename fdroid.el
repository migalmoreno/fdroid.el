;; -*- lexical-binding: t; -*-
(require 'consult)
(require 'embark)

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
  "Selects whether to log the exectuion of events."
  :group 'fdroid
  :type 'boolean)

(cl-defmacro with--fdroidcl (commands &body body)
  "Executes `fdroidcl' with COMMANDS and performs operations set by BODY."
  `(with-temp-buffer
     (apply #'call-process fdroid-program nil t nil ,commands)
     (goto-char (point-min))
     ,@body))

(cl-defun fdroid--list-packages (&optional keywords)
  "Lists all packages in current F-Droid repository. Optionally, filter packages by KEYWORDS
 and returns a list of matching results."
  (with--fdroidcl
   (if keywords
       `("search" ,keywords)
     '("search"))
    (let ((results (make-hash-table :test 'equal)))
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
      results)))

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
           in (fdroid--list-packages (or keywords)) using (hash-value v)
           collect (fdroid--format-package k v completion-hash))
    completion-hash))

(cl-defun fdroid--prompt-completion (&key (multiple nil) (keywords nil))
  "Prompts the user for a package from the full list of packages, or
from the filtered list drawn from KEYWORDS if provided. If specified, prompt the user
for a MULTIPLE package selection."
  (interactive)
  (let ((candidates (if keywords
                        (fdroid--build-candidate-list)
                      (fdroid--build-candidate-list keywords))))
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
  "Updates current F-Droid repository package index."
  (interactive)
  (with--fdroidcl
   '("update")
   (when fdroid-log-events
     (message "Repository updated!"))))

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
  (with--fdroidcl
   `("install" ,package)
   (when fdroid-log-events
     (message "Package \"%s\" successfully installed on device." package))))

;;;###autoload
(defun fdroid-install-multiple (packages)
  "Installs or upgrades multiple PACKAGES on the device."
  (interactive
   (list (mapcar (lambda (e)
                   (gethash e (fdroid--build-candidate-list)))
                 (fdroid--prompt-completion :multiple t))))
  (with--fdroidcl
   `("install" ,@packages)
   (when fdroid-log-events
     (message "Packages \"%s\" successfully installed on device." (mapconcat #'identity packages " ")))))

;;;###autoload
(defun fdroid-uninstall (package)
  "Uninstalls PACKAGE from device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (with--fdroidcl
   `("uninstall" ,package)
   (when fdroid-log-events
     (message "Package \"%s\" successfully uninstalled from device." package))))

;;;###autoload
(defun fdroid-download (package)
  "Downloads PACKAGE to the device."
  (interactive
   (list (gethash (fdroid--prompt-completion) (fdroid--build-candidate-list))))
  (with--fdroidcl
   `("download" ,package)
   (when fdroid-log-events
     (message "Package \"%s\" successfully downloaded to device." package))))

;;;###autoload
(defun fdroid-show (package)
  "Shows detailed information about PACKAGE."
  (interactive))

(embark-define-keymap embark-fdroid-actions
  "Keymap for `fdroidcl' actions which take F-Droid package identifiers."
  ("i" fdroid-install)
  ("d" fdroid-download)
  ("u" fdroid-uninstall)
  ("s" fdroid-show))

(add-to-list 'embark-keymap-alist '(fdroid . embark-fdroid-actions))

(provide 'fdroid)
