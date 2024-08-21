(use-modules (gnu packages android)
             (gnu packages emacs-xyz)
             (guix build utils)
             (guix build-system emacs)
             (guix gexp)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (ice-9 popen)
             (ice-9 rdelim))

(define %source-dir (dirname (current-filename)))

(define (emacs-fdroid-git-version)
  (let* ((port (with-directory-excursion
                %source-dir
                (open-input-pipe "git describe --always --tags")))
         (version (read-line port)))
    (close-pipe port)
    version))

(define-public emacs-fdroid
  (package
    (name "emacs-fdroid")
    (version (emacs-fdroid-git-version))
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system emacs-build-system)
    (inputs
     (list fdroidcl))
    (propagated-inputs
     (list emacs-embark))
    (home-page "https://git.migalmoreno.com/fdroid.el")
    (synopsis "An Emacs interface to manage F-Droid repositories")
    (description "fdroid.el is an Emacs interface to fdroidcl.  Its purpose
is to aid the management of F-Droid packages from the comfort of Emacs.")
    (license license:gpl3+)))

emacs-fdroid
