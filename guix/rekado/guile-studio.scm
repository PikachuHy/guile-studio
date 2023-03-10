(define-module (rekado guile-studio))
(use-modules (guix packages)
             (guix download)
             (guix gexp)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages gnome)
             (gnu packages texinfo))

(define-public guile-studio
  (package
    (name "guile-studio-devel")
    (version "0.1.0")
    (source (local-file (string-append "guile-studio-" version ".tar.gz")))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (srfi srfi-1)
        ,@%gnu-build-system-modules)
       #:tests? #f                      ; there are none
       #:make-flags
       (list (string-append "PICT_DIR="
                            (assoc-ref %build-inputs "guile-picture-language"))
             (string-append "EMACS_DIR="
                            (assoc-ref %build-inputs "emacs"))
             (string-append "GUILE_DIR="
                            (assoc-ref %build-inputs "guile"))
             (string-join (cons "INPUTS="
                                (filter-map
                                 (lambda (input)
                                   (match input
                                     ((label . pkg)
                                      (and (string-prefix? "emacs" label) pkg))))
                                 %build-inputs)))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'install))))
    (inputs
     `(("guile" ,guile-3.0)
       ("guile-picture-language" ,guile-picture-language)
       ("emacs" ,emacs)
       ("emacs-f" ,emacs-f) ; TODO: needed by doom-modeline
       ("emacs-memoize" ,emacs-memoize) ; TODO: needed by all-the-icons
       ("emacs-all-the-icons" ,emacs-all-the-icons)
       ("emacs-all-the-icons-dired" ,emacs-all-the-icons-dired)
       ("emacs-dired-sidebar" ,emacs-dired-sidebar)
       ("emacs-doom-modeline" ,emacs-doom-modeline)
       ("emacs-modus-themes" ,emacs-modus-themes)
       ("emacs-geiser" ,emacs-geiser)
       ("emacs-company" ,emacs-company)
       ("emacs-ivy" ,emacs-ivy)
       ("emacs-flycheck" ,emacs-flycheck)
       ("emacs-flycheck-guile" ,emacs-flycheck-guile)
       ("emacs-paren-face" ,emacs-paren-face)))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (home-page "https://gnu.org/software/guile")
    (synopsis "Totally not a cheap copy of Dr Racket for Guile")
    (description
     "Racket has Dr Racket.  Guile has ... Emacs?  This is Emacs with a few
settings that make working with Guile easier for people new to Emacs.
Features include: CUA mode, Geiser, tool bar icons to evaluate Guile buffers,
support for Guile's very own picture language, code completion, a simple mode
line, etc.")
    (license license:gpl3+)))
