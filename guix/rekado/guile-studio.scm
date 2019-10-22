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
             (gnu packages gnome))


;;; XXX big bad hack...
(system* "tar" "-cf" "/tmp/guile-studio.tar"
         "--exclude-vcs" "--exclude" "guix"
         "-C" "/home/rekado/dev/" "guile-studio")

(define-public guile-studio
  (package
    (name "guile-studio-devel")
    (version "0")
    (source "/tmp/guile-studio.tar")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key source inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin/"))
                    (share (string-append out "/share/")))
               (mkdir-p share)
               (mkdir-p bin)
               (apply invoke "guile" "-s"
                      "guile-studio-configure.scm"
                      out
                      (assoc-ref inputs "emacs")
                      (assoc-ref inputs "guile-picture-language")
                      (string-append (assoc-ref inputs "adwaita-icon-theme")
                                     "/share/icons/Adwaita/")
                      (map cdr inputs))
               #t)))
         (delete 'install))))
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-picture-language" ,guile-picture-language)
       ("emacs" ,emacs)
       ("emacs-geiser" ,emacs-geiser)
       ("emacs-company" ,emacs-company)
       ("emacs-flycheck" ,emacs-flycheck)
       ("emacs-smart-mode-line" ,emacs-smart-mode-line)
       ("emacs-paren-face" ,emacs-paren-face)
       ("adwaita-icon-theme" ,adwaita-icon-theme)))
    (home-page "https://gnu.org/software/guile")
    (synopsis "Totally not a cheap copy of Dr Racket for Guile")
    (description
     "Racket has Dr Racket.  Guile has ... Emacs?  This is Emacs with a few
settings that make working with Guile easier for people new to Emacs.
Features include: CUA mode, Geiser, tool bar icons to evaluate Guile buffers,
support for Guile's very own picture language, code completion, a simple mode
line, etc.")
    (license license:gpl3+)))
