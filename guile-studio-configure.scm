(use-modules (ice-9 match)
             (ice-9 ftw)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (srfi srfi-1)
             (srfi srfi-26))

(define (emacs-package-directory root)
  (define (directory? thing)
    (eq? 'directory (stat:type (stat (string-append root "/" thing)))))
  (match (scandir root directory?)
    (("." ".." subdir . rest)
     (string-append root "/" subdir))
    (_ #false)))

(define (make-guile-studio-wrapper prefix share emacsdir emacs-package-dirs)
  (let ((wrapper (string-append prefix "/bin/guile-studio")))
    (with-output-to-file wrapper
      (lambda ()
        (format #t "#!/bin/sh
EMACSLOADPATH=~a:
exec ~a/bin/emacs -mm --no-site-file --no-site-lisp --no-x-resources --no-init-file --load ~a/guile-studio.el
"
                (string-join
                 (filter-map (lambda (dir)
                               (emacs-package-directory
                                (string-append dir "/share/emacs/site-lisp")))
                             emacs-package-dirs) ":")
                emacsdir share)))
    (chmod wrapper #o555)))

(define* (dump-port in out #:key (buffer-size 16384))
  (define buffer
    (make-bytevector buffer-size))

  (define (loop bytes)
    (or (eof-object? bytes)
        (begin
          (put-bytevector out buffer 0 bytes)
          (loop (get-bytevector-n! in buffer 0 buffer-size)))))

  (loop (get-bytevector-n! in buffer 0 buffer-size)))

(define (main)
  (define (info-file? file)
    (or (string-suffix? ".info" file)
        (string-suffix? ".info.gz" file)))
  (define (info-files top)
    (let ((infodir (string-append top "/share/info")))
      (map (cut string-append infodir "/" <>)
           (or (scandir infodir info-file?) '()))))
  (match (command-line)
    ((_ prefix emacsdir guiledir picture-language . emacs-package-dirs)
     (let* ((share (string-append prefix "/share"))
            (datadir (string-append share "/guile-studio"))
            (infodir (string-append datadir "/info")))
       ;; Generate Info directory
       (mkdir datadir)
       (mkdir infodir)
       (for-each
        (lambda (info)
          (system* "install-info" "--debug" info
                   (string-append infodir "/dir")))
        (append-map info-files (list picture-language guiledir)))

       ;; Generate Emacs startup file
       (call-with-output-file (string-append share "/guile-studio.el")
         (lambda (out)
           ;; Generate global variables at the top.
           (write `(defvar %guile-studio/guile-load-compiled-path
                     ',(parse-path (getenv "GUILE_LOAD_COMPILED_PATH")))
                  out)
           (write `(defvar %guile-studio/guile-load-path
                     ',(parse-path (getenv "GUILE_LOAD_PATH")))
                  out)
           (write `(defvar %guile-studio/prefix ,prefix) out)
           (write `(defvar %guile-studio/picture-language ,picture-language) out)
           (write `(defvar %guile-studio/emacsdir ,emacsdir) out)
           (write `(defvar %guile-studio/guiledir ,guiledir) out)
           ;; Paste the contents of guile-studio.el here
           (call-with-input-file "guile-studio.el"
             (lambda (in)
               (dump-port in out)))))

       (setenv "EMACSLOADPATH"
               (string-join
                (map (cut string-append <> "/share/emacs/site-lisp")
                     emacs-package-dirs) ":" 'suffix))
       (system* "emacs" "--quick" "--batch"
                (format #f "--eval=~a"
                        `(progn
                          (setq byte-compile-debug t)
                          (byte-compile-file ,(string-append "\"" share "/guile-studio.el\"")))))

       ;; CC-BY-SA 4.0 Luis Felipe López Acevedo (aka sirgazil)
       (copy-file "logo.svg"
                  (string-append share "/logo.svg"))

       (make-guile-studio-wrapper prefix share emacsdir emacs-package-dirs)

       ;; Generate Guile init file.
       (with-output-to-file (string-append share "/guile-studio-init.scm")
         (lambda ()
           (format #t "~s" '(begin
                              (define-module (guile-studio-init))
                              (define-public (guile-studio-init . any)
                                (set! (@@ (system repl common) repl-welcome) (const #t))
                                (use-modules (pict)))))))
       (compile-file (string-append share "/guile-studio-init.scm")
                     #:output-file
                     (string-append prefix "/lib/guile/3.0/site-ccache/"
                                    "/guile-studio-init.go")))
     #t)
    ((script . _)
     (format (current-error-port)
             "usage: ~a prefix emacsdir picture-language emacs-package-dirs ...\n"
             script))))

(main)
