(use-modules (ice-9 pretty-print)
             (ice-9 match))

(define (generate-configuration prefix emacsdir picture-language icons emacs-package-dirs)
  `(progn
    (let ((guix-emacs.el
           (expand-file-name
            ,(string-append emacsdir "/share/emacs/site-lisp/guix-emacs.el"))))
      (when (file-exists-p guix-emacs.el)
        (load guix-emacs.el)))
    (when (require 'guix-emacs nil t)
      (guix-emacs-autoload-packages ,@emacs-package-dirs))

    (setq-default indent-tabs-mode nil)
    (tool-bar-mode 1)
    (menu-bar-mode 1)
    (set-scroll-bar-mode 'right)

    ;; Use Ctrl-C/X/Z for copy, cut, paste
    (require 'cua-base)
    (cua-mode 1)
    (require 'company)
    (setq company-idle-delay 0.3)
    (require 'elec-pair)
    (electric-pair-mode 1)
    (require 'scheme)
    (require 'geiser)

    ;; Add site-ccache directories to %load-compiled-path, and run the
    ;; init routine.  We don't use geiser-guile-init-file because that
    ;; would lead to compilation messages in the REPL when Guile
    ;; Studio is first started.
    (defun guile-studio--geiser-guile--parameters (params)
      (append (list "-C" ,(string-append prefix "/lib/guile/2.2/site-ccache/"))
              (list "-C" ,(string-append picture-language "/lib/guile/2.2/site-ccache/"))
              params
              (list "-e" "(@ (guile-studio-init) guile-studio-init)")))
    (advice-add 'geiser-guile--parameters
                :filter-return (function guile-studio--geiser-guile--parameters))
    (setq geiser-guile-load-path
          '(,(string-append picture-language
                            "/share/guile/site/2.2/")))

    (setq geiser-autodoc-identifier-format "%s → %s")
    (setq geiser-default-implementation 'guile
          geiser-active-implementations '(guile)

          initial-major-mode 'scheme-mode
          inhibit-splash-screen t
          confirm-kill-processes nil    ; kill Geiser on exit
          x-select-enable-clipboard t
          x-select-enable-primary t
          save-interprogram-paste-before-kill t
          apropos-do-all t
          mouse-yank-at-point t
          require-final-newline t
          visible-bell t
          load-prefer-newer t
          save-place-file (concat user-emacs-directory "places"))

    ;; Hide the fact that this is Emacs
    (modify-frame-parameters nil '((title . "Guile Studio")))

    ;; Unclutter help menu.
    (require 'menu-bar)
    (defun menu-bar-read-guileref ()
      "Display the Guile Reference manual in Info mode."
      (interactive)
      (info "guile"))
    (setq menu-bar-help-menu
          (let ((menu (make-sparse-keymap "Help")))
            (bindings--define-key menu (vector 'about-gnu-project)
                                  '(menu-item "About GNU" describe-gnu-project
                                              :help "About the GNU System, GNU Project, and GNU/Linux"))
            (bindings--define-key menu (vector 'about-emacs)
                                  '(menu-item "About Emacs" about-emacs
                                              :help "Display version number, copyright info, and basic help"))
            (bindings--define-key menu (vector 'sep2)
                                  menu-bar-separator)
            (bindings--define-key menu (vector 'other-manuals)
                                  '(menu-item "All Other Manuals (Info)" Info-directory
                                              :help "Read any of the installed manuals"))
            (bindings--define-key menu (vector 'emacs-manual)
                                  '(menu-item "Read the Emacs Manual" info-emacs-manual
                                              :help "Full documentation of Emacs features"))
            (bindings--define-key menu (vector 'guile-reference)
                                  '(menu-item "Guile Reference" menu-bar-read-guileref
                                              :help "Read the Guile Reference manual"))
            (bindings--define-key menu (vector 'sep1)
                                  menu-bar-separator)
            (bindings--define-key menu (vector 'emacs-tutorial-language-specific)
                                  '(menu-item "Emacs Tutorial (choose language)..."
                                              help-with-tutorial-spec-language
                                              :help "Learn how to use Emacs (choose a language)"))
            menu))
    (bindings--define-key global-map (vector 'menu-bar 'help-menu)
                          (cons (purecopy "Help") menu-bar-help-menu))

    ;; Unclutter File menu
    (setq menu-bar-file-menu
          (let ((menu (make-sparse-keymap "File")))
            (bindings--define-key menu (vector 'exit-emacs)
                                  '(menu-item "Quit" save-buffers-kill-terminal
                                              :help "Save unsaved buffers, then exit"))
            (bindings--define-key menu (vector 'sep-exit)
                                  menu-bar-separator)
            (bindings--define-key menu (vector 'revert-buffer)
                                  '(menu-item "Revert Buffer" revert-buffer
                                              :enable
                                              (or
                                               (not
                                                (eq revert-buffer-function 'revert-buffer--default))
                                               (not
                                                (eq revert-buffer-insert-file-contents-function
                                                    'revert-buffer-insert-file-contents--default-function))
                                               (and buffer-file-number
                                                    (not
                                                     (verify-visited-file-modtime
                                                      (current-buffer)))))
                                              :help "Re-read current buffer from its file"))
            (bindings--define-key menu (vector 'write-file)
                                  '(menu-item "Save As..." write-file
                                              :enable menu-bar-menu-frame-live-and-visible-p
                                              :help "Write current buffer to another file"))
            (bindings--define-key menu (vector 'save-buffer)
                                  '(menu-item "Save" save-buffer :enable
                                              (and (buffer-modified-p)
                                                   (buffer-file-name))
                                              :help "Save current buffer to its file"))
            (bindings--define-key menu (vector 'sep-save)
                                  menu-bar-separator)
            (bindings--define-key menu (vector 'kill-buffer)
                                  '(menu-item "Close" kill-this-buffer :enable
                                              (kill-this-buffer-enabled-p)
                                              :help "Discard (kill) current buffer"))

            (bindings--define-key menu (vector 'dired)
                                  '(menu-item "Open Directory..." dired
                                              :help "Read a directory, to operate on its files"))
            (bindings--define-key menu (vector 'open-file)
                                  '(menu-item "Open File..." menu-find-file-existing
                                              :help "Read an existing file into an Emacs buffer"))
            (bindings--define-key menu (vector 'new-file)
                                  '(menu-item "Visit New File..." find-file
                                              :enable menu-bar-non-minibuffer-window-p
                                              :help "Specify a new file's name, to edit the file"))
            menu))
    (bindings--define-key global-map (vector 'menu-bar 'file)
                          (cons (purecopy "File") menu-bar-file-menu))

    ;; Check syntax on the fly
    (require 'flycheck)
    (flycheck-define-checker guile
                             "A Guile syntax checker with `guild compile'."
                             :command ("guild" "compile" "--to=cps"
                                       "--warn=unused-variable"
                                       "--warn=unused-toplevel"
                                       "--warn=unbound-variable"
                                       "--warn=macro-use-before-definition"
                                       "--warn=arity-mismatch"
                                       "--warn=duplicate-case-datum"
                                       "--warn=bad-case-datum"
                                       "--warn=format"
                                       source)
                             :predicate
                             (lambda ()
                               (and (boundp 'geiser-impl--implementation)
                                    (eq geiser-impl--implementation 'guile)))
                             :verify
                             (lambda (checker)
                               (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
                                 (list
                                  (flycheck-verification-result-new
                                   :label "Geiser Implementation"
                                   :message (cond
                                             ((eq geiser-impl 'guile) "Guile")
                                             (geiser-impl (format "Other: %s" geiser-impl))
                                             (t "Geiser not active"))
                                   :face (cond
                                          ((or (eq geiser-impl 'guile)) 'success)
                                          (t '(bold error)))))))
                             :error-patterns
                             ((warning
                               line-start (file-name) ":" line ":" column ": warning:" (message) line-end)
                              (error
                               line-start (file-name) ":" line ":" column ":" (message) line-end))
                             :modes (scheme-mode geiser-mode))
    (add-to-list 'flycheck-checkers 'guile)
    (global-flycheck-mode 1)

    ;; Remember location in buffers
    (require 'saveplace)
    (setq-default save-place t)

    ;; Mode line settings
    (require 'smart-mode-line)
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'respectful)
    (setq sml/position-percentage-format nil)
    (setq sml/mule-info nil)
    (setq sml/read-only-char
          (propertize "R" 'display
                      (create-image "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"14\" viewBox=\"0 0 448 612\">\
<path fill=\"currentColor\" \
d=\"M400 224h-24v-72C376 68.2 307.8 0 224 0S72 68.2 72 \
152v72H48c-26.5 0-48 21.5-48 48v192c0 26.5 21.5 48 48 48h352c26.5 \
0 48-21.5 48-48V272c0-26.5-21.5-48-48-48zm-104 0H152v-72c0-39.7 \
32.3-72 72-72s72 32.3 72 72v72z\"></path></svg>" 'svg t)))
    (sml/setup)
    (setq rm-whitelist '("Paredit"))

    (require 'uniquify)
    (setq uniquify-buffer-name-style 'forward)

    ;; Add close button for opened buffers.
    (require 'mouse)
    (defconst my-mode-line-map
      (let ((map (make-sparse-keymap)))
        (define-key map (vector 'mode-line 'mouse-1)
          'mouse-delete-window)
        map))
    (setq global-mode-string 
          (append global-mode-string 
                  '(:eval (if (window-dedicated-p (selected-window))
                              ""
                              (propertize "[⨯]"
                                          'local-map my-mode-line-map
                                          'mouse-face 'mode-line-highlight)))))
    (global-unset-key (vector 'mode-line 'mouse-2)) ; 'mouse-delete-other-windows
    (global-unset-key (vector 'mode-line 'mouse-3)) ; 'mouse-delete-window


    (defun geiser--guile-picture-language--pict-from-file ()
      (interactive)
      (let ((file (read-file-name "Insert image: " nil nil t nil
                                  (lambda (name)
                                    (or (string-suffix-p ".svg" name t)
                                        (string-suffix-p ".png" name t))))))
        (geiser-repl--send
         (concat "(pict-from-file \""
                 file
                 "\")"))))

    (defvar geiser-repl-tool-bar-map (make-sparse-keymap))
    (define-key geiser-repl-tool-bar-map (vector 'insert-image)
      '(menu-item " Insert image" geiser--guile-picture-language--pict-from-file
                  :image
                  (image :type png
                         :file ,(string-append icons "/24x24/actions/insert-image.png"))
                  :help "Insert image..."))

    (defvar scheme-tool-bar-map (make-sparse-keymap))
    (define-key scheme-tool-bar-map (vector 'eval-buffer)
      '(menu-item " Evaluate" geiser-eval-buffer
                  :image
                  (image :type png
                         :file ,(string-append icons "/24x24/actions/media-playback-start.png"))
                  :help "Evaluate buffer..."))
    (define-key scheme-tool-bar-map (vector 'lookup-documentation)
      '(menu-item " Documentation" geiser-doc-symbol-at-point
                  :image
                  (image :type png
                         :file ,(string-append icons "/24x24/actions/help-faq.png"))
                  :help "Show documentation for the current symbol"))
    
    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((buf (generate-new-buffer "untitled.scm")))
                  (switch-to-buffer buf nil t)
                  (funcall (and initial-major-mode))
                  (setq buffer-offer-save t)
                  (delete-other-windows)
                  (set-window-dedicated-p (selected-window) t))
                (run-guile)
                (set-window-dedicated-p (selected-window) t)
                ;; Hide the cluttered Tools and Options menu items
                (define-key global-map (vector 'menu-bar 'tools) 'undefined)
                (define-key global-map (vector 'menu-bar 'options) 'undefined)

                ;; Prefer horizontal splits
                (setq split-height-threshold nil)
                (setq split-width-threshold 80)))

    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'geiser-repl-mode-hook
              (lambda ()
                (paren-face-mode 1)
                (show-paren-mode 1)
                (unless (local-variable-p 'tool-bar-map)
                  (set (make-local-variable 'tool-bar-map)
                       geiser-repl-tool-bar-map))))
    (add-hook 'scheme-mode-hook
              (lambda ()
                (paren-face-mode 1)
                (show-paren-mode 1)
                (unless (local-variable-p 'tool-bar-map)
                  (set (make-local-variable 'tool-bar-map)
                       scheme-tool-bar-map))))

    ;; Don't show the Geiser menu in a Scheme buffer
    (add-hook 'geiser-mode-hook
              (lambda ()
                (define-key geiser-mode-map
                  (vector 'menu-bar 'geiserm) 'undefined)))
    (load-theme 'adwaita t)))

(define (make-guile-studio-wrapper prefix share emacsdir)
  (let ((wrapper (string-append prefix "/bin/guile-studio")))
    (with-output-to-file wrapper
      (lambda ()
        (format #t "#!/bin/sh
exec ~a/bin/emacs -Q --load ~a/guile-studio.el
"
                emacsdir share)))
    (chmod wrapper #o555)))

(define (main)
  (match (command-line)
    ((_ prefix emacsdir picture-language icons . emacs-package-dirs)
     (let ((share (string-append prefix "/share")))
       (with-output-to-file (string-append share "/guile-studio.el")
         (lambda ()
           (pretty-print (generate-configuration prefix emacsdir picture-language icons emacs-package-dirs)
                         #:display? #f)))
       (make-guile-studio-wrapper prefix share emacsdir)
       (with-output-to-file (string-append share "/guile-studio-init.scm")
         (lambda ()
           (format #t "~s" '(begin
                              (define-module (guile-studio-init))
                              (define-public (guile-studio-init . any)
                                (set! (@@ (system repl common) repl-welcome) (const #t))
                                (use-modules (pict)))))))
       (compile-file (string-append share "/guile-studio-init.scm")
                     #:output-file
                     (string-append prefix "/lib/guile/2.2/site-ccache/"
                                    "/guile-studio-init.go")))
     #t)
    ((script . _)
     (format (current-error-port)
             "usage: ~a prefix emacsdir picture-language icons emacs-package-dirs ...\n"
             script))))

(main)
