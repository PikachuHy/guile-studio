(use-modules (ice-9 pretty-print)
             (ice-9 match))

(define (generate-configuration prefix emacsdir picture-language icons emacs-package-dirs)
  `(progn
    (load (expand-file-name
           ,(string-append emacsdir "/share/emacs/site-lisp/guix-emacs.el")))
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
    (setq geiser-guile-load-path
          '(,(string-append picture-language
                            "/share/guile/site/2.2/")
            ,(string-append picture-language
                            "/lib/guile/2.2/site-ccache/")))
    (setq geiser-guile-init-file ,(string-append prefix
                                                 "/share/guile-studio-init"))
    (setq geiser-autodoc-identifier-format "%s ~ %s")
    (setq geiser-default-implementation 'guile
          initial-major-mode 'scheme-mode
          inhibit-splash-screen t
          x-select-enable-clipboard t
          x-select-enable-primary t
          save-interprogram-paste-before-kill t
          apropos-do-all t
          mouse-yank-at-point t
          require-final-newline t
          visible-bell t
          load-prefer-newer t
          save-place-file (concat user-emacs-directory "places"))

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
      (let ((file (read-file-name "Insert image: " nil nil t)))
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
                (set-window-dedicated-p (selected-window) t)))
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
       (with-output-to-file (string-append share "/guile-studio-init")
         (lambda ()
           (format #t "~s" '(use-modules (pict))))))
     #t)
    ((script . _)
     (format (current-error-port)
             "usage: ~a prefix emacsdir picture-language icons emacs-package-dirs ...\n"
             script))))

(main)
