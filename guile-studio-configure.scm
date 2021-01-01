(use-modules (ice-9 pretty-print)
             (ice-9 match)
             (ice-9 ftw)
             (srfi srfi-1)
             (srfi srfi-26))

(define (generate-configuration prefix emacsdir guiledir picture-language icons emacs-package-dirs)
  `(progn
    (let ((guix-emacs.el
           (expand-file-name
            ,(string-append emacsdir "/share/emacs/site-lisp/guix-emacs.el"))))
      (when (file-exists-p guix-emacs.el)
        (load guix-emacs.el)))
    (when (require 'guix-emacs nil t)
      (guix-emacs-autoload-packages))

    (setq-default indent-tabs-mode nil)
    (tool-bar-mode 1)
    (menu-bar-mode 1)
    (scroll-bar-mode -1)

    (require 'ivy)
    (ivy-mode 1)
    ;; Enter directory when hitting RET
    (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)

    (require 'info)
    (setq Info-directory-list
          '(,(string-append prefix "/share/guile-studio/info/")
            ,(string-append picture-language "/share/info/")
            ,(string-append guiledir "/share/info/")))

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
      (append (list "-C" ,(string-append prefix "/lib/guile/3.0/site-ccache/"))
              (list "-C" ,(string-append picture-language "/lib/guile/3.0/site-ccache/"))
              params
              (list "-e" "(@ (guile-studio-init) guile-studio-init)")))
    (advice-add 'geiser-guile--parameters
                :filter-return (function guile-studio--geiser-guile--parameters))
    (setq geiser-guile-load-path
          '(,(string-append picture-language
                            "/share/guile/site/3.0/")))

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

    (defun menu-bar-read-guileref ()
      "Display the Guile Reference manual in Info mode."
      (interactive)
      (info "guile"))
    (defun menu-bar-read-pictref ()
      "Display the Picture Language manual in Info mode."
      (interactive)
      (info "picture-language"))
    (defvar about-guile-studio-text
      `((:face (variable-pitch font-lock-comment-face)
         "Welcome to Guile Studio, an Emacs environment to play
with the "
         :link ("GNU Guile programming language"
	            ,(lambda (_button) (browse-url "https://www.gnu.org/software/guile/"))
	            "Browse https://www.gnu.org/software/guile/")
         " and its picture language.\n\n"

         :face variable-pitch
         :link ("View Guile Manual" ,(lambda (_button) (menu-bar-read-guileref)))
         "\tView the Guile manual.\n"
         :link ("View Picture Language Manual" ,(lambda (_button) (menu-bar-read-pictref)))
         "\tView the Picture Language manual.\n"
         "\n")))

    (defun about-guile-studio ()
      "Display the Guile Studio about buffer."
      (interactive)
      (let ((splash-buffer (get-buffer-create "*Guile Studio*")))
        (with-current-buffer
         splash-buffer
         (let ((inhibit-read-only t))
           (erase-buffer)
           (setq default-directory command-line-default-directory)
           (make-local-variable 'startup-screen-inhibit-startup-screen)
           (let* ((image-file ,(string-append prefix "/share/logo.svg"))
	              (img (create-image image-file))
	              (image-width (and img (car (image-size img))))
	              (window-width (window-width)))
             (when img
               (when (> window-width image-width)
                 ;; Center the image in the window.
	             (insert (propertize " " 'display
			                         `(space :align-to (+ center (-0.5 . ,img)))))

                 ;; Insert the image with a help-echo and a link.
	             (make-button (prog1 (point) (insert-image img)) (point)
		                      'face 'default
		                      'help-echo "mouse-2, RET: Browse https://www.gnu.org/software/guile"
		                      'action (lambda (_button) (browse-url "https://www.gnu.org/software/guile"))
		                      'follow-link t)
	             (insert "\n\n"))))
           (dolist (text about-guile-studio-text)
                   (apply (function fancy-splash-insert) text)
                   (insert "\n")))
         (use-local-map splash-screen-keymap)
         (setq buffer-read-only t)
         (set-buffer-modified-p nil)
         (if (and view-read-only (not view-mode))
             (view-mode-enter nil 'kill-buffer))
         (goto-char (point-min))
         (forward-line 4))
        (switch-to-buffer splash-buffer)))

    ;; Unclutter help menu.
    (require 'menu-bar)
    (setq menu-bar-help-menu
          (let ((menu (make-sparse-keymap "Help")))
            (bindings--define-key menu (vector 'about-gnu-project)
                                  '(menu-item "About GNU" describe-gnu-project
                                              :help "About the GNU System, GNU Project, and GNU/Linux"))
            (bindings--define-key menu (vector 'about-guile-studio)
                                  '(menu-item "About Guile Studio" about-guile-studio
                                              :help "About this program"))
            (bindings--define-key menu (vector 'sep2)
                                  menu-bar-separator)
            (bindings--define-key menu (vector 'other-manuals)
                                  '(menu-item "All Other Manuals (Info)" Info-directory
                                              :help "Read any of the installed manuals"))
            (bindings--define-key menu (vector 'guile-reference)
                                  '(menu-item "Guile Reference" menu-bar-read-guileref
                                              :help "Read the Guile Reference manual"))
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
    (require 'flycheck-guile)
    (global-flycheck-mode 1)

    ;; Remember location in buffers
    (require 'saveplace)
    (setq-default save-place t)

    ;; Right side window
    (defvar popup-right-side-windows
      (rx (or "*Guile Studio*"
              "*Geiser documentation*"
              (seq "*Help" (* anychar) "*")
              "*info*")))
    (add-to-list 'display-buffer-alist
                 `(,popup-right-side-windows
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . right)
                   (slot            . 0)
                   (preserve-size   . (t . t))
                   (window-width    . 80)
                   (window-height   . 1.0)))

    ;; Bottom side window
    (defvar popup-bottom-windows
      (rx (or "*Flycheck*"
              "*Flymake*"
              "*Backtrace*"
              "*Warnings*"
              "*Compile-Log*"
              "*Messages*"
              "*Geiser-dbg*"
              (seq (* anychar) "*Completions" (* anychar)))))
    (add-to-list 'display-buffer-alist
                 `(,popup-bottom-windows
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (side            . bottom)
                   (slot            . 0)
                   (preserve-size   . (t . t))
                   (window-height   . 0.16)))

    (defvar bottom-windows
      (rx (seq "* Guile REPL *" (* anychar))))
    (add-to-list 'display-buffer-alist
                 `(,bottom-windows
                   (display-buffer-at-bottom)
                   (window-height   . 10)))

    ;; Mode line settings
    (require 'doom-modeline)
    (setq doom-modeline-buffer-encoding nil)
    (doom-modeline-mode 1)

    ;; Stop using the minibuffer when leaving it
    (defun stop-using-minibuffer ()
      "kill the minibuffer"
      (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
        (abort-recursive-edit)))
    (add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

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
                         :file ,(string-append icons "/24x24/legacy/insert-image.png"))
                  :help "Insert image..."))

    (defvar scheme-tool-bar-map (make-sparse-keymap))
    (define-key scheme-tool-bar-map (vector 'eval-buffer)
      '(menu-item " Evaluate" geiser-eval-buffer
                  :image
                  (image :type png
                         :file ,(string-append icons "/24x24/legacy/media-playback-start.png"))
                  :help "Evaluate buffer..."))
    (define-key scheme-tool-bar-map (vector 'lookup-documentation)
      '(menu-item " Documentation" geiser-doc-symbol-at-point
                  :image
                  (image :type png
                         :file ,(string-append icons "/24x24/legacy/help-faq.png"))
                  :help "Show documentation for the current symbol"))

    (add-to-list 'initial-frame-alist
                 '(fullscreen . maximized))

    (add-hook 'emacs-startup-hook
              (lambda ()
                (let ((buf (generate-new-buffer "untitled.scm")))
                  (switch-to-buffer buf nil t)
                  (funcall (and initial-major-mode))
                  (insert ";;; Welcome to Guile Studio!\n")
                  (insert ";;; Type your Guile program here and evaluate it.\n")
                  (setq buffer-offer-save t)
                  (delete-other-windows)
                  (set-window-dedicated-p (selected-window) t)

                  (run-guile)
                  (set-window-dedicated-p (selected-window) t)

                  (call-interactively 'about-guile-studio)

                  ;; This is necessary to show the REPL prompt after
                  ;; displaying the side window.
                  (pop-to-buffer "* Guile REPL *"))

                (kill-buffer "*scratch*")

                ;; Hide the cluttered Tools and Options menu items
                (define-key global-map (vector 'menu-bar 'tools) 'undefined)
                (define-key global-map (vector 'menu-bar 'options) 'undefined)))

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

    ;; Color theme
    (require 'modus-themes)                 ; common code
    (require 'modus-operandi-theme)         ; light theme
    (require 'modus-vivendi-theme)          ; dark theme
    (load-theme 'modus-operandi t)))

(define (make-guile-studio-wrapper prefix share emacsdir emacs-package-dirs)
  (let ((wrapper (string-append prefix "/bin/guile-studio")))
    (with-output-to-file wrapper
      (lambda ()
        (format #t "#!/bin/sh
EMACSLOADPATH=~a:
exec ~a/bin/emacs --no-site-file --no-site-lisp --no-x-resources --no-init-file --load ~a/guile-studio.el
"
                (string-join
                 (map (cut string-append <> "/share/emacs/site-lisp")
                      emacs-package-dirs) ":")
                emacsdir share)))
    (chmod wrapper #o555)))

(define (main)
  (define (info-file? file)
    (or (string-suffix? ".info" file)
        (string-suffix? ".info.gz" file)))
  (define (info-files top)
    (let ((infodir (string-append top "/share/info")))
      (map (cut string-append infodir "/" <>)
           (or (scandir infodir info-file?) '()))))
  (match (command-line)
    ((_ prefix emacsdir guiledir picture-language icons . emacs-package-dirs)
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
       (with-output-to-file (string-append share "/guile-studio.el")
         (lambda ()
           (pretty-print
            (generate-configuration prefix
                                    emacsdir
                                    guiledir
                                    picture-language
                                    icons
                                    emacs-package-dirs)
            #:display? #f)))

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
             "usage: ~a prefix emacsdir picture-language icons emacs-package-dirs ...\n"
             script))))

(main)
