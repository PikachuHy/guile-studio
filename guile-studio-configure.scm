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
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (scroll-bar-mode -1)

    (setq window-resize-pixelwise t)

    ;; Show only buffers with the same major mode in the same tab line.
    (require 'tab-line)
    (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers)
    (setq tab-line-close-tab-function 'kill-buffer)

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

    (defun string-display-pixel-width (string)
      "Calculate pixel width of STRING."
      (with-temp-buffer
       (with-silent-modifications
        (setf (buffer-string) string))
       (variable-pitch-mode 1)
       (if (get-buffer-window (current-buffer))
           (car (window-text-pixel-size nil (line-beginning-position) (point)))
           (set-window-buffer nil (current-buffer))
           (car (window-text-pixel-size nil (line-beginning-position) (point))))))

    (defun right-align (string &optional center-p)
      (let ((right-margin 3))
        (concat
         (propertize " " 'display
			         `(space :align-to
                             (- ,(if center-p 'center 'right)
                                (,(+ right-margin (string-display-pixel-width string))))))
         string)))

    ;; Adapted from fancy-splash-insert
    (defun guile-studio-splash-insert (&rest args)
      (let ((current-face nil))
        (while args
          (cond ((eq (car args) :face)
	             (setq args (cdr args) current-face (car args))
	             (if (functionp current-face)
		             (setq current-face (funcall current-face))))
	            ((eq (car args) :link)
	             (setq args (cdr args))
	             (let ((spec (car args)))
	               (if (functionp spec)
		               (setq spec (funcall spec)))
	               (insert-button (car spec)
			                      'face (list 'link current-face)
			                      'action (cadr spec)
			                      'help-echo (concat "mouse-2, RET: "
						                             (or (nth 2 spec)
						                                 "Follow this link"))
			                      'follow-link t)))
	            (t
                 (insert (propertize (car args) 'face current-face))))
          (setq args (cdr args)))))

    (defvar about-guile-studio-text
      `((:face variable-pitch
         "Welcome to Guile Studio, an Emacs environment to play
with the "
         :link ("GNU Guile programming language"
	            ,(lambda (_button) (browse-url "https://www.gnu.org/software/guile/"))
	            "Browse https://www.gnu.org/software/guile/")
         " and its picture language.\n\n"

         :face modus-theme-heading-1 "Manuals\n"
         :face variable-pitch
         "  Learn all about Guile "
         :link (,(right-align "View Guile Manual")
                ,(lambda (_button) (menu-bar-read-guileref)))
         "\n"
         "  How to draw pictures "
         :link (,(right-align "View Picture Language Manual")
                ,(lambda (_button) (menu-bar-read-pictref)))
         "\n\n"
         :face modus-theme-heading-1 "Common commands\n"
         :face variable-pitch
         "  Save " ,(right-align "C-x C-s" t) "\t"
         "  Help " ,(right-align "C-h") "\n"
         
         "  Save as " ,(right-align "C-x C-w" t) "\t"
         "  Cancel " ,(right-align "C-g") "\n"

         "  Open a new file " ,(right-align "C-x C-f" t) "\t"
         "  Undo " ,(right-align "C-/") "\n"

         "  Close side window " ,(right-align "q" t) "\t"
         "  Close buffer " ,(right-align "C-x k") "\n"

         "  Browse directory " ,(right-align "C-x d" t) "\t"
         "  Quit " ,(right-align "C-x C-c") "\n"
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
               (insert "\n")
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
                   (apply (function guile-studio-splash-insert) text)
                   (insert "\n")))
         (use-local-map splash-screen-keymap)
         (setq buffer-read-only t)
         (set-buffer-modified-p nil)
         (if (and view-read-only (not view-mode))
             (view-mode-enter nil 'kill-buffer))
         (goto-char (point-min))
         (forward-line 4))
        (pop-to-buffer splash-buffer 'display-buffer-in-side-window)))

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
                   (inhibit-same-window . t)
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
              "*Geiser dbg*"
              (seq (* anychar) "*Completions" (* anychar)))))
    (add-to-list 'display-buffer-alist
                 `(,popup-bottom-windows
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (inhibit-same-window . t)
                   (side            . bottom)
                   (slot            . 0)
                   (preserve-size   . (t . t))
                   (window-height   . 0.16)))

    (defvar bottom-windows
      (rx (seq "* Guile REPL *" (* anychar))))
    (add-to-list 'display-buffer-alist
                 `(,bottom-windows
                   (display-buffer-reuse-window
                    display-buffer-at-bottom)
                   (window-height   . 10)))

    (require 'dired-sidebar)
    (global-set-key (kbd "C-x d") 'dired-sidebar-toggle-sidebar)
    ;; Delete dired window on "q"
    (define-key dired-mode-map (kbd "q") 'delete-window)

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

    ;; Don't switch buffers when clicking on the name.
    (define-key mode-line-buffer-identification-keymap (vector 'mode-line 'mouse-1) nil)
    (define-key mode-line-buffer-identification-keymap (vector 'mode-line 'mouse-3) nil)

    ;; Context menu on right click.
    (require 'cl-macs)
    (defun context-menu ()
      (let ((menu (make-sparse-keymap)))
        (cl-case major-mode
          (geiser-repl-mode
           (define-key menu (vector 'insert-image)
             '("Insert image" . geiser--guile-picture-language--pict-from-file))
           menu)
          (scheme-mode
           (define-key menu (vector 'eval-buffer)
             '("Evaluate buffer" . geiser-eval-buffer))
           (define-key menu (vector 'lookup-documentation)
             '("Show documentation". geiser-doc-symbol-at-point))
           menu)
          (t
           (mouse-menu-major-mode-map)))))

    (global-set-key (vector 'mouse-3)
                    (lambda (event)
                      (interactive "e")
                      (mouse-set-point event)
                      (popup-menu (context-menu))))

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

    (add-to-list 'initial-frame-alist
                 '(fullscreen . maximized))

    (add-hook 'emacs-startup-hook
              (lambda ()
                (require 'winner)
                (winner-mode 1)
                (let ((buf (generate-new-buffer "untitled.scm")))
                  (with-current-buffer buf
                    (switch-to-buffer buf nil t)
                    (set-window-dedicated-p (selected-window) nil)
                    (set-window-parameter (selected-window) 'guile-studio/edit t)
                    (funcall (and initial-major-mode))
                    (insert ";;; Welcome to Guile Studio!\n")
                    (insert ";;; Type your Guile program here and evaluate it.\n")
                    (setq buffer-offer-save t)

                    (switch-to-geiser)
                    (set-window-dedicated-p (selected-window) t)
                    (call-interactively 'about-guile-studio))

                  ;; This is necessary to show the REPL prompt after
                  ;; displaying the side window.
                  (pop-to-buffer "* Guile REPL *"))

                ;; Always restore default layout
                (defvar guile-studio--layout (winner-conf))
                (define-key global-map (kbd "ESC ESC ESC")
                  (lambda ()
                    (interactive)
                    (keyboard-escape-quit)
                    (winner-set guile-studio--layout)))
                (kill-buffer "*scratch*")

                ;; Hide the cluttered Tools and Options menu items
                (define-key global-map (vector 'menu-bar 'tools) 'undefined)
                (define-key global-map (vector 'menu-bar 'options) 'undefined)))

    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'geiser-repl-mode-hook
              (lambda ()
                (paren-face-mode 1)
                (show-paren-mode 1)))
    (add-hook 'scheme-mode-hook
              (lambda ()
                (paren-face-mode 1)
                (show-paren-mode 1)
                (tab-line-mode 1)))

    ;; Don't show the Geiser menu in a Scheme buffer
    (add-hook 'geiser-mode-hook
              (lambda ()
                (define-key geiser-mode-map
                  (vector 'menu-bar 'geiserm) 'undefined)))

    ;; Color theme
    (require 'modus-themes)                 ; common code
    (require 'modus-operandi-theme)         ; light theme
    (require 'modus-vivendi-theme)          ; dark theme
    (setq modus-themes-scale-headings t
          modus-themes-variable-pitch-headings t
          modus-themes-bold-constructs t
          modus-themes-links 'no-underline)
    (load-theme 'modus-operandi t)

    ;; Increase tab margins
    (let ((palette modus-themes-colors-operandi))
      (set-face-attribute 'tab-line-tab nil
                          :box `(:line-width 8
                                 :color ,(cdr (assoc 'bg-tab-active palette))))
      (set-face-attribute 'tab-line-tab-inactive nil
                          :box `(:line-width 8
                                 :color ,(cdr (assoc 'bg-tab-inactive palette)))))))

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
