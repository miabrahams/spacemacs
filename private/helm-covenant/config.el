;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; Copyright (c) 2015 Michael Abrahams
;; Author: Michael Abrahams <miabraha@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

;; (defvar git-enable-github-support nil
;;   "If non nil the Github packages and extensions are enabled.")

;; (defvar git-enable-magit-svn-plugin nil
;;   "If non nil `magit-svn' plugin is enabled.")

;; (defvar git-magit-status-fullscreen nil
;;   "If non nil magit-status buffer is displayed in fullscreen.")

;; (defvar git-gutter-use-fringe t
;;   "If non nil the fringe is used to display git-gutter icons.")

;; Command prefixes
;; (global-set-key (kbd " M-:") 'helm-eval-expression-with-eldoc)
;; (define-key helm-command-map (kbd "M-:") 'helm-occur)


;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*helm" (* not-newline) "*" eos)
;;                (display-buffer-in-side-window)
;;                (inhibit-same-window . t)
;;                (window-height . 0.35)))

(setq-default helm-semantic-fuzzy-match t ;; Fuzzy match everywhere
              helm-recentf-fuzzy-match t
              helm-buffers-fuzzy-matching t
              helm-M-x-fuzzy-match t
              helm-imenu-fuzzy-match t
              helm-ls-git-fuzzy-match t
              helm-gtags-fuzzy-match t
              helm-apropos-fuzzy-match t
              helm-ff-search-library-in-sexp t
              helm-ff-auto-update-initial-value 1
              helm-ff-file-name-history-use-recentf t
              helm-truncate-lines t
              helm-idle-delay 0.01
              helm-input-idle-delay 0.01
              helm-quick-update t
              helm-adaptive-mode t
              helm-scroll-amount 8
              helm-buffer-max-length 30
              helm-adaptive-history-file "~/.emacs.d/data/helm-history"
              helm-move-to-line-cycle-in-source t
              helm-scroll-amount 8
              helm-completion-in-region-fuzzy-match t
              helm-display-header-line nil
              helm-ff-transformer-show-only-basename nil
              helm-info-default-sources
              '(helm-source-info-elisp
                helm-source-info-cl helm-source-info-eieio helm-source-info-pages helm-source-info-emacs helm-source-info-global)
              helm-gtags-maximum-candidates 30
              )

(defcustom helm-command-prefix-key (if (spacemacs/system-is-mswindows)
                                       "<apps>"
                                     "<menu>")
  "The key `helm-command-prefix' is bound to in the global map."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (when (and (boundp var) (symbol-value var))
      (define-key (current-global-map)
        (read-kbd-macro (symbol-value var)) nil))
    (when key
      (define-key (current-global-map)
        (read-kbd-macro key) 'helm-command-prefix))
    (set var key)))

(defcustom helm-minibuffer-history-key "C-r"
  "The key `helm-minibuffer-history' is bound to in minibuffer local maps."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (cl-dolist (map '(minibuffer-local-completion-map
                      minibuffer-local-filename-completion-map
                      minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                      minibuffer-local-isearch-map
                      minibuffer-local-map
                      minibuffer-local-must-match-filename-map ; Older Emacsen
                      minibuffer-local-must-match-map
                      minibuffer-local-ns-map))
      (when (and (boundp map) (keymapp (symbol-value map)))
        (when (and (boundp var) (symbol-value var))
          (define-key (symbol-value map)
            (read-kbd-macro (symbol-value var)) nil))
        (when key
          (define-key (symbol-value map)
            (read-kbd-macro key) 'helm-minibuffer-history))))
    (set var key)))


(spacemacs|use-package-add-hook helm
  :post-config
  (defun helm-apropos (&optional arg)
    "Preconfigured helm to describe commands, functions, variables and faces."
    (interactive)
    (let ((default (thing-at-point 'symbol)))
      (helm :sources
            (mapcar (lambda (func)
                      (funcall func default))
                    #'helm-apropos-function-list)
            :buffer "*helm apropos*"
            :preselect (and default (concat "\\_<" (regexp-quote default) "\\_>")))))
  (define-key help-map [(control h)] #'helm-apropos)
  (setq-default apropos-sort-by-scores t)

  (defun info-apropos-at-point ()
    "Search info manuals for references to thing-at-point."
    (interactive) (info-apropos (thing-at-point 'symbol)))
  (defun apropos-at-point ()
    "Find documentation for thing-at-point."
    (interactive) (apropos (thing-at-point 'symbol) t))
  (define-key help-map (kbd "a") 'apropos-at-point)
  (define-key help-map (kbd "I") 'info-apropos-at-point)

  (defvar helm-command-map
    (let ((map (make-sparse-keymap)))
      ;; For now, don't add any bullshit
      (define-key map (kbd "/")         #'helm-find)
      (define-key map (kbd "a")         #'helm-do-ag-project-root)
      (define-key map (kbd "c")         #'helm-colors)
      (define-key map (kbd "f")         #'helm-find-files)
      (define-key map (kbd "h a")       #'helm-apropos)
      (define-key map (kbd "h m")       #'helm-man-woman)
      (define-key map (kbd "h h")       #'helm-helpful)
      (define-key map (kbd "h i")       #'helm-info-at-point)
      (define-key map (kbd "h r")       #'helm-info-emacs)
      (define-key map (kbd "i")         #'helm-semantic-or-imenu)
      (define-key map (kbd "t")         #'helm-top)
      (define-key map (kbd "x")         #'helm-M-x)
      (define-key map (kbd "y")         #'helm-show-kill-ring)
      (define-key map (kbd "r")         #'helm-register)
      (define-key map (kbd "b")         #'helm-buffers-list)
      (define-key map (kbd "C-r")       #'helm-recentf)
      (define-key map (kbd ",")         #'helm-calcul-expression)
      (define-key map (kbd "C-:")       #'helm-eval-expression-with-eldoc)
      (define-key map (kbd "C-x")       #'helm-all-mark-rings)

      (define-key map (kbd "\;")        #'helm-resume)
      (define-key map (kbd helm-command-prefix-key)    #'helm-omni)

      ;; Bullshit goes here
      ;; (define-key map (kbd "!")         #'helm-run-external-command)
      ;; (define-key map (kbd "C-i")       #'helm-execute-persistent-action)
      ;; (define-key map (kbd "k")         #'helm-mini)
      ;; (define-key map (kbd "l")         #'helm-locate)
      ;; (define-key map (kbd "s")         #'helm-surfraw)  ;; Might be cool, basically google?
      ;; (define-key map (kbd "M-n")       #'helm-gid)
      ;; (define-key map (kbd "e")         #'helm-etags-select)
      ;; (define-key map (kbd "8")         #'helm-ucs)  ;; For math?
      ;; (define-key map (kbd "F")         #'helm-select-xfont)
      ;; (define-key map (kbd "C-c f")     #'helm-recentf)
      ;; (define-key map (kbd "C-c g")     #'helm-google-suggest)
      ;; (define-key map (kbd "C-x r b")   #'helm-filtered-bookmarks)
      map))
  ;; (define-key map (kbd "<tab>")     #'helm-lisp-completion-at-point)

  (defvar helm-command-prefix)
  (define-prefix-command 'helm-command-prefix 'helm-command-prefix "<helm>")
  (fset 'helm-command-prefix helm-command-map)
  (setq  helm-command-prefix helm-command-map)
  (global-set-key (kbd helm-command-prefix-key) 'helm-command-prefix)

  (define-key helm-map (kbd "C-z")       #'helm-select-action)
  (define-key helm-map (kbd "<tab>")     #'helm-execute-persistent-action)
  (define-key helm-map (kbd "M-o")       #'helm-next-source)
  )




(defun helm-switch-buffer-or-start (buffer-name launcher clear)
  (lambda ()
    (if (get-buffer buffer-name)
        (progn
          (switch-to-buffer buffer-name)
          (if clear (delete-other-windows)))
      (launcher))))

(defvar helm-source-apps "Helm source for running commands")

(setq helm-source-apps
  `((name . "Applications")
    (candidates . (("org-capture" . (lambda () (org-capture nil )))
                   ("Notes.org" . (lambda () (switch-to-buffer "notes.org")))
                   ("Pomodoro" . #'org/init-org-pomodoro)
                   ("Shell" .
                    ,(helm-switch-buffer-or-start
                      "*Shell*" 'shell-switcher-switch-buffer nil))
                   ;; ("Mail" . ,(helm-switch-buffer-or-start "*mu4e-headers*" 'mu4e t))
                   ;; ("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ;; ("RSS" . elfeed)
                   ))
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defvar helm-source-favorites  "Helm source for favorite files list")

(defun set-helm-hotspots (sym val)
  (set-default sym val)
  (setq helm-source-favorites
        `((name . "My Locations")
          (candidates . ,val)
          (action . (("Open" . (lambda (x) (find-file x))))))))

(defcustom helm-hotspots-favorites
  `(("projects.org" . "~/text/projects.org")
    ("notes.org" . "~/text/notes.org")
    ("experimental.el" . "~/.emacs.d/private/experimental.el" )
    ("Krita/CMakeLists" . "~/code/Krita/code/CMakeLists.txt")
    ("krita-linux.org" . "~/Dropbox/krita_notes/krita-linux.org")
    ("linux-notes.org" . "~/text/etc/linux-notes.org")
    ("linux-install.org" . "~/text/etc/linux-install.org"))
  "List of favorite files"
  :type '(alist :key-type string :value-type file)
  :group 'helm
  :set #'set-helm-hotspots)


;; Things to put in Helm-Helpful:
;; a:   Shows commands that match a regular expression (regexp).
;; d:   Shows Emacs functions and variables whose doc matches a regexp.
;; M-x describe-bindings:   Shows current key bindings: keyboard, menu bar, and mouse.
;; c:   Shows the doc for an Emacs command.
;; f:   Shows the doc for an Emacs function.
;; F:   Opens the Emacs manual for an Emacs command.
;; S:   Searches info pages for symbols
;; m:   Describes the current major and minor modes.
;; o:   Shows an Emacs user option's value and documentation.
;; v:   Shows an Emacs variable's value and documentation.
;; C-a: Shows Emacs functions and variables that match a regexp.
;; p:   Finds Emacs-Lisp libraries that match a topic.
;; C-M-a: Shows the tags matched by a given string.
;; u:   Finds a topic in the Unix manual.
;; <help> C-d      view-emacs-debugging
;; <help> C-l      locate-library
;; Other:
;; <help> e        view-echo-area-messages
;; <help> p        finder-by-keyword (finds packages)
;; <help> P        describe-package

(defun helm-helpful ()
  "A Frankenstein help command"
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (mapcar (lambda (func)
                    (funcall func default))
                  #'helm-apropos-function-list)
          :buffer "*helm apropos*"
          :preselect (and default (concat "\\_<" (regexp-quote default) "\\_>")))))



(setq-default projectile-indexing-method 'alien)

(defun helm-omni (&rest arg)
  "Helm interface to my favorite places, which includes apps, locations, project files and bookmarks"
  (interactive)
  (helm :sources (append
                  (list
                   helm-source-apps
                   helm-source-bookmarks)
                  (if (and (projectile-project-p)
                           (boundp 'helm-source-projectile-buffers-list))
                      (list helm-source-projectile-buffers-list
                            helm-source-buffers-list
                            helm-source-projectile-recentf-list
                            helm-source-recentf
                            helm-source-projectile-files-list
                            helm-source-files-in-current-dir
                            helm-source-locate)
                    (list helm-source-buffers-list
                          helm-source-recentf
                          helm-source-files-in-current-dir
                          ))
    (list helm-source-file-cache
          ;; helm-source-locate
          helm-source-buffer-not-found))
   ))


;; (spacemacs|use-package-add-hook guide-key
;;   :post-config
;;   (add-to-list 'guide-key/guide-key-sequence helm-command-prefix-key))

;; (spacemacs|use-package-add-hook helm-projectile
;;   :post-config
;;   (define-key helm-command-map "g" 'spacemacs/helm-projectile-pt))

;;; Command Keymap
;;
;;
