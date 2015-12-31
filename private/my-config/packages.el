;;; packages.el --- my-config Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Michael Abrahams
;; Author: Michael Abrahams <miabraha@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(setq-default my-config-packages
    '(
      ;; package my-configs go here

      ninja-mode
      profiler
      ;; general-close   ; Wait until available in MELPA
      ;; flycheck-tip
      systemd
      save-visited-files
      org-journal
      cmake-project
      dired+
      avy-zap
      ;; back-button
      ;; highlight-tail
      ;; counsel ;Check this out
      ;; cmake-ide
      ;; apropospriate-theme
      ;; bliss-theme
      ;; lush-theme
      ;; cmake-font-lock

      comment-dwim-2
      dired-single
      bookmark+
      corral
      howdoi
      help-mode+
      olivetti
      org2blog
      rtags

      filesets
      helm-filesets
      filesets+
      synonyms

      windresize
      hideshow
      pos-tip
      persistent-scratch
      julia-mode
      julia-shell
      ))


;; List of packages to exclude.
(setq-default my-config-excluded-packages
              '(
                ;; company-clang
                helm-company
                ))

(defun my-config/init-julia-shell ()
  (use-package julia-shell :ensure t :defer t))

(defun my-config/init-julia-mode ()
  (use-package julia-mode
    :ensure t
    :defer t
    :config
    (defun my-julia-mode-hooks ()
      (require 'julia-shell-mode))
    (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
    (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
    (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)
    ))




(defun my-config/init-hideshow ()
  (use-package "hideshow"
    :ensure t
    :commands (hs-minor-mode hs-hide-all hs-show-all)
    :init
    (add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))
    ))

(defun my-config/init-olivetti ()
  (use-package "olivetti"
    :ensure t
    :commands (olivetti-mode)
    ))

(defun my-config/init-org2blog ()
  (use-package "org2blog"
    :ensure t
    :commands (org2blog/wp-login)
    :init
    (setq org2blog/wp-blog-alist
          '(("Risk and Time"
             :url "http://riskandtime.net/xmlrpc.php"
             :username "abrahams"
             :password "WRlecS%FVNDQ%HT67p"
             :default-title "New Post"
             :default-categories ("Finance")
             :tags-as-categories nil)
            ))
    ))

(defun my-config/init-persistent-scratch ()
  (use-package "persistent-scratch"
    :ensure t
    :init
    (setq-default persistent-scratch-save-file "~/.emacs.d/.cache/.persistent-scratch")
    (persistent-scratch-setup-default)
    ))

(defun my-config/init-pos-tip ()
  (use-package "pos-tip"
    :ensure t
    :commands (pos-tip-show)
    :init
    (defun describe-in-popup (fn)
      "Open a postip containing the help text of `symbol-at-point' using FN.
FN should be either `describe-variable' or `describe-function'."
      (let* ((thing (symbol-at-point))
             (description
              (replace-regexp-in-string
               "\\[BACK\\]" ""
               (save-window-excursion
                 (funcall fn thing)
                 (switch-to-buffer "*Help*")
                 (buffer-string)))))
        ;; (pos-tip-show description 'popup-tip-face nil nil -1)
        (pos-tip-show description nil nil nil -1)
        )))
  )


(defun my-config/init-windresize ()
  (use-package "windresize"
    :ensure t
    :commands (windresize)
    :init
    (global-set-key (kbd "C-x ^") 'windresize)
    ))




(defun my-config/init-dired+ ()
  (use-package dired+
    :config
    (diredp-toggle-find-file-reuse-dir t)))

(defun my-config/init-help-mode+ ()
    (use-package "help-mode+" :ensure t))

(defun my-config/init-systemd ()
  (use-package "systemd"
    :ensure t
    :commands 'systemd-mode))

(defun my-config/init-org-journal ()
  (use-package "org-journal" :ensure t
    :commands 'org-journal-new-entry
    :config
    (setq-default org-journal-dir (concat dropbox-directory "text/journal"))
    (global-set-key (kbd "C-c j") 'org-journal-new-entry)))

(defun my-config/init-cmake-project ()
  (use-package "cmake-project" :ensure t
    :config
    (defun maybe-cmake-project-hook ()
      (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
    (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
    (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)))

(defun my-config/init-comment-dwim-2 ()
  (use-package "comment-dwim-2"
    :ensure t
    :commands 'comment-dwim-2
    :config
    (global-set-key (kbd "M-;") 'comment-dwim-2)))

(defun my-config/init-synonyms ()
  (use-package "synonyms"
    :ensure t
    :commands 'synonyms
    :config
    (setq synonyms-file        "~/private/etc/mthesaur.txt")
    (setq synonyms-cache-file "~/private/etc/synonyms.cache")
    ;; Define a key here later
    ))

(defun my-config/init-howdoi ()
  (use-package "howdoi"
    :ensure t
    :commands 'howdoi-query
    :config
    (define-key help-map (kbd "?") 'howdoi-query)))



;; (defun my-config/init-general-close ()
;;   (use-package "general-close" :ensure t
;;     :config
;;     (global-set-key (kbd "M-)") #'general-close)
;;     ))


;; Sadly not working  :c
(defun my-config/init-back-button ()
  (if spacemacs/system-is-mswindows
      (use-package "back-button"
        :ensure t
        :init
        (back-button-mode 1)
        (global-set-key (kbd "<M-left>")  #'back-button-local-backward)
        (global-set-key (kbd "<M-right>") #'back-button-local-forward)
        )))

;; (defun my-config/init-cmake-font-lock ()
;;   (use-package "cmake-font-lock"
;;     :ensure t
;;     :commands
;;     ;; Read the manual to learn how to improve cmake customizations:
;;     (cmake-font-lock-add-keywords "my-func" '("FILE" "RESULT" "OTHER"))
;;     (cmake-font-lock-set-signature "my-func" '(:var nil :prop) '(("FILE" :file) ("RESULT" :var)))
;;     (cmake-font-lock-set-signature "my_warn" (:warning))
;;     (add-to-list '(:warning . font-lock-warning-face) cmake-font-lock-argument-kind-face-alist)
;; ))



;; Spacemacs/use-package macro
;; (defconst spacemacs-use-package-font-lock-keywords
;;   '(("(\\(spacemacs/\\[use|init\\]-package\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
;;      (1 font-lock-keyword-face)
;;      (2 font-lock-constant-face nil t))))
;; (font-lock-add-keywords 'emacs-lisp-mode spacemacs-use-package-font-lock-keywords)


;; (defun configuration-layer//load-layers-files (layers files)
;;   "Load the files of list FILES for all passed LAYERS."
;;   (dolist (layer layers)
;;     (configuration-layer//load-layer-files layer files)))

;; (defvar spacemacs-layername "spacemacs")

;; (defmacro spacemacs/use-package (package-name &rest ARGS)
;;   (let ((package-string (if (stringp package-name)
;;                             package-name
;;                           (symbol-name package-name)))
;;         (layer-string   (if (boundp 'spacemacs-layername)
;;                             (if (stringp spacemacs-layername)
;;                                 spacemacs-layername
;;                               (symbol-name spacemacs-layername))
;;                           "spacemacs")))
;;     (if ARGS
;;         `(progn (defun ,(intern (concat layer-string "/init-" package-string)) ()
;;                   (use-package ,package-string ,@ARGS)))
;;       `(progn (defun ,(intern (concat layer-string "/init-" package-string)) ()
;;                 (use-package ,package-string))))))

;; (spacemacs/use-package "corral"
;;                        :ensure t
;;                        :config
;;                        (global-set-key (kbd "M-9") #'corral-parentheses-backward)
;;                        (global-set-key (kbd "M-0") #'corral-parentheses-forward)
;;                        (global-set-key (kbd "M-[") #'corral-brackets-backward)
;;                        (global-set-key (kbd "M-]") #'corral-brackets-forward)
;;                        (global-set-key (kbd "M-\"") #'corral-double-quotes-backward)
;;                        (setq-default corral-preserve-point t))



(defun my-config/init-rtags ()
  (if ( spacemacs/system-is-linux )
      (use-package "rtags"
        :ensure t
        :commands 'rtags-mode
        :init
        (setq-default rtags-autostart-diagnostics t
                      rtags-completions-enabled t)
        (defun my-rtags-setup ()
          (flycheck-select-checker 'rtags)
          (rtags-mode))
        (add-hook 'c-mode-common-hook #'my-rtags-setup)

        :config
        ;; c-mode-common-hook is also called by c++-mode
        (push 'company-rtags company-backends)

        (define-key c-mode-base-map (kbd "M-.")   #'rtags-find-symbol-at-point)
        (define-key c-mode-base-map (kbd "M-,")   #'rtags-find-references-at-point)
        (define-key c-mode-base-map (kbd "C-M-.") #'rtags-find-file)
        (define-key c-mode-base-map (kbd "C-.")   #'rtags-find-symbol)
        (define-key c-mode-base-map (kbd "C-,")   #'rtags-find-references)
        (define-key c-mode-base-map (kbd "C-<")   #'rtags-find-virtuals-at-point)
        (define-key c-mode-base-map (kbd "C-M-;") #'rtags-imenu)
        (define-key c-mode-base-map (kbd "M-'")   #'company-complete)

        ;; THIS IS AN AMAZING BUFFER, PLEASE GIVE IT LOVE
        (define-key c-mode-base-map (kbd "C-M-?") #'rtags-diagnostics)
        )))


(defun my-config/init-flycheck-tip ()
  (use-package "flycheck-tip"
    :ensure t
    :config
    (eval-after-load 'flycheck
      (progn
        (require 'flycheck-tip)
        (flycheck-tip-use-timer 'verbose)
        (define-key prog-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)))))

(defun my-config/init-save-visited-files ()
  (use-package "save-visited-files"
    :ensure t
    :config
    (turn-on-save-visited-files-mode)))

(defun my-config/init-ninja-mode ()
  (use-package "ninja-mode"
    :ensure t
    :commands 'ninja-mode
    ))

(defun my-config/init-profiler ()
  (use-package "profiler" :defer t))

(defun my-config/init-avy-zap ()
  (use-package "avy-zap"
    :ensure t
    :commands 'avy-zap-up-to-char-dwim
    :init
    (global-set-key (kbd "M-z") #'avy-zap-up-to-char-dwim)))


;; (defun my-config/init-apropospriate-theme ()
;;   (use-package "apropospriate-theme" :ensure t))


;; (defun my-config/init-bliss-theme ()
;;   (use-package "bliss-theme" :ensure t))


;; (defun my-config/init-highlight-tail ()
;;   (use-package "highlight-tail"
;;     :ensure t
;;     :config
;;     (highlight-tail-mode)
;;     ;; Define a key here later
;;     ))

;; (defun my-config/helm-gtags-define-keys-for-mode (mode)
;;   "Define key bindings for the specific MODE."
;;   (when (fboundp mode)
;;     (let ((hook (intern (concat (symbol-name mode) "-hook"))))
;;       (add-hook hook 'helm-gtags-mode))
;;     (evil-leader/set-key-for-mode mode
;;       "mgc" 'helm-gtags-create-tags
;;       "mgd" 'helm-gtags-find-tag
;;       "mgf" 'helm-gtags-select-path
;;       "mgg" 'helm-gtags-dwim
;;       "mgG" 'helm-gtags-dwim-other-window
;;       "mgi" 'helm-gtags-tags-in-this-function
;;       "mgl" 'helm-gtags-parse-file
;;       "mgn" 'helm-gtags-next-history
;;       "mgp" 'helm-gtags-previous-history
;;       "mgr" 'helm-gtags-find-rtag
;;       "mgR" 'helm-gtags-resume
;;       "mgs" 'helm-gtags-select
;;       "mgS" 'helm-gtags-show-stack
;;       "mgu" 'helm-gtags-update-tags)))
