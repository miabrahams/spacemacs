;;; packages.el --- helm-covenant Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 Michael Abrahams
;; Author: Michael Abrahams <miabraha@gmail.com>
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar helm-covenant-packages
  '(
    ;; helm-company
    ;; helm
    ;; helm-descbinds
    ;; helm-flycheck
    ;; helm-projectile
    ;; stickyfunc-enhance
    ;; swiper
    ;; helm-pt
    ;; pcomplete-extension
    ;; helm-ls-git
    ;; helm-pt
    )
  "Place packages to install and/or initialize here.
Built-in packages which require an initialization must be listed explicitly in the list.")

(defvar helm-covenant-excluded-packages '(iedit)
  "List of packages to exclude.")

;; Package configurations are below.

;; (defun helm-covenant/init-helm-flycheck ()
;;   (use-package "helm-flycheck"
;;     :ensure t
;;     :config
;;     (add-hook 'prog-mode-hook 'flycheck-mode)
;;     (define-key helm-command-map (kbd "c") 'helm-flycheck)))

;; (defun helm-covenant/init-helm-gtags ()
;;   (use-package "helm-gtags"
;;     :ensure t
;;     :config
;;     (add-hook 'c-mode-hook 'helm-gtags-mode)
;;     (add-hook 'c++-mode-hook 'helm-gtags-mode)
;;     (add-hook 'asm-mode-hook 'helm-gtags-mode)
;;     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


;; (defun helm-covenant/init-stickyfunc-enhance ()
;;   (use-package "stickyfunc-enhance"
;;     :ensure t
;;     :config
;;     (semantic-mode)
;;     (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)))


;; Consider integrating with this package:
;; (use-package "helm-ag"
;;   :ensure t
;;   :config
;;   (setq-default
;;    helm-ag-base-command "pt --nocolor --nogroup --ignore-case"
;;    helm-ag-command-option "--all-text")
;;   (define-key helm-map "G" #'helm-do-ag))
;; (defun projectile-helm-ag ()
;;   (interactive)
;;   (helm-ag (projectile-project-root)))
;; (setq-default projectile-switch-project-action 'helm-projectile-find-file))

;; (defun helm-covenant/init-helm-ls-git ()
;;   (use-package "helm-ls-git" :ensure t
;;     :config
;;     (bind-key "C-x C-d" 'helm-browse-project)
;;     (bind-key "C-x C-g" 'helm-ls-git-ls)
;;     (define-key helm-command-map (kbd "l") 'helm-ls-git-ls)))


