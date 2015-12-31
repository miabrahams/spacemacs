;;; packages.el --- my-config Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-config-packages
    '(
      ;; package my-configs go here
      ;; company
      ;; irony
      ;; company-irony
      ;; flycheck-irony
      ;; rtags
      ;; with-editor
      ;; ggtags
      ))

;; List of packages to exclude.
(setq my-config-excluded-packages
      '(
        ;; company-clang
        helm-company
        ))


;; (defun my-config/init-irony ()
;;   "init"
;;   (use-package "irony"
;;     :ensure t
;;     :commands (irony-mode)
;;     :defines (irony-mode-map)
;;     :config
;;     (defun my-irony-mode-hook ()
;;       (define-key irony-mode-map [remap completion-at-point]
;;         'irony-completion-at-point-async)
;;       (define-key irony-mode-map [remap complete-symbol]
;;         'irony-completion-at-point-async))
;;     (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)
;;     (add-hook 'objc-mode-hook 'irony-mode)))

;; (defun my-config/init-company ()
;;   "There should be no need for this function"
;;   (use-package "company" :ensure t))

;; (defun my-config/init-company-irony ()
;;   "Initialize] my package"
;;   (use-package "company-irony"
;;     :ensure t
;;     :commands (company-irony company-irony-setup-begin-commands)
;;     :config
;;     (spacemacs|use-package-add-hook 'company
;;       :post-init
;;       (add-to-list 'company-backends 'company-irony)
;;       (define-key company-active-map (kbd "M-\[") 'company-select-next)
;;       (define-key company-active-map (kbd "M-\]") 'company-select-previous)
;;       (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))))



;; (defun my-config/init-flycheck-irony ()
;;   "Initialize my package"
;;   (use-package "flycheck-irony"
;;     :ensure t
;;     :commands (flycheck-irony-setup)
;;     :config
;;     (spacemacs|use-package-add-hook 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; (defun my-config/init-ggtags ()
;;   (use-package "ggtags"
;;     :ensure t
;;     :config
;;     (add-hook 'c-mode-common-hook
;;               (lambda ()
;;                 (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                   (ggtags-mode 1))))))


;; (defun spacemacs/helm-gtags-define-keys-for-mode (mode)
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


;; For each package, define a function my-config/init-<package-my-config>
;;
;; (defun my-config/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
