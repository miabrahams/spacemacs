;;; packages.el --- shellconfig Layer packages File for Spacemacs
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

(setq-default shellconfig-packages
  '(
    ;; package shellconfigs go here

    ;; with-editor
    shell-switcher
    bash-completion


    ;; Convert to lazy load?
    ;; esh-buf-stack
    ;; powershell
    ;; eshell-prompt-extras
    ;; esh-help

    )
  )


(defvar shellconfig-excluded-packages '()
  "List of packages to exclude.")



;; (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
;; (defun shellconfig/init-esh-help ()
;;   (use-package "esh-help" :ensure t
;;     :config
;;     (setup-esh-help-eldoc)))

(defun shellconfig/init-shell-switcher ()
  (use-package "shell-switcher" :ensure t
    :defer t
    :config
    (defun make-new-shell-buffer ()
      "Create a new `shell'.  Not ready for Eshell just yet."
      (interactive)
      (if (spacemacs/system-is-mswindows)
          (powershell (generate-new-buffer-name "*PowerShell*"))
          (shell (generate-new-buffer-name "*Shell*"))))
    (setq-default shell-switcher-new-shell-function
                  'make-new-shell-buffer)
    (global-set-key (kbd "<f5>") #'shell-switcher-switch-buffer)
    ;; (global-set-key (kbd "<f2>") #'shell-switcher-new-shell)
    ))

;; (defun shellconfig/init-with-editor ()
;;   (use-package "with-editor"
;;     :ensure t
;;     :defines (with-editor-async-shell-command with-editor-shell-command)
;;     :config
;;     (define-key (current-global-map)
;;       [remap async-shell-command] #'with-editor-async-shell-command)
;;     (define-key (current-global-map)
;;       [remap shell-command] #'with-editor-shell-command)))

(defun shellconfig/init-bash-completion ()
  (use-package "bash-completion"
    :ensure t
    :defines 'bash-completion-dynamic-complete
    :config
    (add-hook 'shell-dynamic-complete-functions
              'bash-completion-dynamic-complete)))

;; (defun shellconfig/init-eshell-prompt-extras ()
;;   (use-package "eshell-prompt-extras" :ensure t
;;     :config
;;     (setq-default eshell-highlight-prompt nil
;;                   eshell-prompt-function 'epe-theme-lambda)))

;; (defun shellconfig/init-pcomplete-extension ()
;;   (use-package "pcomplete-extension" :ensure t))
