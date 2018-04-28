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
                systemd
                ;; save-visited-files
                ;; cmake-project
                ;; dired+
                ;; avy-zap
                ;; cmake-ide
                ;; cmake-font-lock
                comment-dwim-2
                howdoi
                ;; help-mode+
                olivetti
                ;; org2blog
                ;; rtags
                ;; synonyms
                persistent-scratch
                ;; julia-mode
                ;; julia-shell
                ))


;; List of packages to exclude.
(setq-default my-config-excluded-packages
              '(
                ;; company-clang
                helm-company
                ))

(defun my-config/init-ninja-mode ()
  (use-package "ninja-mode"
    :defer t
    :commands 'ninja-mode
    ))

(defun my-config/init-profiler ()
  (use-package "profiler" :defer t))

(defun my-config/init-systemd ()
  (use-package "systemd"
    :defer t
    :commands 'systemd-mode))


;; (defun my-config/init-save-visited-files ()
;;   (use-package "save-visited-files"
;;     :ensure t
;;     :config
;;     (turn-on-save-visited-files-mode)))


(defun my-config/init-cmake-project ()
  (use-package "cmake-project" :ensure t
    :defer t
    :config
    (defun maybe-cmake-project-hook ()
      (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
    (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
    (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)))

(defun my-config/init-avy-zap ()
  (use-package "avy-zap"
    :ensure t
    :commands 'avy-zap-up-to-char-dwim
    :init
    (global-set-key (kbd "M-z") #'avy-zap-up-to-char-dwim)))

(defun my-config/init-comment-dwim-2 ()
  (use-package "comment-dwim-2"
    :ensure t
    :commands 'comment-dwim-2
    :config
    (global-set-key (kbd "M-;") 'comment-dwim-2)))

(defun my-config/init-howdoi ()
  (use-package "howdoi"
    :ensure t
    :commands 'howdoi-query
    :config
    (define-key help-map (kbd "?") 'howdoi-query)))

(defun my-config/init-persistent-scratch ()
  (use-package "persistent-scratch"
    :ensure t
    :init
    (setq-default persistent-scratch-save-file "~/.emacs.d/.cache/.persistent-scratch")
    (persistent-scratch-setup-default)
    ))


(defun my-config/init-olivetti ()
  (use-package "olivetti"
    :ensure t
    :commands (olivetti-mode)
    ))


;; (defun my-config/init-julia-shell ()
;;   (use-package julia-shell
;;     :ensure t
;;     :defer t))

;; (defun my-config/init-julia-mode ()
;;   (use-package julia-mode
;;     :ensure t
;;     :defer t
;;     :config
;;     (defun my-julia-mode-hooks ()
;;       (require 'julia-shell-mode))
;;     (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;;     (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;;     (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)
;;     ))





;; (defun my-config/init-windresize ()
;;   (use-package "windresize"
;;     :ensure t
;;     :commands (windresize)
;;     :init
;;     (global-set-key (kbd "C-x ^") 'windresize)
;;     ))



;; (defun my-config/init-org2blog ()
;;   (use-package "org2blog"
;;     :ensure t
;;     :commands (org2blog/wp-login)
;;     :init
;;     (setq org2blog/wp-blog-alist
;;           '(("Risk and Time"
;;              :url "http://riskandtime.net/xmlrpc.php"
;;              :username "abrahams"
;;              :password "WRlecS%FVNDQ%HT67p"
;;              :default-title "New Post"
;;              :default-categories ("Finance")
;;              :tags-as-categories nil)
;;             ))
;;     ))



;; (defun my-config/init-rtags ()
;;   (if ( spacemacs/system-is-linux )
;;       (use-package "rtags"
;;         :ensure t
;;         :commands 'rtags-mode
;;         :init
;;         (setq-default rtags-autostart-diagnostics t
;;                       rtags-completions-enabled t)
;;         (defun my-rtags-setup ()
;;           (flycheck-select-checker 'rtags)
;;           (rtags-mode))
;;         (add-hook 'c-mode-common-hook #'my-rtags-setup)
;;         :config
;;         ;; c-mode-common-hook is also called by c++-mode
;;         (push 'company-rtags company-backends)
;;         (define-key c-mode-base-map (kbd "M-.")   #'rtags-find-symbol-at-point)
;;         (define-key c-mode-base-map (kbd "M-,")   #'rtags-find-references-at-point)
;;         (define-key c-mode-base-map (kbd "C-M-.") #'rtags-find-file)
;;         (define-key c-mode-base-map (kbd "C-.")   #'rtags-find-symbol)
;;         (define-key c-mode-base-map (kbd "C-,")   #'rtags-find-references)
;;         (define-key c-mode-base-map (kbd "C-<")   #'rtags-find-virtuals-at-point)
;;         (define-key c-mode-base-map (kbd "C-M-;") #'rtags-imenu)
;;         (define-key c-mode-base-map (kbd "M-'")   #'company-complete)
;;         ;; THIS IS AN AMAZING BUFFER, PLEASE GIVE IT LOVE
;;         (define-key c-mode-base-map (kbd "C-M-?") #'rtags-diagnostics)
;;         )))




;; (defun my-config/init-synonyms ()
;;   (use-package "synonyms"
;;     :ensure t
;;     :commands 'synonyms
;;     :config
;;     (setq synonyms-file        "~/private/etc/mthesaur.txt")
;;     (setq synonyms-cache-file "~/private/etc/synonyms.cache")
;;     ;; Define a key here later
;;     ))


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
