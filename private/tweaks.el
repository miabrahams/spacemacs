;; tweaks.el --- to load after everything else

;; Copyright (C) 2012-2015 Michael Abrahams

;; Author: miabraha@gmail.com


;;; Commentary:
;; put code to be executed after Spacemacs setup has loaded below.
;;

;;; Code:



;; Defaults moved from customize
(setq-default apropos-do-all t
              apropos-sort-by-scores 1
              auto-compression-mode t
              bs-default-configuration "files-and-scratch"
              c-basic-offset 4
              c-doc-comment-style '((c-mode . gtkdoc)
                                    (c++-mode . javadoc)
                                    (java-mode . javadoc)
                                    (pike-mode . autodoc))
              compile-command "nn"
              custom-buffer-done-kill t
              delete-by-moving-to-trash t
              delete-selection-mode 1
              delete-selection-mode t
              diary-file (concat dropbox-directory "text/diary")
              edit-server-edit-mode-hook '(edit-server-edit-mode-set-explicitly reset-font)
              eldoc-minor-mode-string ""
              evil-normal-state-cursor '(box "red")
              evil-search-highlight-persist nil
              expand-region-skip-whitespace nil
              global-aggressive-indent-mode nil
              global-auto-revert-ignore-modes (quote (ninja-mode))
              global-hl-line-mode -1
              helm-move-to-line-cycle-in-source t
              help-window-select t
              hs-hide-comments-when-hiding-all nil
              hungry-delete-mode t
              ido-use-virtual-buffers t
              magit-repo-dirs '("~/repos/")
              minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
              mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(1 ((shift) . 1))
              package-enable-at-startup nil
              pcomplete-autolist t
              projectile-indexing-method 'alien
              protect-buffer-bury-p nil
              recentf-max-menu-items 100
              recentf-max-saved-items 200
              recentf-mode t
              recentf-save-file "~/.emacs.d/.cache/recentf"
              rm-text-properties '(("") ("\\` Ovwrt\\'" 'face 'font-lock-warning-face))
              rtags-completions-enabled 1
              server-window 'pop-to-buffer
              shell-cd-regexp "(cd|ls|ll)"
              shell-completion-execonly nil
              show-paren-mode 1
              sp-autoinsert-pair t
              text-mode-hook (quote (text-mode-hook-identify))
              tool-bar-mode nil
              tramp-default-method "ssh"
              undo-tree-auto-save-history nil
              undo-tree-mode-lighter " ΰ"
              warning-suppress-types (quote (undo discard-info))

              sml/replacer-regexp-list '(("^~/org/" ":Org:")
                                         ("^/sudo:.*:" ":SU:")
                                         ("^~/Documents/" ":Doc:")
                                         ("^~/Dropbox/" ":DB:")
                                         ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
                                         ("^~/[Gg]it/" ":Git:")
                                         ("^~/[Gg]it[Hh]ub/" ":Git:")
                                         ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:"))

              custom-safe-themes '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
                                   "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
                                   "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
                                   "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
                                   "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26"
                                   "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d"
                                   "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" default)

              bs-configurations '(("all" nil nil nil nil nil)
                                  ("files" nil nil nil
                                   bs-visits-non-file bs-sort-buffer-interns-are-last)
                                  ("files-and-scratch"
                                   "^\\*\\(scratch\\|powershell\\)\\*$" nil nil
                                   bs-visits-non-file bs-sort-buffer-interns-are-last)
                                  ("all-intern-last"
                                   nil nil nil nil bs-sort-buffer-interns-are-last)
                                  ("files-and-not-customize"
                                   "^\\*scratch\\*$" nil "Customize" nil
                                   bs-sort-buffer-interns-are-last)))

;; Windows specific config.
;; To use Tramp: add macrobat.net with id_rsa_putty.ppk in putty.exe.
;; Then use M-x find-file /plink:abrahams@macrobat.net: to connect.
(if (spacemacs/system-is-mswindows)
    (setq-default
     ggtags-executable-directory "c:\\bin\\global\\bin"
     company-cmake-executable "C:/Program Files (x86)/CMake_3.2.1/bin/cmake.exe"
     tramp-default-method "plink"
     tramp-default-user "abrahams"
     tramp-default-host "macrobat.net"
     tramp-auto-save-directory "~/.emacs.d/backups"
     )
  (exec-path-from-shell-initialize))


;; Coding tweaks
(add-hook 'prog-mode-hook 'subword-mode)
(defvar compile-command "nn" "Default compile command for interactive shell.")


;; Prettify-symbols
(setq prettify-symbols-alist
      '(("lambda" . 955)  ; λ
        ("->" . 8594)     ; →
        ("=>" . 8658)     ; ⇒
        ("map" . 8614)    ; ↦
        ))
(add-to-list 'lisp-mode-hook (lambda () (prettify-symbols-mode)))
(add-to-list 'emacs-lisp-mode-hook (lambda () (prettify-symbols-mode) (setq mode-name "λ")))


(spacemacs|define-custom-layout "krita"
  :binding "k"
  :body
  (let ((krita-path (if (spacemacs/system-is-mswindows)
                        "r:/src/krita/"
                      "~/code/Krita/code/")))
    (find-file (concat krita-path "CMakeLists.txt"))
    (split-window-right)
    (find-file (concat krita-path "krita/main.cc"))))




;; Toggle-frame-dedication stuff
(modify-all-frames-parameters '((windows-always-dedicated . nil)))
(defun toggle-frame-windows-dedicated ()
  "Toggle whether all windows in the current frame will be dedicated."
  (interactive)
  (let  ((frame-dedicated
          (frame-parameter nil 'windows-always-dedicated)))
    (set-frame-parameter nil 'windows-always-dedicated
                         (not frame-dedicated))
    (dolist (w (window-list)) (apply-frame-dedication w))))

(defun apply-frame-dedication (&optional w)
  "Apply window-dedicated status to window W based on the frame configuration parameter.  If w is nil, apply to selected window."
  (let  ((frame-was-dedicated (frame-parameter nil 'windows-always-dedicated)))
    (set-window-dedicated-p (if w w (selected-window))
                            (if frame-was-dedicated 1 nil))
    ))

(add-hook 'window-configuration-change-hook 'apply-frame-dedication nil t)
(evil-leader/set-key "wD" #'toggle-frame-windows-dedicated)

;; For debugging Krita
(setenv "XDG_DATA_DIRS" "/usr/share/plasma:/usr/local/share/:/usr/share/:/home/abrahams/share/")

;; Auto-recipient.  If I have entered a recipient in the last three minutes, auto-insert it.
(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous
         (save-excursion
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                    (search-forward-regexp (concat "^[^<]*<" erc-nick ">"
                                                   " *\\([^:]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and
           previous
           (> 180 (time-to-seconds
                   (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))



(provide 'tweaks)


;; (use-package "help+" :ensure t)
;; (use-package "help-fns+" :ensure t)
;; (use-package "swiper" :ensure t)
;; (use-package "ace-window"
;; :ensure t
;; :config
;; (ace-window-display-mode)
;; (global-set-key (kbd "C-,") 'ace-window)


;;; tweaks.el ends here
