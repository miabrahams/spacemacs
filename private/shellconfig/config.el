;;; config.el --- Git Layer configuration File for Spacemacs
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

;; Variables

;;; Code:

;; (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
;; (add-hook 'shell-mode-hook #'compilation-shell-minor-mode)
;; (add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
;; (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(setq-default comint-buffer-maximum-size 20000
              comint-completion-addsuffix t
              comint-input-ignoredups t
              comint-input-ring-size 5000
              comint-move-point-for-output nil
              comint-process-echoes nil
              comint-prompt-read-only t
              comint-scroll-show-maximum-output t
              comint-scroll-to-bottom-on-input t)

;; (setq-default powershell-prompt-regex  "PS [^#$%>]+>")
(setq-default powershell-prompt-regex "@{\\(.*\\)}.*» \\n")
(add-to-list 'process-coding-system-alist '("[Pp]owershell" cp437 . cp437))

(setq powershell-prompt-regex "@{\\(.*\\)}.*» \\n\\n")


;; (add-to-load-path "~/.emacs.d/private/")
(with-demoted-errors "powershell.el failed to load: %s" (load "~/.emacs.d/private/powershell.el"))
(defun powershell/define-text-objects ()
  (spacemacs|define-text-object "$" "dollarparen" "$(" ")"))
(add-hook 'powershell-mode-hook 'powershell/define-text-objects)
;; (evil-leader/set-key "asp" 'powershell)
;; (evil-leader/set-key-for-mode 'powershell-mode "mrr" 'powershell-regexp-to-regex)


;; Keybindings
(add-hook 'eshell-mode-hook
          (lambda ()
             (local-set-key [remap eshell-pcomplete] #'helm-esh-pcomplete)
             ;; (local-set-key (kbd "M-p") #'spacemacs/helm-eshell-history)
             ))

(add-hook 'shell-mode-hook
          (lambda ()
            (compilation-shell-minor-mode)
            (local-set-key (kbd "C-c C-l") #'helm-comint-input-ring)
            ;; (lets-have-company)
            (ansi-color-for-comint-mode-on)
            (local-set-key (kbd "TAB") #'company-manual-begin)))

(add-hook 'shell-mode-hook #'company-mode)


(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (if (spacemacs/system-is-mswindows)
      ;; (setq-default dirtrack-list (list "^PS \\([a-zA-Z]:.*\\)>" 1))
      (setq-default dirtrack-list (list "@{\\(.*\\)}.*».*" 1))
    (setq-default dirtrack-list (list "abrahams@ARIES:\\(.*\\)%"  1)))
    ;; (setq-default dirtrack-list (list ".*in \\[33m\\(.*\\)\\[00m.*" 1)))
  (shell-dirtrack-mode nil)
  (dirtrack-mode 1))

(add-hook 'shell-mode-hook 'my-dirtrack-mode)
;; (setq-default w32-pipe-read-delay 50)

(defun comint-testing-wtf ()
  "hi"
  (message "Completion testing")
  )

(defun my-powershell-mode-hook ()
  "Testing"
  (setq-local comint-dynamic-complete-functions
              '(comint-testing-wtf
                comint-replace-by-expanded-history
                shell-dynamic-complete-environment-variable
                shell-dynamic-complete-command
                shell-replace-by-expanded-directory
                comint-dynamic-complete-filename))
  )

(add-hook 'powershell-mode-hook 'my-powershell-mode-hook)

;; (use-package "auto-complete" :ensure t)
;; (use-package "readline-complete" :ensure t
;; For customization, see the docstrings of `rlc-timeout`, `rlc-attempts`, and `ac-rlc-prompts`.
;; :config
;; (add-to-list 'ac-modes 'shell-mode)
;; (add-hook 'shell-mode-hook 'ac-rlc-setup-sources)
;; )
;; M-x company-complete
;; This should be incorporated directly into Powershell.el
;; (setq process-coding-system-alist (cons (cons "powershell" (cons 'utf-8 'utf-8)) process-coding-system-alist))
;; (add-hook 'prog-mode-hook 'lets-have-company)
;;**** Git Completion
;; (defun pcmpl-git-commands ()
;;   "Return the most common git commands by parsing the git output."
;;   (with-temp-buffer
;;     (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
;;     (goto-char 0)
;; ;    (search-forward "available git commands in")
;;     (let (commands)
;;       (while (re-search-forward
;; 	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
;; 	      nil t)
;; 	(push (match-string 1) commands)
;; 	(when (match-string 2)
;; 	  (push (match-string 2) commands)))
;;       (sort commands #'string<))))
;; (defconst pcmpl-git-commands (pcmpl-git-commands)
;;   "List of `git' commands.")
;; (defvar pcmpl-git-ref-list-cmd
;;   "git for-each-ref refs/ --format='%(refname)'"
;;   "The `git' command to run to get a list of refs.")
;; (defun pcmpl-git-get-refs (type)
;;   "Return a list of `git' refs filtered by TYPE."
;;   (with-temp-buffer
;;     (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
;;     (goto-char (point-min))
;;     (let (refs)
;;       (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
;; 	(push (match-string 1) refs))
;;       (nreverse refs))))
;; (defun pcmpl-git-remotes ()
;;   "Return a list of remote repositories."
;;   (split-string (shell-command-to-string "git remote")))
;; (defun pcomplete/git ()
;;   "Completion for `git'."
;;   ;; Completion for the command argument.
;;   (pcomplete-here* pcmpl-git-commands)
;;   (cond
;;    ((pcomplete-match "help" 1)
;;     (pcomplete-here* pcmpl-git-commands))
;;    ((pcomplete-match (regexp-opt '("pull" "push")) 1)
;;     (pcomplete-here (pcmpl-git-remotes)))
;;    ;; provide branch completion for the command `checkout'.
;;    ((pcomplete-match "checkout" 1)
;;     (pcomplete-here* (append (pcmpl-git-get-refs "heads")
;; 			     (pcmpl-git-get-refs "tags"))))
;;    (while (pcomplete-here (pcomplete-entries)))))
