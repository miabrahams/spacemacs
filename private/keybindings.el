;;; keybindings.el --- Defines keybindings
;;; Commentary:
;;; Code:


;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;;
;;            DO NOT CHANGE THIS FILE WITHOUT TESTING              ;;
;;            M-X BYTE-COMPILE-FILE AFTER ANY CHANGES              ;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;;

;; Global insanely important things
(global-set-key (kbd "M-n")           #'forward-paragraph)
(global-set-key (kbd "M-p")           #'backward-paragraph)
(global-set-key (kbd "C-M-SPC")       #'cycle-spacing)
(global-set-key (kbd "C-x k")         #'volatile-kill-buffer)
(global-set-key (kbd "C-M-c")         #'spacemacs/save-buffers-kill-emacs)
(global-set-key (kbd "<C-backspace>") #'backward-kill-word)
(global-set-key (kbd "<C-delete>")    #'kill-word)
(global-set-key (kbd "M-X")           #' execute-extended-command)
(global-set-key (kbd "C-x f")         #'ido-find-file)
(global-set-key (kbd "<S-return>")    #'smart-open-line)
(global-set-key (kbd "M-k")           #'kill-whole-line)
(global-set-key (kbd "<C-delete>")    #'kill-word)
(global-set-key (kbd "C-M-1")         #'eval-sexp-fu-eval-sexp-inner-list)
(global-set-key (kbd "C-M-2")         #'mark-sexp)
(global-set-key (kbd "C-M-3")         #'anzu-query-replace-regexp)
(global-set-key (kbd "M-\#")          #'anzu-query-replace)
(global-set-key (kbd "<C-backspace>") #'backward-kill-word)
(global-set-key (kbd "<S-tab>")       #'indent-rigidly-left)
(global-set-key (kbd "<C-tab>")       #'spacemacs/previous-useful-buffer)
(global-set-key (kbd "<C-iso-lefttab>") #'spacemacs/next-useful-buffer)
(define-key prog-mode-map (kbd "C-M-;") #'comment-or-uncomment-sexp) ; New: use it!
(global-set-key [(shift control tab)] (kbd "<C-iso-lefttab>")) ;For linux/windows
(evil-leader/set-key "SPC" #'helm-omni)
(evil-leader/set-key-for-mode 'org-mode "a" nil)
(global-set-key (kbd "C-;")           #'evil-normal-state)

;; Wtf?  Note: back-button bindings are defined in `custom-set-variables'
(global-set-key (kbd "f")         #'self-insert-command)   ;; WTF???

(global-set-key (kbd "<C-wheel-up>") 'mwheel-scroll)
(global-set-key (kbd "<C-wheel-down>") 'mwheel-scroll)
;; Unbound things that could use a good owner
;; (global-set-key (kbd "C-\\")  #'???)
;; (global-set-key [(meta control \;)] ')
;; (global-set-key [(meta l \;)] ')
;; (global-set-key [(meta backspace)] 'back-mark)
;; (global-set-key (kbd "M-/") ')
;; (global-set-key [(control \{)] ')
;; (global-set-key [(control \})] ')
;; C-o/M-o ... figure out if it's worth having a dedicated open line command
;; (global-set-key (kbd "m-u") help-map)
;; (global-set-key (kbd "M-l") 'kill-line)
;; (global-set-key (kbd "C-=") #'ace-window)
;; (global-set-key [(meta right)] 'windmove-right)
;; (global-set-key [(meta left)] 'windmove-left)
;; (global-set-key [(meta up)] 'windmove-up)
;; (global-set-key [(meta down)] 'windmove-down)
;; (global-set-key (kbd "M-z") #'zop-up-to-char)
;; (bind-key "M-k" ')




;; Global less insanely important things
(global-set-key (kbd "C-M-s")     #'swiper)
(evil-leader/set-key "pp"         #'helm-projectile-switch-project)
(evil-leader/set-key "asc"        #'spacemacs/comint-clear-buffer)
(evil-leader/set-key "xax"        #'align-regexp)
(evil-leader/set-key "xc"         #'whitespace-cleanup)
(global-set-key (kbd "C-x C-v")   #'ff-find-related-file)  ;; For coding
(global-set-key (kbd "<M-pause>") #'profiler-dwim)
(global-set-key (kbd "M-\\")      #'kill-whitespace)
(global-set-key (kbd "C-j")       #'join-line-or-lines-in-region)

;; Break off training wheels
(global-set-key (kbd "C-x C-f")  nil) ;#'helm-for-files)
(global-set-key (kbd "C-x f")    nil) ;#'helm-find-files)
(global-set-key (kbd "C-x C-b")  nil) ;#'helm-mini)
(global-set-key (kbd "C-x C-r")  nil) ;#'helm-recentf)
(global-set-key (kbd "C-x b")    nil) ;#'helm-mini)
(evil-leader/set-key "fF" #'helm-for-files)
(evil-leader/set-key "fo" #'helm-for-files)

;; Smartparens
(global-set-key (kbd "C-M-f") #'sp-forward-sexp)
(global-set-key (kbd "C-M-b") #'sp-backward-sexp)
(global-set-key (kbd "C-M-d") #'sp-down-sexp)
(global-set-key (kbd "C-M-u") #'sp-backward-up-sexp)
(global-set-key (kbd "C-M-n") #'sp-next-sexp)
(global-set-key (kbd "C-M-p") #'sp-previous-sexp)
(global-set-key (kbd "C-M-w") #'sp-kill-sexp)
(global-set-key (kbd "<M-backspace>") #'sp-backward-unwrap-sexp)
(global-set-key (kbd "<C-M-backspace>") #'sp-unwrap-sexp)

;; Defined in functions.el
(global-set-key (kbd "C-x |") #'goto-nextframe)
(global-set-key (kbd "C-x \\") #'window-toggle-split-direction)
(global-set-key (kbd "C-a")   #'my/smarter-move-beginning-of-line)
(global-set-key (kbd "C-x 2") #'my/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") #'my/hsplit-last-buffer)
(global-set-key (kbd "C-a")   #'my/smarter-move-beginning-of-line)
(global-set-key (kbd "C-`") #'push-mark-no-activate)
(global-set-key [remap exchange-point-and-mark] #'exchange-point-and-mark-no-activate)
(global-set-key [remap goto-line] #'goto-line-with-feedback)
(global-set-key [remap eval-defun] #'eval-region-or-defun)




;; Extra help bindings
(define-key help-map (kbd "C-f") #'find-function)
(define-key help-map (kbd "?") #'howdoi-query)
(define-key help-map (kbd "A") (lambda () (interactive) (apropos (thing-at-point 'symbol) t)))
(define-key help-map (kbd "p") #'spacemacs/describe-package)




;; Define a mode using two A's to override annoying major modes.
(defvar aan-excellent-mode-map (make-keymap))
(define-key aan-excellent-mode-map (kbd "M--")   #'hs-hide-level)
(define-key aan-excellent-mode-map (kbd "M-+")   #'hs-show-all)
(define-key aan-excellent-mode-map (kbd "M-/")   #'evil-toggle-fold)
(define-key aan-excellent-mode-map (kbd "C-M--") #'evil-close-folds) ; Save M-_ for "Redo""
(define-key aan-excellent-mode-map (kbd "C-M-=") #'evil-open-folds)
(define-key aan-excellent-mode-map (kbd "M-\;")  #'comment-dwim-2)
;; "hideshow" also defines a fold command C-M-/
(define-minor-mode aan-excellent-mode
  "A minor mode so that my key settings override annoying major modes."
  t "" 'aan-excellent-mode-map)
(aan-excellent-mode 1)

(defvar my-hs-hide nil "Current state of hideshow for toggling all.")
(defun toggle-hs-all () "Toggle hideshow all."
       (interactive)
       (setq my-hs-hide (not my-hs-hide))
       (if my-hs-hide
	   (hs-hide-all)
	 (hs-show-all)))
(define-key aan-excellent-mode-map (kbd "C-M-/") #'toggle-hs-all) ; evil-toggle uses hideshow anyway



;; No stupid mouse crap like secondary selection. Foo!
(global-set-key [remap mouse-drag-secondary] 'mouse-drag-region)
(global-set-key [remap mouse-set-secondary] 'mouse-set-region)
(global-set-key [remap mouse-start-secondary] 'mouse-set-point)
(global-set-key [remap mouse-kill-secondary] 'mouse-kill)
(global-set-key [remap mouse-yank-secondary] 'mouse-yank-primary)
(global-set-key [remap mouse-secondary-save-then-kill] 'mouse-save-then-kill)
;; (define-key Info-mode-map (kbd "<mouse-4>") nil)
;; (define-key Info-mode-map (kbd "<mouse-5>") nil)



;; F-keys.
;; (global-set-key (kbd "<f1>")   #'org-agenda-list)
(global-set-key (kbd "<f2>")   #'org-journal-new-entry)
;; (global-set-key (kbd "<f7>")   (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "<f8>")   (new-send-cmd compile-command))
(global-set-key (kbd "<S-f8>") (new-send-cmd (string-join (list compile-command " install"))))
(global-set-key (kbd "<f9>")   (new-send-cmd "krita"))
(global-set-key (kbd "<S-f9>") (if (spacemacs/system-is-linux)
                                    #'gdb
                                 (new-send-cmd "windbg -g krita")))
(global-set-key (kbd "<f10>")   #'magit-status)
(global-set-key (kbd "<S-f10>") #'magit-status-target)
(global-set-key (kbd "<f12>")   #'customize-group)




;; Buffer-move bindings - VERY GOOD THINGS FOUND HERE
(global-set-key (kbd "<C-S-up>")     #'buf-move-up)
(global-set-key (kbd "<C-S-down>")   #'buf-move-down)
(global-set-key (kbd "<C-S-left>")   #'buf-move-left)
(global-set-key (kbd "<C-S-right>")  #'buf-move-right)



;; Helm shizz -- consider moving this to helm-covenant/config.el?
(global-set-key (kbd "C-x i")    #'helm-semantic-or-imenu)
(global-set-key (kbd "M-y")      #'helm-show-kill-ring)
(global-set-key (kbd "M-x")      #'helm-M-x)
(global-set-key (kbd "C-x m")    #'helm-semantic-or-imenu)
(global-set-key (kbd "M-y")      #'helm-show-kill-ring)
(define-key minibuffer-local-map (kbd "C-c C-l") #'helm-minibuffer-history)


;; Bindings which need to be lazy-loaded
(spacemacs|use-package-add-hook "expand-region"
  :post-init
  (global-set-key (kbd "C-'") #'er/expand-region)
  :post-config
  (append er/try-expand-list '(mark-paragraph)))
(spacemacs|use-package-add-hook "flycheck"
  :post-config
  (define-key prog-mode-map (kbd "C-c n") #'flycheck-next-error)
  (define-key prog-mode-map (kbd "C-c p") #'flycheck-previous-error))
(spacemacs|use-package-add-hook "org-bullets"
  :post-config
  (setq-default org-bullets-bullet-map
    '(keymap
      (double-down-mouse-1 . org-cycle)
      (mouse-2 . (lambda (e)
                   (interactive "e")
                   (mouse-set-point e)
                   (org-cycle))))))
(spacemacs|use-package-add-hook "eval-sexp-fu"
  :post-config
  (global-set-key (kbd "C-M-x") #'eval-sexp-fu-eval-sexp-inner-list))
(spacemacs|use-package-add-hook "company"
  :post-init
  (define-key company-active-map (kbd "M-\[") #'company-select-next)
  (define-key company-active-map (kbd "M-\]") #'company-select-previous))



;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;;
;;            M-X BYTE-COMPILE-FILE AFTER ANY CHANGES              ;;
;;            I SWEAR TO GOD JUST TRY ME MOTHERFUCKER              ;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%;;


(provide 'keybindings)
;;; keybindings.el ends here
