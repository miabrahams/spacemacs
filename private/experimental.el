;;; experimental.el --- new and exciting changes

;; Copyright (C) 2012-2015 Michael Abrahams

;; Author: miabraha@gmail.com


;;; Commentary:
;; Put volatile, unsorted and untested code below.
;;

;;; Code:



;; Basic default bookmarks: have to be synced by hand unfortunately!
;; ;;;; Emacs Bookmark Format Version 1 ;;;;
;; ;;; This format is meant to be slightly human-readable;
;; ;;; nevertheless, you probably don't want to edit it.
;; ;;; -*- End Of Bookmark File Format Version Stamp -*-
;; (("Notes"
;;   (filename . "c:/Dropbox/text/notes.org")
;;   (front-context-string . "* Messages      ")
;;   (rear-context-string)
;;   (position . 1))
;;  ("Artwork"
;;   (filename . "c:/Dropbox/text/creative/artwork.org")
;;   (front-context-string . "* Model to-do li")
;;   (rear-context-string)
;;   (position . 1))
;;  ("Concepts"
;;   (filename . "c:/Dropbox/text/creative/concepts.org")
;;   (front-context-string . "* Uncategorized\n")
;;   (rear-context-string)
;;   (position . 1))
;;  ("Todo"
;;   (filename . "C:/Dropbox/text/org/todo.org")
;;   (front-context-string . "* Todo\n\n[ These ")
;;   (rear-context-string)
;;   (position . 1))
;;  ("Projects"
;;   (filename . "C:/Dropbox/text/projects.org")
;;   (front-context-string . "\n\n\n* Ry and Sama")
;;   (rear-context-string)
;;   (position . 10))
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random shit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some symbols for mode lines
;; ⇛⤴⇪⇖↻↳↟⇅

;; (define-key dired-mode-map "c" 'find-file)

;; ;; MU4E
;; (when (and nil (spacemacs/system-is-linux))
;;   (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;   (use-package "mu4e"
;;     :commands 'mu4e
;;     :init
;;     (progn
;;       (setq-default mu4e-maildir       "~/Maildir/aerys"
;;                     mu4e-drafts-folder "/[Gmail].Drafts"
;;                     mu4e-sent-folder   "/[Gmail].Sent Mail"
;;                     mu4e-trash-folder  "/[Gmail].Trash"
;;                     mu4e-refile-folder "/[Gmail].All Mail"
;;                     ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
;;                     mu4e-sent-messages-behavior 'delete
;;                     ;; allow for updating mail using 'U' in the main view:
;;                     mu4e-get-mail-command "offlineimap"
;;                     ;; don't keep message buffers around
;;                     message-kill-buffer-on-exit t
;;                     ;; Personal information
;;                     user-mail-address "aerys.bat@gmail.com"
;;                     user-full-name  "Aerys Bat"
;;                     mu4e-headers-skip-duplicates t
;;                     mu4e-html2text-command 'mu4e-shr2text
;;                     ;;mu4e-compose-signature (concat "Foo X. Bar\n"  "http://www.example.com\n")
;;                     ;; make sure the gnutls command line utils are installed
;;                     ;; package 'gnutls-bin' in Debian/Ubuntu
;;                     send-mail-function 'smtpmail-send-it
;;                     smtpmail-stream-type 'starttls
;;                     smtpmail-default-smtp-server "smtp.gmail.com"
;;                     smtpmail-smtp-server "smtp.gmail.com"
;;                     smtpmail-smtp-service 587
;;                     mu4e-use-fancy-chars t
;;                     mu4e-view-show-images t
;;                     )
;;       (when (fboundp 'imagemagick-register-types)
;;         (imagemagick-register-types))
;;       ;; setup some handy shortcuts
;;       ;; you can quickly switch to your Inbox -- press ``ji''
;;       ;; then, when you want archive some messages, move them to
;;       ;; the 'All Mail' folder by pressing ``ma''.
;;       (setq-default mu4e-maildir-shortcuts
;;                     '( ("/INBOX"               . ?i)
;;                        ("/[Gmail].Sent Mail"   . ?s)
;;                        ("/[Gmail].Trash"       . ?t)
;;                        ("/[Gmail].All Mail"    . ?a)))
;;       ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;;       ;; additional non-Gmail addresses and want assign them different
;;       ;; behavior.)
;;       ;; fetch mail
;;       (setq-default  mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
;;                      mu4e-update-interval 300)             ;; update every 5 minutes
;;       ;; if our mail server lives at smtp.example.org; if you have a local
;;       ;; mail-server, simply use 'localhost' here.
;;       (setq-default smtpmail-smtp-server "smtp.gmail.com")
;;       (add-hook 'mu4e-view-mode-hook
;;                 (lambda()
;;                   ;; try to emulate some of the eww key-bindings
;;                   (local-set-key (kbd "<tab>") 'shr-next-link)
;;                   (local-set-key (kbd "<backtab>") 'shr-previous-link)))
;;       )
;;     :config
;;     (require 'mu4e-contrib)
;;     )
;;   )


;; (use-package "pomodoro"
;;   :ensure t
;;   :commands (pomodoro-start pomodoro-stop pomodoro-resume pomodoro-pause)
;;   :init
;;   (setq-default pomodoro-work-cycle "⏰"
;;                 pomodoro-break-cycle "♫"
;;                 pomodoro-play-sounds nil)
;;   ;; Pomodoro: this would be better as a minor-mode.  See projectile.el and perhaps submit pull request.
;;   ;; Also would be nice to add a start/stop function.
;;   (defvar pomodoro-keymap-prefix (kbd "C-c p") "Prefix for pomodoro.el commands")
;;   (defvar pomodoro-command-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "p") #'pomodoro-start)
;;       (define-key map (kbd "k") #'pomodoro-stop)
;;       (define-key map (kbd "r") #'pomodoro-resume)
;;       (define-key map (kbd "a") #'pomodoro-pause)
;;       map)
;;     "Keymap for pomodoro.el commands after `pomodoro-keymap-prefix'.")
;;   (setq pomodoro-map pomodoro-command-map)
;;   (global-set-key pomodoro-keymap-prefix pomodoro-map)
;;   (defvar pomodoro-state "uninitialized")
;;   (defun pomodoro-dwim ()
;;     "Start or stop pomodoro timer"
;;     (interactive)
;;     (case pomodoro-state
;;       ("uninitialized" (progn (pomodoro-start)
;;                               (setq pomodoro-state "running")))
;;       ("paused" (if (eq pomodoro-time-remaining 0)
;;                     (progn (pomodoro-start)
;;                            (setq pomodoro-state "running"))
;;                   (progn (pomodoro-pause)
;;                          (setq pomodoro-state "paused"))))
;;       ("paused" (progn (pomodoro-resume)
;;                        (setq pomodoro-state "running")))))
;;   :config
;;   (pomodoro-add-to-mode-line))



(provide 'experimental)
;;; experimental.el ends here
