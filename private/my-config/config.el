;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; Copyright (c) 2015 Michael Abrahams
;; Author: Michael Abrahams <miabraha@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:


;; Don't GC inside of Helm
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(setq-default evil-escape-key-sequence "kj")

(spacemacs|use-package-add-hook rcirc
  :post-config
  (setq-default rcirc-server-alist (quote (("scorpio.local" :nick "abrahams" :port 6667)))))

(spacemacs|use-package-add-hook znc
  :post-config
  (setq-default znc-servers (quote (("scorpio.local" 6667 nil ((scorpio "abrahams" "alphAnum8ric")))))))

(setq projectile-tags-command "c:\\bin\\ctags\\ctags.exe -R -e")
(evil-leader/set-key "st" 'helm-etags-select)

(spacemacs|use-package-add-hook evil-search-highlight-persist
  :post-config
  (set-face-attribute 'evil-search-highlight-persist-highlight-face nil :background "dim gray"))

(setq org-journal-dir (concat dropbox-directory "text/journal"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))


;; We want this here b/c it runs even in the terminal
(spacemacs|use-package-add-hook helm
  :post-config
  ;; Make Helm prettier
  (defun prettify-helm (&rest a)
    "Make Helm prettier.  A is used to allow an optional argument.  It seems weird to call this on frame creation but whatever works."
    (unless inside-terminal
      ;; (set-face-attribute 'helm-selection nil :inherit 'ag-match-face :background nil)
      (set-face-attribute 'helm-ff-directory nil :background nil)
      (set-face-attribute 'helm-ff-file nil :inherit nil)
      (set-face-attribute 'helm-ff-executable nil :inherit nil)
      )
    (set-face-attribute 'helm-source-header nil
                        :foreground "white" :weight 'bold :height 1.0 :background nil)
    (when inside-terminal
      ;; (set-face-attribute 'helm-selection nil :background "#000000")
      (set-face-attribute 'helm-source-header nil :background "#000000")
      (set-face-attribute 'helm-visible-mark  nil :background "#000000"))
    )

  (add-hook 'after-make-frame-functions 'prettify-helm)
  (prettify-helm)
  )

(spacemacs|use-package-add-hook expand-region
  :post-config
  (global-set-key (kbd "C-'") 'er/expand-region))

(spacemacs|use-package-add-hook hl-todo
  :post-config
  (setq-default hl-todo-activate-in-modes (quote (prog-mode emacs-lisp-mode)))
  )


(spacemacs|use-package-add-hook "smartparens"
  :post-config
  (sp-pair "\"" nil :actions :rem)
  (sp-local-pair 'c-mode "'" nil :unless nil)
  (sp-local-pair 'emacs-lisp-mode "`" "'")
  (sp-local-pair 'emacs-lisp-mode "\"" "\""))

(spacemacs|use-package-add-hook "znc"
  :post-init
  (setq-default znc-servers '(("scorpio" 6667 nil
                               ((abrahams "abrahams" "alphAnum8ric"))))))

(spacemacs|use-package-add-hook "org-agenda"

  :pre-init
  (defun my-daily-org-agenda ()
    (interactive)
    "Daily org agenda"
    (org-agenda nil "d"))

  (evil-leader/set-key "oa" 'my-daily-org-agenda)


  :post-config
  (defun my/save-all-agenda-buffers ()
    "Function used to save all agenda buffers that are
currently open, based on `org-agenda-files'."
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (member (buffer-file-name)
                      (mapcar 'expand-file-name (org-agenda-files t)))
          (save-buffer)))))

  (defun org-agenda-reschedule-to-today ()
    (interactive)
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest rest) (current-time))))
      (call-interactively 'org-agenda-schedule)))

  (defun org-agenda-reschedule-to-next-sunday ()
    (interactive)
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest rest)
                 ;; Compute next sunday
                 (let* ((ct (decode-time (current-time)))
                        (dow (car (nthcdr 6 ct)))
                        (nextsun (- 7 dow)))
                   ;; Set "day" and "dow"
                   (setcar (nthcdr 3 ct) (+ nextsun (nth 3 ct)))
                   (setcar (nthcdr 6 ct) 0)
                   (apply 'encode-time ct)))))
      (call-interactively 'org-agenda-schedule)))

  (setq-default org-agenda-insert-diary-extract-time t
                org-agenda-include-diary t
                org-agenda-restore-windows-after-quit nil
                org-startup-truncated nil
                org-agenda-start-on-weekday 0
                org-agenda-prefix-format
                '((agenda . " %i %-9:c[%+2e] %?-12t")
                  (timeline . "  % s")
                  (todo . " %i %-12:c")
                  (tags . " %i %-12:c")
                  (search . " %i %-12:c"))
                org-agenda-todo-keyword-format "%-5s"
                org-agenda-use-time-grid nil
                org-agenda-skip-unavailable-files t
                org-agenda-span 'day
                org-agenda-window-setup (quote current-window)
                org-confirm-elisp-link-function 'y-or-n-p
                org-agenda-bulk-custom-functions '((84 org-agenda-reschedule-to-today)
                                                   (87 org-agenda-reschedule-to-next-sunday))
                org-agenda-cmp-user-defined 'my-cmp-todo-state
                org-agenda-sorting-strategy '((agenda habit-up time-up user-defined-down
                                                      priority-down tag-up category-keep)
                                              (todo priority-down category-keep)
                                              (tags priority-down category-keep)
                                              (search category-keep))
                org-agenda-files (list (concat dropbox-directory "text/notes.org")
                                       (concat dropbox-directory "text/org/todo.org")
                                       (concat dropbox-directory "text/work/work.org")
                                       (concat dropbox-directory "text/creative/artwork.org"))
                org-habit-graph-column 52
                org-habit-show-all-today t
                org-habit-show-done-always-green t
                org-habit-show-habits-only-for-today t
                org-habit-today-glyph 32)


  ;; From Aaron Bieber
  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.\
PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))

  (setq org-agenda-custom-commands
        '(("d" "Daily agenda and all TODOs"
           ((agenda "" ((org-agenda-ndays 1)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "Unscheduled tasks"))))
           )))


  (defun my-org-agenda-mode-hook ()
    "Set up org-agenda mode"
    (spacemacs/disable-hl-line-mode)
    (setq-local truncate-lines 1)
    (defun org-agenda-capture (&optional with-time)
      "Call `org-capture' with the date at point.
With a `C-1' prefix, use the HH:MM value at point (if any) or the
current HH:MM time."
      (interactive "P")
      (if (not (eq major-mode 'org-agenda-mode))
          (user-error "You cannot do this outside of agenda buffers")
        (call-interactively 'org-capture)))
    )
  (add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)

  )



(spacemacs|use-package-add-hook "org"
  :pre-init
  ;; Org-protocol: disabled for now.
  ;; Test using this command:
  ;; C:\bin\emacs\bin\emacsclientw.exe "org-protocol:/capture?template=p&link=aaaa&title=TITLE&body=BODY"
  ;; I deleted org-protocol.el/elc in c:\bin\emacs; not sure this is necessary.
  ;; (require 'org-protocol)

  ;; (package-initialize)
  ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
  ;; http://orgmode.org/manual/Capture-templates.html#Capture-templates
  ;; For Chrome extension :D
  :post-config
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))
  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame))
    (org-save-all-org-buffers))
  (defun my-cmp-todo-state (a b)
    "Compare the todo states of strings A and B."
    (let* ((ma (or (get-text-property 1 'org-marker a)
                   (get-text-property 1 'org-hd-marker a)))
           (mb (or (get-text-property 1 'org-marker b)
                   (get-text-property 1 'org-hd-marker b)))
           (fa (and ma (marker-buffer ma)))
           (fb (and mb (marker-buffer mb)))
           (todo-kwds
            (or (and fa (with-current-buffer fa org-todo-keywords-1))
                (and fb (with-current-buffer fb org-todo-keywords-1))))
           (ta (or (get-text-property 1 'todo-state a) ""))
           (tb (or (get-text-property 1 'todo-state b) ""))
           (la (- (length (member ta todo-kwds))))
           (lb (- (length (member tb todo-kwds))))
           (donepa (member ta org-done-keywords-for-agenda))
           (donepb (member tb org-done-keywords-for-agenda)))
      (cond ((and donepa (not donepb)) -1)
            ((and (not donepa) donepb) +1)
            (nil)
            )))


  (setq-default org-confirm-elisp-link-not-regexp "org-journal-new-entry"
                org-directory (concat dropbox-directory "/text")
                org-default-notes-file (concat org-directory "/notes.org")
                org-extend-today-until 1
                org-todo-keywords
                '((sequence "TODO(t)" "|" "DONE(d)")
                  (sequence "UNREAD(u)" "BLANK(b)" "DRAFT(r)" "SENT(s)"))
                org-link-frame-setup
                '((vm . vm-visit-folder-other-frame)
                  (vm-imap . vm-visit-imap-folder-other-frame)
                  ;; (gnus . org-gnus-no-new-news)
                  (file . find-file-other-window)
                  (wl . wl-other-frame))
                )


  (add-to-list 'org-modules 'org-habit)

  (let ((todo-file (concat dropbox-directory "text/org/todo.org"))
        (work-file (concat dropbox-directory "text/work/work.org"))
        (notes-file (concat dropbox-directory "text/notes.org"))
        (concepts-file (concat dropbox-directory "text/creative/concepts.org")))
    (setq-default
     ;; At some point, figure out how to do unscheduled view?
     org-capture-templates `(("t" "Todo item" entry
                              (file+headline ,todo-file "Todo Items")
                              "** TODO %? \nSCHEDULED: %t" :empty-lines 1)
                             ("T" "Unscheduled todo" entry
                              (file+headline ,todo-file "Todo Items")
                              "** TODO %?%i" :empty-lines 1)
                             ("w" "Work" entry
                              (file+headline ,work-file "Todo")
                              "** TODO %? \n SCHEDULED: %t\n%i" :empty-lines 1)
                             ("m" "Message" entry
                              (file+headline ,notes-file "Messages")
                              "** BLANK %? \nSCHEDULED: %t\n" :empty-lines 2)
                             ("n" "Note" entry
                              (file org-default-notes-file)
                              "* %^{Subject} :NOTE:\n%i\n%a%?" :empty-lines 1)
                             ("c" "Capture" entry
                              (file+headline org-default-notes-file "Notes")
                              "* %?\n\n  Source: %u, %c\n\n  %i" :empty-lines 1)
                             ("f" "Creative Concept" entry
                              (file+headline ,concepts-file "Uncategorized")
                              "*  %?\n" :empty-lines 1)
                             ("p" "Protocol" entry
                              (file+headline org-default-notes-file "Notes")
                              "** %?   %:annotation \n%i\n" :empty-lines 1)
                             ("L" "Protocol Link" entry
                              (file+headline org-default-notes-file "Org-capture links")
                              "** %?[[%:link][%:description]] %U\n"
                              :empty-lines 1 :immediate-finish t))))

  (push '("\\`CAPTURE-" . insert) evil-buffer-regexps)


  (defun my-org-mode-hook ()
    "Fix back-button-mode bindings conflicting with org-mode."
    (let ((oldmap (cdr (assoc 'back-button-mode minor-mode-map-alist)))
          (newmap (make-sparse-keymap)))
      (set-keymap-parent newmap oldmap)
      (define-key newmap (kbd "<M-left>") nil)
      (define-key newmap (kbd "<M-right>") nil)
      (make-local-variable 'minor-mode-overriding-map-alist)
      (push `(back-button-mode . ,newmap) minor-mode-overriding-map-alist))
    (setq-local word-wrap 1)
    (set-face-attribute 'org-habit-alert-future-face nil :background "brown3")
    (set-face-attribute 'org-todo nil :background nil)
    (set-face-attribute 'org-habit-alert-future-face nil :inherit 'org-habit-alert-face)
    (set-face-attribute 'org-habit-overdue-future-face nil :inherit 'org-habit-overdue-face)
    )

  (add-hook 'org-mode-hook 'my-org-mode-hook)


  (define-key org-mode-map [(control tab)] nil)
  (define-key org-mode-map (kbd "C-c d") #'org-schedule)
  (global-set-key (kbd "C-c c") #'org-capture)
  (set-face-attribute 'org-level-1 nil :background nil :overline nil)
  (set-face-attribute 'org-level-2 nil :background nil :overline nil)
  (set-face-attribute 'org-level-3 nil :background nil :overline nil)
  )




;; C++ specific stuff
(spacemacs|use-package-add-hook "cc-mode"
  :pre-init
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  :post-config
  (c-add-style "kde-c" '("stroustrup"
                         (c-basic-offset . 4)
                         (c-offsets-alist
                          (case-label . 4)
                          (access-label . -)
                          (label . 0)
                          (statement-cont . c-lineup-math)
                          )))
  (c-add-style "kde-c++" '("kde-c" (c-offsets-alist . ((case-label . 0) (inline-open . 0)))))
  (defun kde-c++-mode-init ()
    "Set up C++ style for KDE source code."
    (interactive)
    (setq-default c-block-comment-prefix "* ")
    (c-set-style "kde-c++")
    (prettify-symbols-mode)
    (define-key c++-mode-map (kbd "<C-return>") #'c-indent-new-comment-line)
    (message "KDE-C++ mode loaded! <3"))

  (add-hook 'c-mode-common-hook #'kde-c++-mode-init)
  (add-hook 'c-mode-common-hook #'kde-c++-mode-init)
  (add-hook 'c++-mode-hook #'kde-c++-mode-init)
  )

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

(spacemacs|diminish compilation-shell-minor-mode " Cmp")

