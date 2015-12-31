;;; helm-rtags.el --- RTags helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Michael Abrahams

;; Author: Michael Abrahams <miabraha@gmail.com>
;; URL:
;; Package-Version:
;; Version: 0.0
;; Package-Requires: ((rtags "") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this package, add these lines to your init.el or .emacs file:
;;
;;     ;; Enable helm-rtags-mode
;;     (add-hook 'c-mode-hook 'helm-rtags-mode)
;;     (add-hook 'c++-mode-hook 'helm-rtags-mode)
;;     (add-hook 'asm-mode-hook 'helm-rtags-mode)
;;
;;     ;; Set key bindings
;;     (eval-after-load "helm-rtags"
;;       '(progn
;;          (define-key helm-rtags-mode-map (kbd "M-t") 'helm-rtags-find-tag)
;;          (define-key helm-rtags-mode-map (kbd "M-r") 'helm-rtags-find-rtag)
;;          (define-key helm-rtags-mode-map (kbd "M-s") 'helm-rtags-find-symbol)
;;          (define-key helm-rtags-mode-map (kbd "M-g M-p") 'helm-rtags-parse-file)
;;          (define-key helm-rtags-mode-map (kbd "C-c <") 'helm-rtags-previous-history)
;;          (define-key helm-rtags-mode-map (kbd "C-c >") 'helm-rtags-next-history)
;;          (define-key helm-rtags-mode-map (kbd "M-,") 'helm-rtags-pop-stack)))
;;

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-files)
(require 'which-func)
;; (require 'rtags)

;; (declare-function cygwin-convert-file-name-from-windows "cygw32.c")
;; (declare-function cygwin-convert-file-name-to-windows "cygw32.c")

;; (defgroup helm-rtags nil
;;   "RTags for Helm"
;;   :group 'helm)

;; (defcustom helm-rtags-path-style 'root
;;   "Style of file path"
;;   :type '(choice (const :tag "Root of the current project" root)
;;                  (const :tag "Relative from the current directory" relative)
;;                  (const :tag "Absolute Path" absolute))
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-read-only nil
;;   "Rtags read only mode."
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-auto-update nil
;;   "*If non-nil, tag files are updated whenever a file is saved."
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-pulse-at-cursor t
;;   "If non-nil, pulse at point after jumping"
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-cache-select-result nil
;;   "*If non-nil, results of helm-rtags-select and helm-rtags-select-path are cached."
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-cache-max-result-size (* 10 1024 1024) ;10M
;;   "Max size(bytes) to cache for each select result."
;;   :type 'integer
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-update-interval-second 60
;;   "Tags are updated in `after-save-hook' if this seconds is passed from last update.
;; Always update if value of this variable is nil."
;;   :type '(choice (integer :tag "Update interval seconds")
;;                  (boolean :tag "Update every time" nil))
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-highlight-candidate t
;;   "Highlight candidate or not"
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-prefix-key "\C-c"
;;   "If non-nil, it is used for the prefix key of rtags-xxx command."
;;   :type 'string
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-suggested-key-mapping nil
;;   "If non-nil, suggested key mapping is enabled."
;;   :type 'boolean
;;   :group 'helm-rtags)

;; (defcustom helm-rtags-display-style nil
;;   "Style of display result."
;;   :type '(choice (const :tag "Show in detail" detail)
;;                  (const :tag "Normal style" nil))
;;   :group 'helm-rtags)

(defcustom helm-rtags-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean
  :group 'helm-rtags)

(defcustom helm-rtags-maximum-candidates (if helm-rtags-fuzzy-match 100 9999)
  "Maximum number of helm candidates"
  :type 'integer
  :group 'helm-rtags)

;; (defface helm-rtags-file
;;   '((t :inherit font-lock-keyword-face))
;;   "Face for line numbers in the error list."
;;   :group 'helm-rtags)

;; (defface helm-rtags-lineno
;;   '((t :inherit font-lock-doc-face))
;;   "Face for line numbers in the error list."
;;   :group 'helm-rtags)

;; (defface helm-rtags-match
;;   '((t :inherit helm-match))
;;   "Face for word matched against tagname"
;;   :group 'helm-rtags)

(defvar helm-rtags--tag-location nil)
(defvar helm-rtags--last-update-time 0)
(defvar helm-rtags--completing-history nil)
(defvar helm-rtags--context-stack (make-hash-table :test 'equal))
(defvar helm-rtags--result-cache (make-hash-table :test 'equal))
(defvar helm-rtags--saved-context nil)
(defvar helm-rtags--use-otherwin nil)
(defvar helm-rtags--local-directory nil)
(defvar helm-rtags--parsed-file nil)
(defvar helm-rtags--current-position nil)
(defvar helm-rtags--real-tag-location nil)
(defvar helm-rtags--last-input nil)
(defvar helm-rtags--query nil)

(defconst helm-rtags--buffer "*helm rtags*")

(defconst helm-rtags--include-regexp
  "\\`\\s-*#\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")

;;;;;;;;;;;;;;;
;; Starts here!
;;;;;;;;;;;;;;;

;; Not defined:
;; - Some values in rtags-comp-func-alist

;;;###autoload
(defun helm-rtags-find-tag (tag)
  "Jump to definition of TAG."
  (interactive
   (list (helm-rtags--read-tagname 'tag)))
  (helm-rtags--common '(helm-source-rtags-tags) tag))


(defconst helm-rtags--prompt-alist
  '((tag       . "Find Definition: ")
    (pattern   . "Find Pattern: ")
    (rtag      . "Find Reference: ")
    (symbol    . "Find Symbol: ")
    (find-file . "Find File: ")))

(defconst helm-rtags-comp-func-alist
  '((tag       . helm-rtags--completing-rtags)
    (pattern   . helm-rtags--completing-pattern)
    (rtag      . helm-rtags--completing-grtags)
    (symbol    . helm-rtags--completing-gsyms)
    (find-file . helm-rtags--completing-files))
  )


(defcustom helm-rtags-use-input-at-cursor nil
  "Use input at cursor"
  :type 'boolean
  :group 'helm-rtags)

(defcustom helm-rtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-rtags)

(defsubst helm-rtags--using-other-window-p ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defcustom helm-rtags-preselect nil
  "If non-nil, preselect current file and line."
  :type 'boolean
  :group 'helm-rtags)


(defvar helm-rtags--find-file-action
  ;; '(("Open file" . helm-rtags--action-openfile)
  ;; ("Open file other window" . helm-rtags--action-openfile-other-window)))
  (lambda () ))

;; Completion functions for completing-read.
(defun helm-rtags--completing-rtags (string predicate code)
  (helm-rtags--complete 'tag string predicate code))
(defun helm-rtags--completing-pattern (string predicate code)
  (helm-rtags--complete 'pattern string predicate code))
(defun helm-rtags--completing-grtags (string predicate code)
  (helm-rtags--complete 'rtag string predicate code))
(defun helm-rtags--completing-gsyms (string predicate code)
  (helm-rtags--complete 'symbol string predicate code))
(defun helm-rtags--completing-files (string predicate code)
  (helm-rtags--complete 'find-file string predicate code))

(defun helm-rtags--complete (type string predicate code)
  (when (eq type "symbol")
    (rtags-symbolname-complete string predicate code)
    (rtags-filename-complete string predicate code)))

;; (defun helm-rtags--action-openfile (cand)
;;   (let* ((file-and-line (helm-rtags--extract-file-and-line cand))
;;          (filename (car file-and-line))
;;          (line (cdr file-and-line))
;;          (open-func (helm-rtags--select-find-file-func))
;;          (default-directory (helm-rtags--base-directory)))
;;     (helm-rtags--do-open-file open-func filename line)))

;; (defun helm-rtags--action-openfile-other-window (cand)
;;   (let ((helm-rtags--use-otherwin t))
;;     (helm-rtags--action-openfile cand)))



;;;###autoload
(defun helm-rtags-find-rtag (tag)
  "Jump to referenced point"
  (interactive
   (list (helm-rtags--read-tagname 'rtag (which-function))))
  (helm-rtags--common '(helm-source-rtags-rtags) tag))

;;;###autoload
(defun helm-rtags-find-symbol (tag)
  "Jump to the symbol location"
  (interactive
   (list (helm-rtags--read-tagname 'symbol)))
  (helm-rtags--common '(helm-source-rtags-gsyms) tag))


(defvar helm-source-rtags-tags
  (helm-build-in-buffer-source "Jump to definitions"
    :init 'helm-rtags--tags-init
    :candidate-number-limit helm-rtags-maximum-candidates
    :real-to-display 'helm-rtags--candidate-transformer
    :persistent-action 'helm-rtags--persistent-action
    :fuzzy-match helm-rtags-fuzzy-match
    :action helm-rtags--find-file-action))

(defvar helm-source-rtags-rtags
  (helm-build-in-buffer-source "Jump to references"
    :init 'helm-rtags--rtags-init
    :candidate-number-limit helm-rtags-maximum-candidates
    :real-to-display 'helm-rtags--candidate-transformer
    :persistent-action 'helm-rtags--persistent-action
    :fuzzy-match helm-rtags-fuzzy-match
    :action helm-rtags--find-file-action))

(defvar helm-source-rtags-gsyms
  (helm-build-in-buffer-source "Jump to symbols"
    :init 'helm-rtags--gsyms-init
    :candidate-number-limit helm-rtags-maximum-candidates
    :real-to-display 'helm-rtags--candidate-transformer
    :persistent-action 'helm-rtags--persistent-action
    :fuzzy-match helm-rtags-fuzzy-match
    :action helm-rtags--find-file-action))


(defun helm-rtags--token-at-point (type)
  (if (not (eq type 'find-file))
      (thing-at-point 'symbol t)
    (let ((line (helm-current-line-contents)))
      (when (string-match helm-rtags--include-regexp line)
        (match-string-no-properties 1 line)))))


(defun helm-rtags--read-tagname (type &optional default-tagname)
  (let ((tagname (helm-rtags--token-at-point type))
        (prompt (assoc-default type helm-rtags--prompt-alist))
        (comp-func (assoc-default type helm-rtags-comp-func-alist)))
    (if (and tagname helm-rtags-use-input-at-cursor)
        tagname
      (when (and (not tagname) default-tagname)
        (setq tagname default-tagname))
      (when tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
      (let ((completion-ignore-case helm-rtags-ignore-case)
            (completing-read-function 'completing-read-default))
        (completing-read prompt comp-func nil nil nil
                         'helm-rtags--completing-history tagname)))))


(defun helm-rtags--common (srcs tagname)
  "Basic Helm source.  SRCS TAGNAME."
  (let ((helm-quit-if-no-candidate t)
        (helm-execute-action-at-once-if-one t)
        (dir (helm-rtags--searched-directory))
        (src (car srcs))
        (preselect-regexp (when helm-rtags-preselect
                            (regexp-quote (helm-rtags--current-file-and-line)))))
    (when (symbolp src)
      (setq src (symbol-value src)))
    (unless helm-rtags--use-otherwin
      (setq helm-rtags--use-otherwin (helm-rtags--using-other-window-p)))
    (when tagname
      (setq helm-rtags--query tagname))
    (let ((tagroot (helm-rtags--find-tag-simple)))
      (helm-attrset 'helm-rtags-base-directory dir src)
      (helm-attrset 'name (concat "RTags at " (or dir tagroot)) src)
      (helm :sources srcs :buffer helm-rtags--buffer
            :preselect preselect-regexp))))

(defun helm-rtags--searched-directory ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 (let ((dir (read-directory-name "Input Directory: ")))
         (setq helm-rtags--local-directory (file-name-as-directory dir))))
    (16 (file-name-directory (buffer-file-name)))))

(defun helm-rtags--find-tag-simple (&optional prefix)
  (let ((loc (rtags-current-location))
        (fn (buffer-file-name)))
    (rtags-call-rc :path fn :path-filter prefix "-f" loc :output t)))

  ;; (or
  ;;  (getenv "RTAGSROOT")
  ;;     (locate-dominating-file default-directory "RTAGS")
  ;;     (if (not (yes-or-no-p "File RTAGS not found. Run 'rtags'? "))
  ;;         (user-error "Abort")
  ;;       (let* ((tagroot (read-directory-name "Root Directory: "))
  ;;              (label (helm-rtags--read-rtagslabel))
  ;;              (default-directory tagroot))
  ;;         (message "rtags is generating tags....")
  ;;         (unless (zerop (process-file "rtags" nil nil nil
  ;;                                      "-q" (helm-rtags--label-option label)))
  ;;           (error "Faild: 'rtags -q'"))
  ;;         tagroot)))

(rtags-call-rc :path fn :path-filter prefix "-f" arg)











;; (defmacro helm-declare-obsolete-variable (old new version)
;;   `(progn
;;      (defvaralias ,old ,new)
;;      (make-obsolete-variable ,old ,new ,version)))

;; (helm-declare-obsolete-variable
;;  'helm-c-rtags-path-style 'helm-rtags-path-style "0.8")
;; (helm-declare-obsolete-variable
;;  'helm-c-rtags-ignore-case 'helm-rtags-ignore-case  "0.8")
;; (helm-declare-obsolete-variable
;;  'helm-c-rtags-read-only 'helm-rtags-read-only "0.8")


;; (defconst helm-rtags--search-option-alist
;;   '((pattern   . "-g")
;;     (rtag      . "-r")
;;     (symbol    . "-s")
;;     (find-file . "-Poa")))

;; (defsubst helm-rtags--windows-p ()
;;   (memq system-type '(windows-nt ms-dos)))

;; Work around for GNU global Windows issue
;; (defsubst helm-rtags--use-abs-path-p (rtagslibpath)
;;   (and (helm-rtags--windows-p) rtagslibpath))

(defun helm-rtags--construct-options (type completion)
  (let ((find-file-p (eq type 'find-file))
        (rtagslibpath (getenv "RTAGSLIBPATH"))
        options)
    (unless find-file-p
      (push "--result=grep" options))
    (when completion
      (push "-c" options))
    (helm-aif (assoc-default type helm-rtags--search-option-alist)
        (push it options))
    (when (or (eq helm-rtags-path-style 'absolute)
              (helm-rtags--use-abs-path-p rtagslibpath))
      (push "-a" options))
    (when helm-rtags-ignore-case
      (push "-i" options))
    (when (and current-prefix-arg (not find-file-p))
      (push "-l" options))
    (when rtagslibpath
      (push "-T" options))
    options))




;; (defun helm-rtags--path-libpath-p (tagroot)
;;   (helm-aif (getenv "RTAGSLIBPATH")
;;       (cl-loop for path in (parse-colon-path it)
;;                for libpath = (file-name-as-directory (expand-file-name path))
;;                thereis (string= tagroot libpath))))

;; (defun helm-rtags--tag-directory ()
;;   (with-temp-buffer
;;     (helm-aif (getenv "RTAGSROOT")
;;         it
;;       (unless (zerop (process-file "global" nil t nil "-p"))
;;         (error "RTAGS not found"))
;;       (goto-char (point-min))
;;       (when (looking-at "^\\([^\r\n]+\\)")
;;         (let ((tag-path (match-string-no-properties 1)))
;;           (file-name-as-directory
;;            (if (eq system-type 'cygwin)
;;                (cygwin-convert-file-name-from-windows tag-path)
;;              tag-path)))))))

;; (defun helm-rtags--find-tag-directory ()
;;   (setq helm-rtags--real-tag-location nil)
;;   (let ((tagroot (helm-rtags--tag-directory)))
;;     (if (and (helm-rtags--path-libpath-p tagroot) helm-rtags--tag-location)
;;         (progn
;;           (setq helm-rtags--real-tag-location tagroot)
;;           helm-rtags--tag-location)
;;       (setq helm-rtags--tag-location tagroot))))

;; (defun helm-rtags--base-directory ()
;;   (let ((dir (or helm-rtags--local-directory
;;                  (cl-case helm-rtags-path-style
;;                    (root (or helm-rtags--real-tag-location
;;                              helm-rtags--tag-location))
;;                    (otherwise default-directory))))
;;         (remote (file-remote-p default-directory)))
;;     (if (and remote (not (file-remote-p dir)))
;;         (concat remote dir)
;;       dir)))

;; (defsubst helm-rtags--new-context-info (index stack)
;;   (list :index index :stack stack))

;; (defun helm-rtags--put-context-stack (tag-location index stack)
;;   (puthash tag-location (helm-rtags--new-context-info index stack)
;;            helm-rtags--context-stack))

;; (defsubst helm-rtags--current-context ()
;;   (let ((file (buffer-file-name (current-buffer))))
;;     (list :file file :position (point) :readonly buffer-file-read-only)))

;; (defsubst helm-rtags--save-current-context ()
;;   (setq helm-rtags--saved-context (helm-rtags--current-context)))

;; (defun helm-rtags--open-file (file readonly)
;;   (if readonly
;;       (find-file-read-only file)
;;     (find-file file)))

;; (defun helm-rtags--open-file-other-window (file readonly)
;;   (setq helm-rtags--use-otherwin nil)
;;   (if readonly
;;       (find-file-read-only-other-window file)
;;     (find-file-other-window file)))

;; (defun helm-rtags--get-context-info ()
;;   (let* ((tag-location (helm-rtags--find-tag-directory))
;;          (context-info (gethash tag-location helm-rtags--context-stack))
;;          (context-stack (plist-get context-info :stack)))
;;     (if (null context-stack)
;;         (error "Context stack is empty(TAG at %s)" tag-location)
;;       context-info)))

;; (defun helm-rtags--get-or-create-context-info ()
;;   (or (gethash helm-rtags--tag-location helm-rtags--context-stack)
;;       (helm-rtags--new-context-info -1 nil)))

;; ;;;###autoload
;; (defun helm-rtags-clear-all-cache ()
;;   (interactive)
;;   (clrhash helm-rtags--result-cache))

;; ;;;###autoload
;; (defun helm-rtags-clear-cache ()
;;   (interactive)
;;   (helm-rtags--find-tag-directory)
;;   (let* ((tag-location (or helm-rtags--real-tag-location
;;                            helm-rtags--tag-location))
;;          (rtags-path (concat tag-location "RTAGS"))
;;          (gpath-path (concat tag-location "GPATH")))
;;     (remhash rtags-path helm-rtags--result-cache)
;;     (remhash gpath-path helm-rtags--result-cache)))

;; (defun helm-rtags--move-to-context (context)
;;   (let ((file (plist-get context :file))
;;         (curpoint (plist-get context :position))
;;         (readonly (plist-get context :readonly)))
;;     (helm-rtags--open-file file readonly)
;;     (goto-char curpoint)
;;     (recenter)))

;; ;;;###autoload
;; (defun helm-rtags-next-history ()
;;   "Jump to next position on context stack"
;;   (interactive)
;;   (let* ((context-info (helm-rtags--get-context-info))
;;          (current-index (plist-get context-info :index))
;;          (context-stack (plist-get context-info :stack))
;;          context)
;;     (when (<= current-index -1)
;;       (error "This context is latest in context stack"))
;;     (setf (nth current-index context-stack) (helm-rtags--current-context))
;;     (cl-decf current-index)
;;     (if (= current-index -1)
;;         (setq context helm-rtags--current-position
;;               helm-rtags--current-position nil)
;;       (setq context (nth current-index context-stack)))
;;     (helm-rtags--put-context-stack helm-rtags--tag-location
;;                                    current-index context-stack)
;;     (helm-rtags--move-to-context context)))

;; ;;;###autoload
;; (defun helm-rtags-previous-history ()
;;   "Jump to previous position on context stack"
;;   (interactive)
;;   (let* ((context-info (helm-rtags--get-context-info))
;;          (current-index (plist-get context-info :index))
;;          (context-stack (plist-get context-info :stack))
;;          (context-length (length context-stack)))
;;     (cl-incf current-index)
;;     (when (>= current-index context-length)
;;       (error "This context is last in context stack"))
;;     (if (= current-index 0)
;;         (setq helm-rtags--current-position (helm-rtags--current-context))
;;       (setf (nth (- current-index 1) context-stack) (helm-rtags--current-context)))
;;     (let ((prev-context (nth current-index context-stack)))
;;       (helm-rtags--move-to-context prev-context))
;;     (helm-rtags--put-context-stack helm-rtags--tag-location
;;                                    current-index context-stack)))

;; (defun helm-rtags--get-result-cache (file)
;;   (helm-rtags--find-tag-directory)
;;   (let* ((file-path (concat (or helm-rtags--real-tag-location
;;                                 helm-rtags--tag-location)
;;                             file))
;;          (file-mtime (nth 5 (file-attributes file-path)))
;;          (hash-value (gethash file-path helm-rtags--result-cache))
;;          (cached-file-mtime (nth 0 hash-value)))
;;     (if (and cached-file-mtime (equal cached-file-mtime file-mtime))
;;         (nth 1 hash-value)
;;       nil)))

;; (defun helm-rtags--put-result-cache (file cache)
;;   (helm-rtags--find-tag-directory)
;;   (let* ((file-path (concat (or helm-rtags--real-tag-location
;;                                 helm-rtags--tag-location)
;;                             file))
;;          (file-mtime (nth 5 (file-attributes file-path)))
;;          (hash-value (list file-mtime cache)))
;;     (puthash file-path hash-value helm-rtags--result-cache)))

;; (defun helm-rtags--referer-function (file ref-line)
;;   (let ((is-opened (cl-loop with path = (concat default-directory file)
;;                             for buf in (buffer-list)
;;                             when (string= (buffer-file-name buf) path)
;;                             return it))
;;         retval)
;;     (with-current-buffer (find-file-noselect file)
;;       (goto-char (point-min))
;;       (forward-line (1- ref-line))
;;       (unless (zerop (current-indentation))
;;         (setq retval (which-function)))
;;       (unless is-opened
;;         (kill-buffer (current-buffer)))
;;       retval)))

;; (defun helm-rtags--show-detail ()
;;   (goto-char (point-min))
;;   (while (not (eobp))
;;     (let ((line (helm-current-line-contents)))
;;       (let* ((file-and-line (helm-rtags--extract-file-and-line line))
;;              (file (car file-and-line))
;;              (ref-line (cdr file-and-line))
;;              (ref-func (helm-rtags--referer-function file ref-line)))
;;         (when ref-func
;;           (search-forward ":" nil nil 2)
;;           (insert " " ref-func "|"))
;;         (forward-line 1)))))

;; (defun helm-rtags--exec-global-command (type input &optional detail)
;;   (let ((args (helm-rtags--construct-command type input)))
;;     (helm-rtags--find-tag-directory)
;;     (helm-rtags--save-current-context)
;;     (let ((buf-coding buffer-file-coding-system))
;;       (with-current-buffer (helm-candidate-buffer 'global)
;;         (let ((default-directory (helm-rtags--base-directory))
;;               (input (car (last args)))
;;               (coding-system-for-read buf-coding)
;;               (coding-system-for-write buf-coding))
;;           (unless (zerop (apply 'process-file "global" nil '(t nil) nil args))
;;             (error (format "%s: not found" input)))
;;           (when detail
;;             (helm-rtags--show-detail)))))))

;; (defun helm-rtags--construct-command (type &optional in)
;;   (setq helm-rtags--local-directory nil)
;;   (let ((dir (helm-attr 'helm-rtags-base-directory (helm-get-current-source))))
;;     (when (and dir (not (eq type 'find-file)))
;;       (setq helm-rtags--local-directory dir)))
;;   (let ((input (or in helm-rtags--query))
;;         (options (helm-rtags--construct-options type nil)))
;;     (when (string= input "")
;;       (error "Input is empty!!"))
;;     (setq helm-rtags--last-input input)
;;     (reverse (cons input options))))

;; (defun helm-rtags--tags-init (&optional input)
;;   (helm-rtags--exec-global-command 'tag input))

;; (defun helm-rtags--pattern-init (&optional input)
;;   (helm-rtags--exec-global-command 'pattern input helm-rtags-display-style))

;; (defun helm-rtags--rtags-init (&optional input)
;;   (helm-rtags--exec-global-command 'rtag input helm-rtags-display-style))

;; (defun helm-rtags--gsyms-init ()
;;   (helm-rtags--exec-global-command 'symbol nil helm-rtags-display-style))

;; (defun helm-rtags--files-init ()
;;   (helm-rtags--exec-global-command 'find-file nil))

;; (defun helm-rtags--real-file-name ()
;;   (let ((buffile (buffer-file-name)))
;;     (unless buffile
;;       (error "This buffer is not related to file."))
;;     (if (file-remote-p buffile)
;;         (tramp-file-name-localname (tramp-dissect-file-name buffile))
;;       (file-truename buffile))))

;; (defun helm-rtags--find-tag-from-here-init ()
;;   (helm-rtags--find-tag-directory)
;;   (helm-rtags--save-current-context)
;;   (let ((token (helm-rtags--token-at-point 'from-here)))
;;     (unless token
;;       (error "Cursor is not on symbol."))
;;     (let* ((filename (helm-rtags--real-file-name))
;;            (from-here-opt (format "--from-here=%d:%s"
;;                                   (line-number-at-pos)
;;                                   (if (eq system-type 'cygwin)
;;                                       (cygwin-convert-file-name-to-windows filename)
;;                                     filename))))
;;       (setq helm-rtags--last-input token)
;;       (with-current-buffer (helm-candidate-buffer 'global)
;;         (let* ((default-directory (helm-rtags--base-directory))
;;                (status (process-file "global" nil '(t nil) nil
;;                                      "--result=grep" from-here-opt token)))
;;           (unless (zerop status)
;;             (cond ((= status 1)
;;                    (error "Error: %s%s" (buffer-string) filename))
;;                   ((= status 3)
;;                    (error "Error: %s" (buffer-string)))
;;                   (t (error "%s: not found" token)))))))))

;; (defun helm-rtags--parse-file-init ()
;;   (with-current-buffer (helm-candidate-buffer 'global)
;;     (unless (zerop (process-file "global" nil t nil
;;                                  "--result=cscope" "-f" helm-rtags--parsed-file))
;;       (error "Failed: 'global --result=cscope -f %s" helm-rtags--parsed-file))))

;; (defun helm-rtags--push-context (context)
;;   (let* ((context-info (helm-rtags--get-or-create-context-info))
;;          (current-index (plist-get context-info :index))
;;          (context-stack (plist-get context-info :stack)))
;;     (unless (= current-index -1)
;;       (setq context-stack (nthcdr (1+ current-index) context-stack)))
;;     (setq helm-rtags--current-position nil)
;;     (push context context-stack)
;;     (helm-rtags--put-context-stack helm-rtags--tag-location -1 context-stack)))

;; (defsubst helm-rtags--select-find-file-func ()
;;   (if helm-rtags--use-otherwin
;;       'helm-rtags--open-file-other-window
;;     'helm-rtags--open-file))

;; (defun helm-rtags--do-open-file (open-func file line)
;;   (funcall open-func file helm-rtags-read-only)
;;   (goto-char (point-min))
;;   (forward-line (1- line))
;;   (back-to-indentation)
;;   (recenter)
;;   (helm-rtags--push-context helm-rtags--saved-context)
;;   (when helm-rtags-pulse-at-cursor
;;     (pulse-momentary-highlight-one-line (point))))

;; (defun helm-rtags--find-line-number (cand)
;;   (if (string-match "\\s-+\\([1-9][0-9]+\\)\\s-+" cand)
;;       (string-to-number (match-string-no-properties 1 cand))
;;     (error "Can't find line number in %s" cand)))

;; (defun helm-rtags--parse-file-action (cand)
;;   (let ((line (helm-rtags--find-line-number cand))
;;         (open-func (helm-rtags--select-find-file-func)))
;;     (helm-rtags--do-open-file open-func helm-rtags--parsed-file line)))

;; (defsubst helm-rtags--has-drive-letter-p (path)
;;   (string-match-p "\\`[a-zA-Z]:" path))

;; (defun helm-rtags--extract-file-and-line (cand)
;;   (if (and (helm-rtags--windows-p) (helm-rtags--has-drive-letter-p cand))
;;       (when (string-match "\\(\\`[a-zA-Z]:[^:]+\\):\\([^:]+\\)" cand)
;;         (cons (match-string-no-properties 1 cand)
;;               (string-to-number (match-string-no-properties 2 cand))))
;;     (let ((elems (split-string cand ":")))
;;       (cons (cl-first elems) (string-to-number (cl-second elems))))))


;; (defun helm-rtags--file-content-at-pos (file pos)
;;   (with-current-buffer (find-file-noselect file)
;;     (save-excursion
;;       (goto-char pos)
;;       (format "%s:%d%s:%s"
;;               file (line-number-at-pos)
;;               (helm-aif (which-function) (format "[%s]" it) "")
;;               (helm-current-line-contents)))))

;; (defun helm-rtags--files-candidate-transformer (file)
;;   (let ((removed-regexp (concat "\\`" helm-rtags--tag-location)))
;;     (replace-regexp-in-string removed-regexp "" file)))

;; (defun helm-rtags--show-stack-init ()
;;   (cl-loop with context-stack = (plist-get (helm-rtags--get-context-info) :stack)
;;            with stack-length = (length context-stack)
;;            for context in (reverse context-stack)
;;            for file = (plist-get context :file)
;;            for pos  = (plist-get context :position)
;;            for index = (1- stack-length) then (1- index)
;;            for line = (helm-rtags--file-content-at-pos file pos)
;;            collect (cons (helm-rtags--files-candidate-transformer line) index)))

;; (defun helm-rtags--persistent-action (cand)
;;   (let* ((file-and-line (helm-rtags--extract-file-and-line cand))
;;          (filename (car file-and-line))
;;          (line (cdr file-and-line))
;;          (default-directory (helm-rtags--base-directory)))
;;     (find-file filename)
;;     (goto-char (point-min))
;;     (forward-line (1- line))
;;     (helm-highlight-current-line)))


;; (defvar helm-source-rtags-pattern
;;   (helm-build-in-buffer-source "Find pattern"
;;     :init 'helm-rtags--pattern-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :real-to-display 'helm-rtags--candidate-transformer
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action helm-rtags--find-file-action))

;; (defun helm-rtags--highlight-candidate (candidate)
;;   (let ((regexp (concat "\\_<" helm-rtags--last-input "\\_>"))
;;         (limit (1- (length candidate)))
;;         (last-pos 0)
;;         (case-fold-search nil))
;;     (while (and (< last-pos limit)
;;                 (string-match regexp candidate last-pos))
;;       (put-text-property (match-beginning 0) (match-end 0)
;;                          'face 'helm-rtags-match
;;                          candidate)
;;       (setq last-pos (1+ (match-end 0))))
;;     candidate))

;; (defun helm-rtags--transformer-regexp (candidate)
;;   (if (and (helm-rtags--windows-p) (helm-rtags--has-drive-letter-p candidate))
;;       "\\`\\([a-zA-Z]:[^:]+\\):\\([^:]+\\):\\(.*\\)"
;;     "\\`\\([^:]+\\):\\([^:]+\\):\\(.*\\)"))

;; (defun helm-rtags--candidate-transformer (candidate)
;;   (if (not helm-rtags-highlight-candidate)
;;       candidate
;;     (let ((regexp (helm-rtags--transformer-regexp candidate)))
;;       (when (string-match regexp candidate)
;;         (format "%s:%s:%s"
;;                 (propertize (match-string 1 candidate) 'face 'helm-rtags-file)
;;                 (propertize (match-string 2 candidate) 'face 'helm-rtags-lineno)
;;                 (helm-rtags--highlight-candidate (match-string 3 candidate)))))))

;; (defun helm-rtags--parse-file-candidate-transformer (file)
;;   (let ((removed-file (replace-regexp-in-string "\\`\\S-+ " "" file)))
;;     (when (string-match "\\`\\(\\S-+\\) \\(\\S-+\\) \\(.+\\)\\'" removed-file)
;;       (format "%-25s %-5s %s"
;;               (match-string-no-properties 1 removed-file)
;;               (match-string-no-properties 2 removed-file)
;;               (match-string-no-properties 3 removed-file)))))

;; (defvar helm-source-rtags-files
;;   (helm-build-in-buffer-source "Find files"
;;     :init 'helm-rtags--files-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :real-to-display 'helm-rtags--files-candidate-transformer
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action (helm-actions-from-type-file)))

;; (defvar helm-source-rtags-find-tag-from-here
;;   (helm-build-in-buffer-source "Find tag from here"
;;     :init 'helm-rtags--find-tag-from-here-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :real-to-display 'helm-rtags--candidate-transformer
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action helm-rtags--find-file-action))

;; (defvar helm-source-rtags-parse-file
;;   (helm-build-in-buffer-source "Parse file"
;;     :init 'helm-rtags--parse-file-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :real-to-display 'helm-rtags--parse-file-candidate-transformer
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action 'helm-rtags--parse-file-action))

;; (defun helm-rtags--show-stack-action (index)
;;   (let* ((context-info (helm-rtags--get-context-info))
;;          (context-stack (plist-get context-info :stack)))
;;     (helm-rtags--put-context-stack helm-rtags--tag-location
;;                                    index context-stack)
;;     (helm-rtags--move-to-context (nth index context-stack))))

;; (defvar helm-source-rtags-show-stack
;;   (helm-build-sync-source "Show Context Stack"
;;     :candidates 'helm-rtags--show-stack-init
;;     :volatile t
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action 'helm-rtags--show-stack-action))

;; ;;;###autoload
;; (defun helm-rtags-select ()
;;   (interactive)
;;   (helm-rtags--common '(helm-source-rtags-select) nil))

;; ;;;###autoload
;; (defun helm-rtags-select-path ()
;;   (interactive)
;;   (helm-rtags--common '(helm-source-rtags-select-path) nil))

;; (defsubst helm-rtags--beginning-of-defun ()
;;   (cl-case major-mode
;;     ((c-mode c++-mode java-mode) 'c-beginning-of-defun)
;;     (php-mode 'php-beginning-of-defun)
;;     (otherwise 'beginning-of-defun)))

;; (defsubst helm-rtags--end-of-defun ()
;;   (cl-case major-mode
;;     ((c-mode c++-mode java-mode malabar-mode) 'c-end-of-defun)
;;     (php-mode 'php-end-of-defun)
;;     (otherwise 'end-of-defun)))

;; (defun helm-rtags--current-funcion-bound ()
;;   (save-excursion
;;     (let (start)
;;       (funcall (helm-rtags--beginning-of-defun))
;;       (setq start (line-number-at-pos))
;;       (funcall (helm-rtags--end-of-defun))
;;       (cons start (line-number-at-pos)))))

;; (defun helm-rtags--tags-refered-from-this-function ()
;;   (let* ((file (helm-rtags--real-file-name))
;;          (bound (helm-rtags--current-funcion-bound))
;;          (start-line (car bound))
;;          (end-line (cdr bound)))
;;     (with-temp-buffer
;;       (unless (process-file "global" nil t nil "-f" "-r" file)
;;         (error "Failed: global -f -r %s" file))
;;       (goto-char (point-min))
;;       (let (tagnames finish)
;;         (while (and (not finish) (not (eobp)))
;;           (let* ((cols (split-string (helm-current-line-contents) nil t))
;;                  (lineno (string-to-number (cl-second cols))))
;;             (if (and (> lineno start-line) (< lineno end-line))
;;                 (let* ((tag (cl-first cols))
;;                        (elm (cl-find tag tagnames :test 'equal)))
;;                   (unless elm
;;                     (push tag tagnames)))
;;               (when (>= lineno end-line)
;;                 (setq finish t)))
;;             (forward-line 1)))
;;         (reverse tagnames)))))

;; (defun helm-rtags--tag-in-function-persistent-action (cand)
;;   (let* ((bound (helm-rtags--current-funcion-bound))
;;          (limit (save-excursion
;;                   (goto-char (point-min))
;;                   (forward-line (cdr bound))
;;                   (goto-char (line-end-position))
;;                   (point))))
;;     (when (search-forward cand nil limit)
;;       (helm-highlight-current-line))))

;; ;;;###autoload
;; (defun helm-rtags-tags-in-this-function ()
;;   "Show tagnames which are referenced in this function and jump to it."
;;   (interactive)
;;   (let ((tags (helm-rtags--tags-refered-from-this-function)))
;;     (unless tags
;;       (error "There are no tags which are refered from this function."))
;;     (let* ((name (format "Tags in [%s]" (which-function)))
;;            (tag (helm-comp-read
;;                  "Tagnames: " tags
;;                  :must-match t :name name
;;                  :persistent-action 'helm-rtags--tag-in-function-persistent-action)))
;;       (helm-rtags-find-tag tag))))

;; (defun helm-rtags--source-select-tag (candidate)
;;   (helm-build-in-buffer-source "Select Tag"
;;     :init (lambda () (helm-rtags--tags-init candidate))
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action helm-rtags--find-file-action))

;; (defun helm-rtags--source-select-rtag (candidate)
;;   (helm-build-in-buffer-source "Select Rtag"
;;     :init (lambda () (helm-rtags--rtags-init candidate))
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action helm-rtags--find-file-action))

;; (defun helm-rtags--select-tag-action (c)
;;   (helm-run-after-quit
;;    (lambda ()
;;      (helm-rtags--common (list (helm-rtags--source-select-tag c)) nil))))

;; (defun helm-rtags--select-rtag-action (c)
;;   (helm-run-after-quit
;;    (lambda ()
;;      (helm-rtags--common (list (helm-rtags--source-select-rtag c)) nil))))

;; (defun helm-rtags--select-cache-init-common (args tagfile)
;;   (let ((cache (helm-rtags--get-result-cache tagfile)))
;;     (if cache
;;         (insert cache)
;;       (apply 'process-file "global" nil t nil args)
;;       (let* ((cache (buffer-string))
;;              (cache-size (length cache)))
;;         (when (<= cache-size helm-rtags-cache-max-result-size)
;;           (helm-rtags--put-result-cache tagfile cache))))))

;; (defun helm-rtags--source-select-init ()
;;   (with-current-buffer (helm-candidate-buffer 'global)
;;     (if (not helm-rtags-cache-select-result)
;;         (process-file "global" nil t nil "-c")
;;       (helm-rtags--select-cache-init-common '("-c") "RTAGS"))))

;; (defvar helm-source-rtags-select
;;   (helm-build-in-buffer-source "Find tag from here"
;;     :init 'helm-rtags--source-select-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action '(("Goto the location" . helm-rtags--select-tag-action)
;;               ("Goto the location(other buffer)" .
;;                (lambda (c)
;;                  (setq helm-rtags--use-otherwin t)
;;                  (helm-rtags--select-tag-action c)))
;;               ("Move to the referenced point" . helm-rtags--select-rtag-action))))

;; (defun helm-rtags--select-path-init ()
;;   (with-current-buffer (helm-candidate-buffer 'global)
;;     (if (not helm-rtags-cache-select-result)
;;         (process-file "global" nil t nil "-Poa")
;;       (helm-rtags--select-cache-init-common '("-Poa") "GPATH"))))

;; (defvar helm-source-rtags-select-path
;;   (helm-build-in-buffer-source "Select path"
;;     :init 'helm-rtags--select-path-init
;;     :candidate-number-limit helm-rtags-maximum-candidates
;;     :real-to-display 'helm-rtags--files-candidate-transformer
;;     :persistent-action 'helm-rtags--persistent-action
;;     :fuzzy-match helm-rtags-fuzzy-match
;;     :action (helm-actions-from-type-file)))


;; (defun helm-rtags--make-rtags-sentinel (action)
;;   (lambda (process _event)
;;     (when (eq (process-status process) 'exit)
;;       (if (zerop (process-exit-status process))
;;           (message "Success: %s TAGS" action)
;;         (message "Failed: %s TAGS(%d)" action (process-exit-status process))))))

;; (defsubst helm-rtags--read-rtagslabel ()
;;   (let ((labels '("default" "native" "ctags" "pygments")))
;;     (completing-read "RTAGSLABEL(Default: default): " labels nil t nil nil "default")))

;; (defsubst helm-rtags--label-option (label)
;;   (concat "--rtagslabel=" label))

;; ;;;###autoload
;; (defun helm-rtags-create-tags (dir label)
;;   (interactive
;;    (list (read-directory-name "Root Directory: ")
;;          (helm-rtags--read-rtagslabel)))
;;   (let ((default-directory dir)
;;         (proc-buf (get-buffer-create " *helm-rtags-create*")))
;;     (let ((proc (start-file-process "helm-rtags-create" proc-buf
;;                                     "rtags" "-q" (helm-rtags--label-option label))))
;;       (set-process-sentinel proc (helm-rtags--make-rtags-sentinel 'create)))))

;; (defun helm-rtags--current-file-and-line ()
;;   (let* ((buffile (buffer-file-name))
;;          (path (cl-case helm-rtags-path-style
;;                  (absolute buffile)
;;                  (root
;;                   (file-relative-name buffile (helm-rtags--find-tag-directory)))
;;                  (relative
;;                   (file-relative-name buffile (helm-rtags--base-directory))))))
;;     (format "%s:%d" path (line-number-at-pos))))

;; ;;;###autoload
;; (defun helm-rtags-find-tag-other-window (tag)
;;   "Jump to definition in other window."
;;   (interactive
;;    (list (helm-rtags--read-tagname 'tag)))
;;   (setq helm-rtags--use-otherwin t)
;;   (helm-rtags-find-tag tag))

;; ;;;###autoload
;; (defun helm-rtags-find-pattern (pattern)
;;   "Grep and jump by rtags tag files."
;;   (interactive
;;    (list (helm-rtags--read-tagname 'pattern)))
;;   (helm-rtags--common '(helm-source-rtags-pattern) pattern))

;; (defun helm-rtags--find-file-after-hook ()
;;   (helm-rtags--push-context helm-rtags--saved-context))

;; ;;;###autoload
;; (defun helm-rtags-find-files (file)
;;   "Find file from tagged with gnu global."
;;   (interactive
;;    (list (helm-rtags--read-tagname 'find-file)))
;;   (add-hook 'helm-after-action-hook 'helm-rtags--find-file-after-hook)
;;   (unwind-protect
;;       (helm-rtags--common '(helm-source-rtags-files) file)
;;     (remove-hook 'helm-after-action-hook 'helm-rtags--find-file-after-hook)))

;; ;;;###autoload
;; (defun helm-rtags-find-tag-from-here ()
;;   "Jump point by current point information.
;; Jump to definition point if cursor is on its reference.
;; Jump to reference point if curosr is on its definition"
;;   (interactive)
;;   (helm-rtags--common '(helm-source-rtags-find-tag-from-here) nil))

;; ;;;###autoload
;; (defun helm-rtags-dwim ()
;;   "Find by context. Here is
;; - on include statement then jump to included file
;; - on symbol definition then jump to its references
;; - on reference point then jump to its definition."
;;   (interactive)
;;   (let ((line (helm-current-line-contents)))
;;     (if (string-match helm-rtags--include-regexp line)
;;         (let ((helm-rtags-use-input-at-cursor t))
;;           (helm-rtags-find-files (match-string-no-properties 1 line)))
;;       (if (and (buffer-file-name) (thing-at-point 'symbol))
;;           (helm-rtags-find-tag-from-here)
;;         (call-interactively 'helm-rtags-find-tag)))))

;; (defun helm-rtags--set-parsed-file ()
;;   (let* ((this-file (file-name-nondirectory (buffer-file-name)))
;;          (file (if current-prefix-arg
;;                    (read-file-name "Parsed File: " nil this-file)
;;                  this-file)))
;;     (setq helm-rtags--parsed-file (expand-file-name file))))

;; (defun helm-rtags--find-preselect-line ()
;;   (let ((defun-bound (bounds-of-thing-at-point 'defun)))
;;     (if (not defun-bound)
;;         (line-number-at-pos)
;;       (let ((defun-begin-line (line-number-at-pos (car defun-bound)))
;;             (filename (helm-rtags--real-file-name)))
;;         (with-temp-buffer
;;           (unless (zerop (process-file "global" nil t nil "-f" filename))
;;             (error "Failed: global -f"))
;;           (goto-char (point-min))
;;           (let (start-line)
;;             (while (and (not start-line)
;;                         (re-search-forward "^\\S-+\\s-+\\([1-9][0-9]*\\)" nil t))
;;               (let ((line (string-to-number (match-string-no-properties 1))))
;;                 (when (>= line defun-begin-line)
;;                   (setq start-line line))))
;;             (or start-line (line-number-at-pos))))))))

;; ;;;###autoload
;; (defun helm-rtags-parse-file ()
;;   "Parse current file with gnu global. This is similar to `imenu'.
;; You can jump definitions of functions, symbols in this file."
;;   (interactive)
;;   (helm-rtags--find-tag-directory)
;;   (helm-rtags--save-current-context)
;;   (setq helm-rtags--use-otherwin (helm-rtags--using-other-window-p))
;;   (helm-rtags--set-parsed-file)
;;   (helm-attrset 'name
;;                 (format "Parsed File: %s"
;;                         (file-relative-name helm-rtags--parsed-file
;;                                             helm-rtags--tag-location))
;;                 helm-source-rtags-parse-file)
;;   (let ((presel (when helm-rtags-preselect
;;                   (format "^\\S-+\\s-+%d\\s-+" (helm-rtags--find-preselect-line)))))
;;     (helm :sources '(helm-source-rtags-parse-file)
;;           :buffer helm-rtags--buffer :preselect presel)))

;; ;;;###autoload
;; (defun helm-rtags-pop-stack ()
;;   "Jump to previous point on the context stack and pop it from stack."
;;   (interactive)
;;   (let* ((context-info (helm-rtags--get-context-info))
;;          (context-stack (plist-get context-info :stack))
;;          (context (pop context-stack)))
;;     (helm-rtags--put-context-stack helm-rtags--tag-location -1 context-stack)
;;     (helm-rtags--move-to-context context)))

;; ;;;###autoload
;; (defun helm-rtags-show-stack ()
;;   "Show current context stack."
;;   (interactive)
;;   (helm-other-buffer 'helm-source-rtags-show-stack
;;                      (get-buffer-create helm-rtags--buffer)))

;; ;;;###autoload
;; (defun helm-rtags-clear-stack ()
;;   "Clear current context stack."
;;   (interactive)
;;   (let ((tag-location (helm-rtags--find-tag-directory)))
;;     (message "Clear '%s' context stack." tag-location)
;;     (remhash tag-location helm-rtags--context-stack)))

;; ;;;###autoload
;; (defun helm-rtags-clear-all-stacks ()
;;   "Clear all context stacks."
;;   (interactive)
;;   (message "Clear all context statks.")
;;   (setq helm-rtags--context-stack (make-hash-table :test 'equal)))

;; (defun helm-rtags--read-tag-directory ()
;;   (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
;;     ;; On Windows, "rtags d:/tmp" work, but "rtags d:/tmp/" doesn't
;;     (directory-file-name (expand-file-name dir))))

;; (defsubst helm-rtags--how-to-update-tags ()
;;   (cl-case (prefix-numeric-value current-prefix-arg)
;;     (4 'entire-update)
;;     (16 'generate-other-directory)
;;     (otherwise 'single-update)))

;; (defun helm-rtags--update-tags-command (how-to)
;;   (cl-case how-to
;;     (entire-update '("global" "-u"))
;;     (generate-other-directory (list "rtags" (helm-rtags--read-tag-directory)))
;;     (single-update (list "global" "--single-update" (helm-rtags--real-file-name)))))

;; (defun helm-rtags--update-tags-p (how-to interactive-p current-time)
;;   (or interactive-p
;;       (and (eq how-to 'single-update)
;;            (buffer-file-name)
;;            (or (not helm-rtags-update-interval-second)
;;                (>= (- current-time helm-rtags--last-update-time)
;;                    helm-rtags-update-interval-second)))))

;; ;;;###autoload
;; (defun helm-rtags-update-tags ()
;;   "Update TAG file. Update All files with `C-u' prefix.
;; Generate new TAG file in selected directory with `C-u C-u'"
;;   (interactive)
;;   (let ((how-to (helm-rtags--how-to-update-tags))
;;         (interactive-p (called-interactively-p 'interactive))
;;         (current-time (float-time (current-time))))
;;     (when (helm-rtags--update-tags-p how-to interactive-p current-time)
;;       (let* ((cmds (helm-rtags--update-tags-command how-to))
;;              (proc (apply 'start-file-process "helm-rtags-update-tag" nil cmds)))
;;         (if (not proc)
;;             (message "Failed: %s" (mapconcat 'identity cmds " "))
;;           (set-process-sentinel proc (helm-rtags--make-rtags-sentinel 'update))
;;           (setq helm-rtags--last-update-time current-time))))))

;; ;;;###autoload
;; (defun helm-rtags-resume ()
;;   "Resurrect previously invoked `helm-rtags` command."
;;   (interactive)
;;   (unless (get-buffer helm-rtags--buffer)
;;     (error "Error: helm-rtags buffer is not existed."))
;;   (helm-resume helm-rtags--buffer))

;; (defsubst helm-rtags--check-browser-installed (browser)
;;   (let ((used-browser (or browser "mozilla")))
;;     (unless (executable-find used-browser)
;;       (error "Not found browser '%s'" used-browser))))

;; (defun helm-rtags-display-browser ()
;;   "Display current screen on hypertext browser.
;; `browse-url-generic-program' is used as browser if its value is non-nil.
;; `mozilla' is used in other case."
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (if (not file)
;;         (error "This buffer is not related to file.")
;;       (let* ((lineopt (concat "+" (number-to-string (line-number-at-pos))))
;;              (browser (symbol-value 'browse-url-generic-program))
;;              (args (list lineopt file)))
;;         (helm-rtags--check-browser-installed browser)
;;         (when browser
;;           (setq args (append (list "-b" browser) args)))
;;         ;; `gozilla' commend never returns error status if command is failed.
;;         (apply 'process-file "gozilla" nil nil nil args)))))

;; (defvar helm-rtags-mode-name " HelmRtags")
;; (defvar helm-rtags-mode-map (make-sparse-keymap))

;; ;;;###autoload
;; (define-minor-mode helm-rtags-mode ()
;;   "Enable helm-rtags"
;;   :group      'helm-rtags
;;   :init-value nil
;;   :global     nil
;;   :keymap     helm-rtags-mode-map
;;   :lighter    helm-rtags-mode-name
;;   (if helm-rtags-mode
;;       (when helm-rtags-auto-update
;;         (add-hook 'after-save-hook 'helm-rtags-update-tags nil t))
;;     (when helm-rtags-auto-update
;;       (remove-hook 'after-save-hook 'helm-rtags-update-tags t))))

;; ;; Key mapping of rtags-mode.
;; (when helm-rtags-suggested-key-mapping
;;   ;; Current key mapping.
;;   (let ((prefix helm-rtags-prefix-key))
;;     (define-key helm-rtags-mode-map (concat prefix "h") 'helm-rtags-display-browser)
;;     (define-key helm-rtags-mode-map "\C-]" 'helm-rtags-find-tag-from-here)
;;     (define-key helm-rtags-mode-map "\C-t" 'helm-rtags-pop-stack)
;;     (define-key helm-rtags-mode-map (concat prefix "P") 'helm-rtags-find-files)
;;     (define-key helm-rtags-mode-map (concat prefix "f") 'helm-rtags-parse-file)
;;     (define-key helm-rtags-mode-map (concat prefix "g") 'helm-rtags-find-pattern)
;;     (define-key helm-rtags-mode-map (concat prefix "s") 'helm-rtags-find-symbol)
;;     (define-key helm-rtags-mode-map (concat prefix "r") 'helm-rtags-find-rtag)
;;     (define-key helm-rtags-mode-map (concat prefix "t") 'helm-rtags-find-tag)
;;     (define-key helm-rtags-mode-map (concat prefix "d") 'helm-rtags-find-tag)

;;     ;; common
;;     (define-key helm-rtags-mode-map "\e*" 'helm-rtags-pop-stack)
;;     (define-key helm-rtags-mode-map "\e." 'helm-rtags-find-tag)
;;     (define-key helm-rtags-mode-map "\C-x4." 'helm-rtags-find-tag-other-window)))

;; (provide 'helm-rtags)

;; ;; Local Variables:
;; ;; coding: utf-8
;; ;; indent-tabs-mode: nil
;; ;; End:

;; ;;; helm-rtags.el ends here
