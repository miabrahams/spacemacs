;;; helm-tags.el --- Helm for Rtags. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-utils)


(defgroup helm-tags nil
  "Tags related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-rtags-tag-file-name "TAGS"
  "Rtags tag file name."
  :type  'string
  :group 'helm-tags)

(defcustom helm-rtags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'helm-tags)

(defcustom helm-rtags-match-part-only t
  "Whether to match only the tag part of CANDIDATE in
helm-source-rtags-select."
  :type 'boolean
  :group 'helm-tags)

(defcustom helm-rtags-execute-action-at-once-if-one t
  "Whether to jump straight to the selected tag if there's only
one match."
  :type 'boolean
  :group 'helm-tags)


(defgroup helm-tags-faces nil
  "Customize the appearance of helm-tags faces."
  :prefix "helm-"
  :group 'helm-tags
  :group 'helm-faces)

(defface helm-rtags-file
    '((t (:foreground "Lightgoldenrod4"
          :underline t)))
  "Face used to highlight rtags filenames."
  :group 'helm-tags-faces)


;;; Rtags
;;
;;
(defun helm-rtags-run-switch-other-window ()
  "Run switch to other window action from `helm-source-rtags-select'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     (lambda (c)
       (helm-rtags-action-goto 'find-file-other-window c)))))

(defun helm-rtags-run-switch-other-frame ()
  "Run switch to other frame action from `helm-source-rtags-select'."
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action
     (lambda (c)
       (helm-rtags-action-goto 'find-file-other-frame c)))))

(defvar helm-rtags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c o")    'helm-rtags-run-switch-other-window)
    (define-key map (kbd "C-c C-o")  'helm-rtags-run-switch-other-frame)
    map)
  "Keymap used in Rtags.")

(defvar helm-rtags-mtime-alist nil
  "Store the last modification time of rtags files here.")
(defvar helm-rtags-cache (make-hash-table :test 'equal)
  "Cache content of rtags files used here for faster access.")

(defun helm-rtags-get-tag-file (&optional directory)
  "Return the path of rtags file if found.
Lookes recursively in parents directorys for a
`helm-rtags-tag-file-name' file."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (helm-rtags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      (expand-file-name helm-rtags-tag-file-name current-dir))))

(defun helm-rtags-all-tag-files ()
  "Return files from the following sources;
  1) An automatically located file in the parent directories, by `helm-rtags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
  (helm-fast-remove-dups
   (delq nil
         (append (list (helm-rtags-get-tag-file)
                       tags-file-name)
                 tags-table-list))
   :test 'equal))

(defun helm-rtags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (let ((file-exists? #'(lambda (dir)
                          (let ((tag-path (expand-file-name
                                           helm-rtags-tag-file-name dir)))
                            (and (stringp tag-path)
                                 (file-regular-p tag-path)
                                 (file-readable-p tag-path))))))
    (cl-loop with count = 0
          until (funcall file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `helm-rtags-tag-file-search-limit'.
          if (= count helm-rtags-tag-file-search-limit)
          do (cl-return nil)
          ;; Or search upper directories.
          else
          do (cl-incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun helm-rtags-get-header-name (_x)
  "Create header name for this helm rtags session."
  (concat "Rtags in "
          (with-helm-current-buffer
            (helm-rtags-get-tag-file))))

(defun helm-rtags-create-buffer (file)
  "Create the `helm-buffer' based on contents of rtags tag FILE."
  (let* ((tag-fname file)
         max
         (split (with-current-buffer (find-file-noselect tag-fname)
                  (prog1
                      (split-string (buffer-string) "\n" 'omit-nulls)
                    (setq max (line-number-at-pos (point-max)))
                    (kill-buffer))))
         (progress-reporter (make-progress-reporter "Loading tag file..." 0 max)))
    (cl-loop
          with fname
          with cand
          for i in split for count from 0
          for elm = (unless (string-match "^\x0c" i)
                      (helm-aif (string-match "\177" i)
                          (substring i 0 it)
                        i))
          do (cond ((and elm (string-match "^\\([^,]+\\),[0-9]+$" elm))
                    (setq fname (propertize (match-string 1 elm)
                                            'face 'helm-rtags-file)))
                   (elm (setq cand (concat fname ": " elm)))
                   (t (setq cand nil)))
          when cand do (progn
                         (insert (concat cand "\n"))
                         (progress-reporter-update progress-reporter count)))))

(defun helm-rtags-init ()
  "Feed `helm-buffer' using `helm-rtags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfiles (helm-rtags-all-tag-files)))
    (when tagfiles
      (with-current-buffer (helm-candidate-buffer 'global)
        (dolist (f tagfiles)
          (helm-aif (gethash f helm-rtags-cache)
              ;; An entry is present in cache, insert it.
              (insert it)
            ;; No entry, create a new buffer using content of tag file (slower).
            (helm-rtags-create-buffer f)
            ;; Store content of buffer in cache.
            (puthash f (buffer-string) helm-rtags-cache)
            ;; Store or set the last modification of tag file.
            (helm-aif (assoc f helm-rtags-mtime-alist)
                ;; If an entry exists modify it.
                (setcdr it (helm-rtags-mtime f))
              ;; No entry create a new one.
              (add-to-list 'helm-rtags-mtime-alist
                           (cons f (helm-rtags-mtime f))))))))))

(defun helm-rtags-split-line (line)
  (let ((regexp "\\`\\([[:lower:][:upper:]]?:?.*?\\): \\(.*\\)"))
    (when (string-match regexp line)
      (cl-loop for n from 1 to 2 collect (match-string n line)))))

(defvar helm-source-rtags-select nil
  "Helm source for Rtags.")

(defun helm-rtags-build-source ()
  (helm-build-in-buffer-source "Rtags"
    :header-name 'helm-rtags-get-header-name
    :init 'helm-rtags-init
    :get-line 'buffer-substring
    :match-part (lambda (candidate)
                  ;; Match only the tag part of CANDIDATE
                  ;; and not the filename.
                  (if helm-rtags-match-part-only
                      ;; Ignore the first part of the tag
                      ;; which is irrelevant
                      ;;(e.g in "(cl-defun foo" search only if "foo" match)
                      (cadr (split-string
                             (cadr (helm-rtags-split-line candidate))))
                      candidate))
    :help-message 'helm-rtags-help-message
    :keymap helm-rtags-map
    :action '(("Go to tag" . (lambda (c)
                               (helm-rtags-action-goto 'find-file c)))
              ("Go to tag in other window" . (lambda (c)
                                               (helm-rtags-action-goto
                                                'find-file-other-window
                                                c)))
              ("Go to tag in other frame" . (lambda (c)
                                              (helm-rtags-action-goto
                                               'find-file-other-frame
                                               c))))
    :persistent-help "Go to line"
    :persistent-action (lambda (candidate)
                         (helm-rtags-action-goto 'find-file candidate)
                         (helm-highlight-current-line))))

(defvar find-tag-marker-ring)

(defun helm-rtags-action-goto (switcher candidate)
  "Helm default action to jump to an rtags entry in other window."
  (require 'rtags)
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (let* ((split (helm-rtags-split-line candidate))
         (fname (cl-loop for tagf being the hash-keys of helm-rtags-cache
                      for f = (expand-file-name
                               (car split) (file-name-directory tagf))
                      when (file-exists-p f)
                      return f))
         (elm   (cadr split)))
    (if (null fname)
        (error "file %s not found" fname)
      (ring-insert find-tag-marker-ring (point-marker))
      (funcall switcher fname)
      (goto-char (point-min))
      (search-forward elm nil t)
      (goto-char (match-beginning 0)))))

(defun helm-rtags-mtime (file)
  "Last modification time of rtags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun helm-rtags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file helm-rtags-mtime-alist))))
    (and last-modif
         (/= last-modif (helm-rtags-mtime file)))))


;;;###autoload
(defun helm-rtags-select (arg)
  "Preconfigured helm for rtags.
If called with a prefix argument or if any of the tag files have
been modified, reinitialize cache.

This function aggregates three sources of tag files:

  1) An automatically located file in the parent directories, by `helm-rtags-get-tag-file'.
  2) `tags-file-name', which is commonly set by `find-tag' command.
  3) `tags-table-list' which is commonly set by `visit-tags-table' command."
  (interactive "P")
  (let ((tag-files (helm-rtags-all-tag-files))
        (helm-execute-action-at-once-if-one helm-rtags-execute-action-at-once-if-one)
        (str (if (region-active-p) (buffer-substring-no-properties (region-beginning) (region-end))
               (thing-at-point 'symbol))))
    (if (cl-notany 'file-exists-p tag-files)
        (message "Error: No tag file found. Create with rtags shell command, or visit with `find-tag' or `visit-tags-table'.")
      (cl-loop for k being the hash-keys of helm-rtags-cache
            unless (member k tag-files)
            do (remhash k helm-rtags-cache))
      (mapc (lambda (f)
              (when (or (equal arg '(4))
                        (and helm-rtags-mtime-alist
                             (helm-rtags-file-modified-p f)))
                (remhash f helm-rtags-cache)))
            tag-files)
      (unless helm-source-rtags-select
        (setq helm-source-rtags-select
              (helm-rtags-build-source)))
      (helm :sources 'helm-source-rtags-select
            :keymap helm-rtags-map
            :default (list (concat "\\_<" str "\\_>") str)
            :buffer "*helm rtags*"))))

(provide 'helm-tags)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-tags.el ends here
