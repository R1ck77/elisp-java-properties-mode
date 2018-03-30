(require 'expcache)
(require 'seq)
(require 'cl-seq)

(defvar fwrapper--files-cache nil "cache with the list of files in the java directory")
(make-variable-buffer-local 'fwrapper--files-cache)

(defun fwrapper--safe-directory-file-names (path)
  (condition-case nil
      (directory-files path)
    (error nil)))

(defun fwrapper--safe-directory-files (path)
  (mapcar (lambda (file-name)
            (concat (file-name-as-directory path) file-name))
          (fwrapper--safe-directory-file-names path)))

(defun fwrapper--readable-file-p (path &optional accept-links)
  "t if the file is readable and is not a directory or a link, if the case"
  (and (file-readable-p path)
       (not (file-directory-p path))
       (or accept-links (not (file-symlink-p path)))))

(defun fwrapper--recursable-directory-p (path)
  "t if the directory is useful in a recursive search for files.

It must be:
- a directory
- readable
- not . or ..
- not a symbolic link"
(and (file-directory-p path)
     (file-readable-p path)
     (not (file-symlink-p path))
     (let ((basename (file-name-base path)))
          (not (or (equal "." basename)
                   (equal ".." basename))))))

(defun fwrapper--all-files (path)
  (if (not (file-directory-p path))
      (list path)
    (let* ((contents (fwrapper--safe-directory-files path))
           (files (seq-filter 'fwrapper--readable-file-p contents))
           (directories (seq-filter 'fwrapper--recursable-directory-p contents)))
      (while directories
        (setq files (append files (fwrapper--all-files (car directories))))
        (setq directories (cdr directories)))
      files)))

(defun fwrapper-all-files (path)
  (fwrapper--all-files (file-truename path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO/FIXME don't use find, roll your own!
(defun fwrapper--raw-get-all-java-files (path)
  (shell-command-to-string (format "find %s -type f -iname '*.java' 2>/dev/null"
                                   (file-truename path))))

(defun fwrapper-find-all-java-files (path)
  "Use GNU's find to list all java files in the hierarchy below path

Returns nil if no files is found. Ignore errors."  
  (let ((raw-result (fwrapper--raw-get-all-java-files path)))
    (if (string-match "java" raw-result)
        (split-string raw-result "\n"))))

(defun fwrapper-get-all-java-files (path)
  (if (not fwrapper--files-cache)
      (setq fwrapper--files-cache (expcache-new-cache)))
  (expcache-get-or fwrapper--files-cache
                   "files"
                   (lambda ()
                     (fwrapper-find-all-java-files path))))

;;; the fact that the find version and the custom version file count doesn't match is mildly unsettling
(message "WARNING!!!! fwrapper needs some serious debugging")
(provide 'fwrapper)
