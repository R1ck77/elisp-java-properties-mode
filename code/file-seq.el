(require 'expcache)
(require 'seq)

(defvar file-seq--files-cache nil "cache with the list of files in the java directory")
(make-variable-buffer-local 'file-seq--files-cache)

(defun file-seq--safe-directory-file-names (path)
  (condition-case nil
      (directory-files path)
    (error nil)))

(defun file-seq--safe-directory-files (path)
  (mapcar (lambda (file-name)
            (concat (file-name-as-directory path) file-name))
          (file-seq--safe-directory-file-names path)))

(defun file-seq--readable-file-p (path &optional accept-links)
  "t if the file is readable and is not a directory or a link, if the case"
  (and (file-readable-p path)
       (not (file-directory-p path))
       (or accept-links (not (file-symlink-p path)))))

(defun file-seq--recursable-directory-p (path)
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

(defun file-seq--for-all-files (path consumer)
  (if (not (file-directory-p path))
      (funcall consumer path)
    (let* ((contents (file-seq--safe-directory-files path))
           (directories (seq-filter 'file-seq--recursable-directory-p contents)))
      ;;; name this next expression
      (mapc (lambda (f)
              (funcall consumer f))
            (seq-filter 'file-seq--readable-file-p contents))
      (while directories
        (file-seq--for-all-files (car directories) consumer)
        (setq directories (cdr directories)))))
  nil)

(defun file-seq-for-all-files (path consumer)
  (file-seq--for-all-files (file-truename path) consumer))

(defun file-seq-all-files (path)
  (let ((result nil))
    (file-seq-for-all-files path (lambda (a-file)
                                   (setq result (cons a-file result))))
    result))

;;; the fact that the find version and the custom version file count doesn't match is mildly unsettling
(message "WARNING!!!! file-seq needs some serious debugging")
(provide 'file-seq)
