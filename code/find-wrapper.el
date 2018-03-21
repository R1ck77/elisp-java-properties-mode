(require 'expcache)

(defvar find-wrapper--files-cache nil "cache with the list of files in the java directory")
(make-variable-buffer-local 'find-wrapper--files-cache)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO/FIXME don't use find, roll your own!
(defun find-wrapper--raw-get-all-java-files (path)
  (shell-command-to-string (format "find %s -type f -iname '*.java' 2>/dev/null"
                                   (file-truename path))))

(defun find-wrapper-find-all-java-files (path)
  "Use GNU's find to list all java files in the hierarchy below path

Returns nil if no files is found. Ignore errors."  
  (let ((raw-result (find-wrapper--raw-get-all-java-files path)))
    (if (string-match "java" raw-result)
        (split-string raw-result "\n"))))

(defun find-wrapper-get-all-java-files (path)
  (if (not find-wrapper--files-cache)
      (setq find-wrapper--files-cache (expcache-new-cache)))
  (expcache-get-or find-wrapper--files-cache
                   "files"
                   (lambda ()
                     (find-wrapper-find-all-java-files path))))


(provide 'find-wrapper)
