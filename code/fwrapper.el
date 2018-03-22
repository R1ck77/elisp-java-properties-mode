(require 'expcache)

(defvar fwrapper--files-cache nil "cache with the list of files in the java directory")
(make-variable-buffer-local 'fwrapper--files-cache)


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

(provide 'fwrapper)
