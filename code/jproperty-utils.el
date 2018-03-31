(require 'fwrapper)
(require 'seq)

(defun jproperty-utils--drop-last-element (path)
  "Remove the last component from the path, until the path becomes a \"\"

Works only on absolute files"
  (if (not (= (aref path 0) ?/))
      (error "drop-last-elemnt works only with absolute paths")
    (mapconcat 'identity (reverse (cdr (reverse (split-string path "/")))) "/")))

(defun jproperty-utils--drop-elements-until-directory (path)
  (setq path (jproperty-utils--drop-last-element path))
  (while (or (equal "" path)
             (not (file-accessible-directory-p path)))
    (setq path (jproperty-utils--drop-last-element path)))
  path)

(defun jproperty-utils-tree-common-ancestor-p (path)
  "Check whether the current path contains both a resources and a java path"
  (= 2 (length (seq-filter (lambda (component)
                      (or (equal component "resources")
                          (equal component "java")))
                    (directory-files path)))))

(defun jproperty-utils--find-code-basepath (path)
  "Find the first directory that contains both a \"java\" and \"resources\" directory"
  (setq path (jproperty-utils--drop-elements-until-directory path)) ; remove the filename
  (let ((path (file-truename path)))
    (while (not (jproperty-utils-tree-common-ancestor-p path))
      (setq path (jproperty-utils--drop-last-element path))
      (if (equal path "") (error "no resources path ")))
    path))

(defun jproperty-utils--current-code-basepath ()
  (file-name-as-directory (jproperty-utils--find-code-basepath (file-truename (buffer-file-name)))))

(defun jproperty-utils--java-subtree ()
  (concat (jproperty-utils--current-code-basepath) "java"))

(defun jproperty-utils--resource-subtree ()
  (concat (jproperty-utils--current-code-basepath) "resources"))

(defun jproperty-utils-is-file-p (path)
  "Return the path if associated with a readable and writable non link file, nil otherwise"
  (and (file-readable-p path)
       (file-writable-p path)
       (not (file-directory-p path))))

(defun jproperty-utils-resource-as-path (resource-part)
  "Interpret the resource as a relative path, and return the absolute path, whether it makes sense or not."
  "Return the resource as a path relative to the current basepath"
  (concat (file-name-as-directory
           (jproperty-utils--resource-subtree))
          resource-part))

(defun jproperty-utils-kill-line ()
  "kill a whole line"
  (save-excursion
    (beginning-of-line)
    (kill-line 1)))

(defun jproperty-utils-delete-file (path)
  (condition-case nil
      (not (delete-file path))
    (error nil)))

(defun jproperty-utils-remove-line-and-file (path)
  "Remove the path and, on success, the line.

Fails with an error if the file cannot be deleted"
  (if (jproperty-utils-delete-file path)
      (jproperty-utils-kill-line)
    (error (format "Unable to remove file '%s' associated with resource" path))))

(defun jproperty-utils--current-resource-limits ()
  "Read the current line and return a cons cell with the key and value"
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (goto-char (point-min))
     (save-match-data
       (when (search-forward-regexp "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.*?\\)[[:blank:]]*$" nil t)
         (list (match-beginning 1) (match-end 1)
               (match-beginning 2) (match-end 2)))))))

(defun jproperty-utils--parse-current-resource ()
  "Read the current line and return a cons cell with the key and value"
  (let ((limits (jproperty-utils--current-resource-limits)))
    (if limits
        (cons (buffer-substring (nth 0 limits) (nth 1 limits))
              (buffer-substring (nth 2 limits) (nth 3 limits))))))

(defun jproperty-utils-valid-resource-p ()
  "Returns a cons cell with resource key and value for valid non empty resources, nil otherwise"
  (jproperty-utils--parse-current-resource))

(defun jproperty-utils-matching-lines-in-buffer (regex)
  (let ((accumulator nil)
        (counter 1))
    (seql-for-each-line (lambda (line)
                          (setq counter (+ counter 1))
                          (when (string-match-p regex line)
                            (setq accumulator (append accumulator (cons counter line))))))))

(defun jproperty-utils-matching-lines-for-file (path regex)
  "Return a buffer visiting the path, or error if impossible to do so"
  (with-current-buffer (find-file-noselect path t t)
    (goto-char (point-min))
    (jproperty-utils-matching-lines-in-buffer regex)))

(defun jproperty-utils-full-key-matches-in-file (path key)
  (jproperty-utils-matching-lines-for-file path
                                  (regexp-quote (concat "\"" key "\""))))

(defun jproperty-utils-tail-key-matches-in-file (path key)
  (jproperty-utils-matching-lines-for-file path
                                  (regexp-quote (concat key "\""))))

(defun jproperty-utils-dependency-of-key-from-path-p (path key)
  "Return nil if no file has dependencies from the current key

If some dependency is found, a list of them is returned (to be better defined…)"
  (let ((filename (file-name-base path)))
    (mapcar (lambda (x) (concat filename ":" x))
            (append (jproperty-utils-full-key-matches-in-file path key)
                    (jproperty-utils-tail-key-matches-in-file path key)))))

(defun jproperty-utils-java-file-p (path)
  (string-match-p "\\.java$" path))

(defun jproperty-get-all-java-files ()
  (seq-filter 'jproperty-utils-java-file-p (fwrapper-all-files (jproperty-utils--java-subtree))))

(defun jproperty-utils-java-dependencies-p (key)
  "Return not nil if the key is referenced (with some logic) in a java file"
  (let ((all-java-files (jproperty-get-all-java-files))
        (references nil))
    (while all-java-files
      (setq references (append references (jproperty-utils-dependency-of-key-from-path-p (car all-java-files) key)))
      (setq all-java-files (cdr all-java-files)))
    references))

(provide 'jproperty-utils)
