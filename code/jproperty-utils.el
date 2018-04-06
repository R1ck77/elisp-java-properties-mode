(require 'fwrapper)
(require 'seq)
(require 'seql)

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
  "Return all lines matching the specified regex in the current buffer as cons cells"
  (let ((accumulator nil)
        (counter 0))
    (seql-for-each-line-content (lambda (line)
                                  (setq counter (+ counter 1))
                                  (when (string-match-p regex line)
                                    (setq accumulator (append accumulator
                                                              (list (cons counter line)))))))
    accumulator))

(defun jproperty-utils-matching-lines-for-file (path regex)
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (jproperty-utils-matching-lines-in-buffer regex)))

(defun jproperty-utils--turn-cons-into-list-by-adding-symbol (symbol elements)
  ;;; TODO save the lambda beforehand
  (mapcar (lambda (x)
         (list symbol (car x) (cdr x)))
       elements))

(defun jproperty-utils-full-key-matches-in-file (path key)
  "Check if the file contains lines matching the string '\"key\"'

Return list of lists containing the symbol 'full, the filename, the line number and line content"
  (jproperty-utils--turn-cons-into-list-by-adding-symbol 'full (jproperty-utils-matching-lines-for-file path (regexp-quote (concat "\"" key "\"")))))

(defun jproperty-utils-last-key-element (key)
  (car (last (split-string key "[.]"))))

(defun jproperty-utils-tail-key-matches-in-file (path key)
  "Check if the file contains lines matching the string 'last-key-element\"'

last-key-element is the last component of the dot separated key.

Return list of lists containing the symbol 'tail, the filename, the line number and line content"
  (jproperty-utils--turn-cons-into-list-by-adding-symbol 'tail (jproperty-utils-matching-lines-for-file path (regexp-quote (concat (jproperty-utils-last-key-element key) "\"")))))

(defun jproperty-utils-add-file-to-result (filename result)
  "Turn a result of type (symbol line-num line-content) into (symbol filename line content)"
  (list (first result) filename (nth 1 result) (nth 2 result)))

(defun jproperty-utils-dependency-of-key-from-path-p (path key)
  "Return nil if no file has dependencies from the current key

If some dependency is found, a list of them is returned (to be better defined…)"
  (let ((filename (file-name-nondirectory path))) ;;; don't create a lambda at every turn!a
    (mapcar (lambda (result) (jproperty-utils-add-file-to-result filename result))
            (append (jproperty-utils-full-key-matches-in-file path key)
                    (jproperty-utils-tail-key-matches-in-file path key)))))

(defun jproperty-utils-java-file-p (path)
  (string-match-p "\\.java$" path))

(defun jproperty-utils-get-all-java-files ()
  (seq-filter 'jproperty-utils-java-file-p (fwrapper-all-files (jproperty-utils--java-subtree))))

(defun jproperty-utils-java-dependencies-p (key)
  "Return not nil if the key is referenced (with some logic) in a java file"
  (if (null key)
      nil
      (let ((all-java-files (jproperty-utils-get-all-java-files))
            (references nil))
        (while all-java-files
          (setq references (append references (jproperty-utils-dependency-of-key-from-path-p (car all-java-files) key)))
          (setq all-java-files (cdr all-java-files)))
        references)))

(defun jproperty-utils-apply-font-on-current-line (text-highlighter)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (let ((end (point)))
        (funcall text-highlighter start end)))))

(defun jproperty-utils-remove-fonts-from-line ()
  "Remove all fontification properties from the current line"
  (jproperty-utils-apply-font-on-current-line
   (lambda (start end)
     (remove-text-properties start end '(font-lock-face)))))

(defun jproperty-utils-current-resource-limits ()
  "Read the current line and return a cons cell with the key and value bounds"
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (goto-char (point-min))
     (save-match-data
       (when (search-forward-regexp "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.*?\\)[[:blank:]]*$" nil t)
         (list (match-beginning 1) (match-end 1)
               (match-beginning 2) (match-end 2)))))))

(defun jproperty-utils-mark-resource-key-as-unused ()
  "Mark the current key with the 'unused' mark"
 (let ((limits (jproperty-utils-current-resource-limits)))
    (when limits
      (put-text-property (nth 0 limits) (nth 1 limits)
                         'font-lock-face
                         font-lock-preprocessor-face))))

(defun jproperty-utils-string-with-content-p (string)
  (not (string-match-p "^[ 	]*$" string)))

(defun jproperty-utils-print-dependencies (key dependencies-representation)
  (if (not dependencies-representation)
      (insert (propertize "No dependencies found!" 'face 'bold))
    (insert (propertize (concat "Dependencies for '" key "' (work in progress…):\n\n") 'face 'bold ))
    (mapc (lambda (x)
            (insert (format "%s %s:%s %s\n"
                            (if (eq 'full (nth 0 x))
                                (propertize (symbol-name (nth 0 x)) 'face 'bold)
                              (nth 0 x))                            
                            (propertize (nth 1 x) 'face 'file-name-shadow)
                            (propertize (number-to-string (nth 2 x)) 'face 'linum)
                            (propertize (nth 3 x) 'face 'font-lock-builtin-face))))
          dependencies-representation)))

(defun jproperty-utils-all-resource-keys ()
  "Return a list of all keys present in the property file without check"
  (let ((result nil))
    (seql-for-each-line
     (lambda ()
       (let ((key-value (jproperty-utils--parse-current-resource)))
         (when (car key-value)
           (setq result (cons (substring-no-properties (car key-value)) result ))))))
    result))

(defun jproperty-utils--line-contains-key-p (line key)
  (string-match-p (regexp-quote (concat "\"" key "\"")) line))

(defun jproperty-utils--check-keys-in-line (line keys)
  "Check which keys are referenced (as strings) in the line, return a list of keys"
  (let ((result nil))
    (mapc (lambda (key))
          (when (jproperty-utils--line-contains-key-p line key)
            (setq result (cons key results)))
          keys)))

(defun jproperty-utils--add-file-line-information (filename line-num keys)
  (mapcar (lambda (key)
            (list filename line-num key)) keys))

;;; TODO: change semantic eventually! Use hash-table to store the results (detect duplicate keys!)
(defun jproperty-utils-keys-in-buffer (path keys)
  "Check all lines of the current buffer for keys match.

Returns a list of matching keys"
  (save-match-data
    (let ((result nil)
          (line-num 1)
          (filename (file-name-nondirectory path)))
     (seql-for-each-line-content
      (lambda (line)
        (setq results (append results
                              (jproperty-utils--add-file-line-information filename
                                                                   line-num
                                                                   (jproperty-utils--check-keys-in-line line keys))))
        (setq line-num (+ line-num 1)))))))

(provide 'jproperty-utils)
