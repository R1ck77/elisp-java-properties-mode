(require 'seq)

(defun javares--contains-java-and-resources-p (path)
  "Check whether the current path contains both a resources and a java path"
  (= 2 (length (seq-filter (lambda (component)
                      (or (equal component "resources")
                          (equal component "java")))
                    (directory-files path)))))

(defun javares--drop-last-element (path)
  "Remove the last component from the path, until the path becomes a \"\"

Works only on absolute files"
  (if (not (= (aref path 0) ?/))
      (error "drop-last-elemnt works only with absolute paths")
    (mapconcat 'identity (reverse (cdr (reverse (split-string path "/")))) "/")))

(defun javares--drop-elements-until-directory (path)
  (setq path (javares--drop-last-element path))
  (while (or (equal "" path)
             (not (file-accessible-directory-p path)))
    (setq path (javares--drop-last-element path)))
  path)

(defun javares--find-code-basepath (path)
  "Find the first directory that contains both a \"java\" and \"resources\" directory"
  (setq path (javares--drop-elements-until-directory path)) ; remove the filename
  (let ((path (file-truename path)))
    (while (not (javares--contains-java-and-resources-p path))
      (setq path (javares--drop-last-element path))
      (if (equal path "") (error "no resources path ")))
    path))

(defun javares--find-code-basepath (path)
  "Find the first directory that contains both a \"java\" and \"resources\" directory"
  (setq path (javares--drop-elements-until-directory path)) ; remove the filename
  (let ((path (file-truename path)))
    (while (not (javares--contains-java-and-resources-p path))
      (setq path (javares--drop-last-element path))
      (if (equal path "") (error "no resources path ")))
    path))

(defun javares--java-subtree ()
  (concat (javares--find-code-basepath (file-truename (buffer-file-name)))
          "/java"))

(defun javares--resource-subtree ()
  (concat (javares--find-code-basepath (file-truename (buffer-file-name)))
          "/resources"))

;;;; HIC SUNT LEONES

(defun jproperty-utils-is-file-p (path)
  "Return the path if associated with a readable and writable non link file, nil otherwise"
  (and (file-readable-p path)
       (file-writable-p path)
       (not (file-directory-p path))))

(defun jproperty-utils-resource-as-path (resource-part)
  "Interpret the resource as a relative path, and return the absolute path, whether it makes sense or not."
  "Return the resource as a path relative to the current basepath"
  (candidate-path (concat
                   (file-name-as-directory
                    (javares--resource-subtree))
                   javares--resource-path)))

(defun jproperty-utils-kill-line ()
  "kill a whole line"
  (save-excursion
    (beginning-of-line)
    (kill-line 1)))

(defun jproperty-utils-remove-line-and-file (path)
  "Remove the path and, on success, the line.

Fails with an error if the file cannot be deleted"
  
  )

(defun jproperty-utils-valid-resource-p ()
  "Returns a cons cell with resource key and value for valid non empty resources, nil otherwise"
 
  )

(defun jproperty-utils-java-dependencies-p (key)
  "Return not nil if the key is referenced (with some logic) in a java file"
  )


(provide 'jproperty-utils)
