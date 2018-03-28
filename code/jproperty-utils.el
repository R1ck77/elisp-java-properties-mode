(defun jproperty-utils-is-file-p (path)
  "Return the path if associated with a readable and writable non link file, nil otherwise"
  
  )

(defun jproperty-utils-resource-as-path (resource-part)
  "Interpret the resource as a relative path, and return the absolute path, whether it makes sense or not."
  
  )

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
