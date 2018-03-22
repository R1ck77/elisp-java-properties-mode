(require 'cl)

(defun seql-for-each-line-boundaries (consumer)
  "Recurse each line in the buffer and apply the function consumer to each one, passing the line boundaries as arguments"
  (save-excursion
    (goto-char (point-min))
    (let ((keep-going t))
      (while keep-going
        (setq keep-going (= (forward-line) 0))
        (funcall consumer (line-beginning-position)
                 (line-end-position))))))

(defun seql-for-each-line-content (consumer)
  "Helper function that applies the con"
  (lexical-let ((consumer consumer))
    (seql-for-each-line-boundaries
     (lambda (begin end)
       (funcall consumer (buffer-substring begin end))))))

(provide 'seql)
