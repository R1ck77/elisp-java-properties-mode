(defun seql-for-each-line-bondaries (function)
  (goto-char (point-min))
  (let ((keep-going t))
    (while keep-going
      (setq keep-going (= (forward-line) 0))
      (funcall function (line-beginning-position)
               (line-end-position)))))

(provide 'seql)