(require 'jproperty-utils)

;;; eval
;;;    (setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;; before this module to add this directory to the emacs load path

(defun jproperty-smart-delete-resource (check-java-dependencies)
  "Delete the resource with the file, if present.

If an argument is provided, also verify that the resource doesn't have any weak reference in the java files, 
and if that's the case, stop with an error"
  (interactive)
  (message "** delete resource stub")
  (let ((key-value (jproperty-utils-valid-resource-p)))
    (if (key-value)
        ;;; Valid resources
        (let ((resource-file (jproperty-utils-resource-as-path (cdr key-value))))
          (if (jproperty-utils-is-file-p resource-file)
              ;;; Resources with an associated file require also the file to be deleted
              (jproperty-utils-remove-line-and-file resource-file)
            ;;; Resources not associated with files just need the line removed
            (jproperty-utils-kill-line)))
      ;;; Invalid resource lines are deleted without question
      (jproperty-utils-kill-line))))

;;; Fontification of resources
(defvar jproperty--fontify-lock-key (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.+?\\)[[:blank:]]*$" 1 font-lock-keyword-face))
(defvar jproperty--fontify-lock-value (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.+?\\)[[:blank:]]*$"  2 font-lock-string-face))
(defvar jproperty--fontify-lock-wrong-resource (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*$"  1 font-lock-warning-face))

(defun jproperty-add-fontify-keywords ()
  (interactive)   ;;; TODO add wrong resources to fontification
  (font-lock-add-keywords nil (list jproperty--fontify-lock-value
                                    jproperty--fontify-lock-wrong-resource
                                    jproperty--fontify-lock-key)))

;;; Mode boilerplate code
(defvar jproperty-mode-hook nil "*Hooks to execute upon activating jproperty mode")

(defvar jproperty-mode-map nil "Keymap for jproperty majore mode")
(when (not jproperty-mode-map)
  (setq jproperty-mode-map (make-keymap))
  (define-key jproperty-mode-map "\C-c\C-k" 'jproperty-smart-delete-resource))

(defun jproperty-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'jproperty-mode)
  (setq mode-name "JavaProperties")
  (use-local-map jproperty-mode-map)
  (run-hooks 'jproperty-mode-hook)
  (jproperty-add-fontify-keywords))

(provide 'jproperty)


