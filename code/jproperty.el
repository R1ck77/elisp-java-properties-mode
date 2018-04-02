(require 'jproperty-utils)

;;; eval
;;;    (setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;; before this module to add this directory to the emacs load path
;;; Useful evaluation after loading the file
;;;    (add-hook 'find-file-hook 'jproperty-find-file-hook)

(defun jproperty-smart-delete-resource (&optional check-skip-java-dependencies-test)
  "Delete the resource with the file, if present.

If an argument is provided, also verify that the resource doesn't have any weak reference in the java files, 
and if that's the case, stop with an error"
  (interactive "P")
  (let ((key-value (jproperty-utils-valid-resource-p)))
    (if key-value
        ;;; Valid resources
        (let ((resource-file (jproperty-utils-resource-as-path (cdr key-value))))
          (if (jproperty-utils-is-file-p resource-file)
              (if (and (not check-skip-java-dependencies-test)
                       (jproperty-utils-java-dependencies-p (car key-value)))
                  (error (format "I won't remove referenced resource '%s'" (car key-value)))
                ;;; Valid resources when removed should remove the corresponding file
                (jproperty-utils-remove-line-and-file resource-file))
            ;;; Resources not associated with files just need the line removed
            (jproperty-utils-kill-line)))
      ;;; Invalid resource lines are deleted without question
      (jproperty-utils-kill-line))))

;;; TODO weak/non weak referencing difference not accounted yet
(defun jproperty-check-key-of-current-property ()
  "Check the property in the current line.

Mark the line if the property is not referenced by any java file, or if it's only weakly referenced.

Unreferened: font-lock-preprocessor-face
Weakly referenced (when implemented): font-lock-variable-name-face"
  (interactive)
  (let ((key-value (jproperty-utils-valid-resource-p)))
    (if (and key-value (jproperty-utils-string-with-content-p (cdr key-value)))
        (if (jproperty-utils-java-dependencies-p (car key-value))
            (jproperty-utils-remove-fonts-from-line)
          (jproperty-utils-mark-resource-key-as-unused)))))

(defun jproperty-check-all-keys-in-file ()
  "Check all properties for unused keys"
  (interactive)
  (seql-for-each-line (lambda ()
                        (jproperty-check-key-of-current-property))))

(defun jproperty-show-key-dependencies ()
  "Show how the current property is referenced in code"
  (interactive)
  (let ((dependencies (jproperty-utils-java-dependencies-p (car (jproperty-utils-valid-resource-p)))))
    (with-output-to-temp-buffer "Key dependencies"
      (jproperty-utils-format-dependencies dependencies))))

;;; Hook utility function
(defun jproperty-find-file-hook ()
  "Java property mode when opening files ending with .resources and .properties"
  (let ((extension (file-name-extension (buffer-name))))
    (when (or (string= "resources" extension)
              (string= "properties" extension))
      (jproperty-mode))))

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
  (define-key jproperty-mode-map "\C-c\C-k" 'jproperty-smart-delete-resource)
  (define-key jproperty-mode-map "\C-c\C-v" 'jproperty-check-key-of-current-property)
  (define-key jproperty-mode-map "\C-c\C-a" 'jproperty-check-all-keys-in-file)
  (define-key jproperty-mode-map "\C-c\C-s" 'jproperty-show-key-dependencies))

(defun jproperty-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'jproperty-mode)
  (setq mode-name "JavaProperties")
  (use-local-map jproperty-mode-map)
  (run-hooks 'jproperty-mode-hook)
  (jproperty-add-fontify-keywords))

(provide 'jproperty)
