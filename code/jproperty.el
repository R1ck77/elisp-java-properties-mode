(require 'jproputil)

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
  (let ((key-value (jproputil-valid-resource-p)))
    (if key-value
        ;;; Valid resources
        (let ((resource-file (jproputil-resource-as-path (cdr key-value))))
          (if (jproputil-is-file-p resource-file)
              (if (and (not check-skip-java-dependencies-test)
                       (jproputil-java-dependencies-p (car key-value)))
                  (error (format "I won't remove referenced resource '%s'" (car key-value)))
                ;;; Valid resources when removed should remove the corresponding file
                (jproputil-remove-line-and-file resource-file))
            ;;; Resources not associated with files just need the line removed
            (jproputil-kill-line)))
      ;;; Invalid resource lines are deleted without question
      (jproputil-kill-line))))

;;; TODO weak/non weak referencing difference not accounted yet
(defun jproperty-check-key-of-current-property ()
  "Check the property in the current line.

Mark the line if the property is not referenced by any java file, or if it's only weakly referenced.

Unreferened: font-lock-preprocessor-face
Weakly referenced (when implemented): font-lock-variable-name-face"
  (interactive)
  (let ((key-value (jproputil-valid-resource-p)))
    (if (and key-value (jproputil-string-with-content-p (cdr key-value)))
        (if (jproputil-java-dependencies-p (car key-value))
            (jproputil-remove-fonts-from-line)
          (jproputil-mark-resource-key-as-unused)))))

(defun jproperty-check-all-keys-in-file ()
  "Check all properties for unused keys"
  (interactive)
  (seql-for-each-line (lambda ()
                        (jproperty-check-key-of-current-property))))

(defun jproperty--update-hash-value (result hash-table)
  (let ((key (nth 2 result)))
    ;;; (message (format "Working with the hash %s the current result is %s the result before modification is: %s" key result (gethash key hash-table)))
    (puthash key
             (cons result (gethash key hash-table))
             hash-table)))

(defun jproperty--convert-to-hash-table (results)
  (let ((hash-table (make-hash-table ':test 'equal)))
    (mapc (lambda (result)
            ;;; (message (format "Mapping this result: %s" result))
            (jproperty--update-hash-value result hash-table))
          results)
    hash-table))

(defun jproperty--check-results-for-keys ()
  (let ((all-keys (jproputil-all-resource-keys))
        (results nil))
    (let ((java-files (jproputil-get-all-java-files)))
      (with-temp-buffer
        (while java-files
          (let ((current-file (car java-files)))
            ;;; (message (format "Checking %s" current-file))
             (delete-region (point-min) (point-max))
             (insert-file-contents (car java-files))
             (setq results (append results (jproputil-keys-in-buffer current-file all-keys))))
           (setq java-files (cdr java-files)))))
    (jproperty--convert-to-hash-table results)))

(defun jproperty-check-all-keys-in-file2 ()
  "Check all properties for unused keys"
  (interactive)
  (let ((dependencies (jproperty--check-results-for-keys)))
    (seql-for-each-line (lambda ()
                          (let ((key-value (jproputil--parse-current-resource)))
                            ;;; TODO: move THIS code with modification in the origin code
                            ;;; This part was copied from jproperty-check-key-of-current-property!!!!
                            (message (format "Evaluating %s" (car key-value)))
                            (if (and key-value (jproputil-string-with-content-p (cdr key-value)))
                                (if (> (length (gethash (car key-value) dependencies)) 0)
                                    (jproputil-remove-fonts-from-line)
                                  (jproputil-mark-resource-key-as-unused))))))))

(defun jproperty-show-key-dependencies ()
  "Show how the current property is referenced in code"
  (interactive)
  (let* ((key-value (jproputil-valid-resource-p))
         (dependencies (jproputil-java-dependencies-p (car key-value))))
    (with-output-to-temp-buffer "Key dependencies"
      (with-current-buffer (get-buffer "Key dependencies")
        (jproputil-print-dependencies (car key-value) dependencies)))))

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
  (define-key jproperty-mode-map "\C-c\C-a" 'jproperty-check-all-keys-in-file2)
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
