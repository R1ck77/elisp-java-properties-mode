
(defun jproperty-smart-delete-resource ()
  (interactive)
  (message "** delete resource stub")
  )

;;; Fontification of resources
(defvar jproperty--fontify-lock-key (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.+?\\)[[:blank:]]*$" 1 font-lock-keyword-face))
(defvar jproperty--fontify-lock-value (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.+?\\)[[:blank:]]*$"  2 font-lock-string-face))
(defvar jproperty--fontify-lock-wrong-resource (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*$"  1 font-lock-warning-face))
(defun jproperty-add-fontify-keywords ()
  (interactive)   ;;; TODO add wrong resources to fontification
  (font-lock-add-keywords nil (list
                                    jproperty--fontify-lock-value
                                    jproperty--fontify-lock-wrong-resource
                                    jproperty--fontify-lock-key
                                    )))

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


