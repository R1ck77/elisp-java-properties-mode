(require 'seq)
(require 'fwrapper)
(require 'seql)

(defvar javares-path-resource-regexp ".+?/.+")

;;; eval
;;;    (setq load-path (cons (file-name-directory (buffer-file-name)) load-path))
;;; before this module to add this directory to the emacs load path

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

(defun javares--find-subtree-path (resources &optional current-path)
  "Returns either the java or the resources base path relative to a specified path, or the current buffer.

Assumes that the correct java and resources directories are siblings."
    (if (not current-path)
      (setq current-path (file-truename (buffer-file-name))))
  (concat (javares--find-code-basepath current-path) (if resources "/resources" "/java")))

(defun javares--current-resources-path (&optional current-path)
  (javares--find-subtree-path t current-path))

(defun javares--current-java-path (&optional current-path)
    (javares--find-subtree-path nil current-path))

(defun javares--resource-path (javares--resource-path &optional resources-base-path error-message)
  "Resolve the relative path \"javares--resource-path\"

It either uses \"resources-base-path\" or the base path found using the current buffer as a reference."
  (if (not resources-base-path)
      (setq resources-base-path (javares--current-resources-path)))
  (let ((candidate-path (concat resources-base-path "/" javares--resource-path)))
    (if (file-readable-p candidate-path)
        candidate-path
      (if error-message
          (progn
            (message (concat "Error locating the resource: \"" candidate-path "\" is not readable!"))
            nil)))))

(defun javares--parse-current-resource ()
  "Read the current line and return a cons cell with the key and value"
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (if (search-forward-regexp "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.*?\\)[[:blank:]]*$" nil t)
          (cons (match-string 1) (match-string 2))))))

(defun javares--mark-line-with-warning ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (let ((end (point)))
        (put-text-property start end 'font-lock-face font-lock-warning-face)))))

(defun javares--unmark-line ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (let ((end (point)))
        (remove-text-properties start end '(font-lock-face))))))

(defun javares-resource-seems-a-path-p (resource)
  "Returns not nil if the resource looks like a path.

The regular expression used to identify a path is stored in javares-path-resource-regexp. "
  (string-match javares-path-resource-regexp resource))

(defun javares--resource-invalid-p ()
  "Returns the full path of the resource at line if the resource is valid, nil otherwise"
  (let ((resource-value (cdr (javares--parse-current-resource))))
    (if (not resource-value)
        "The line is not a syntactically valid resource"
      (if (not (javares-resource-seems-a-path-p resource-value))
          nil
        (if (and (javares-resource-seems-a-path-p resource-value)
                 (not (javares--resource-path resource-value nil t)))
            (format "The resource '%s' looks like a path, but the corresponding file is missing" resource-value))))))

(defun javares-check-resource ()
  (interactive)
  "Check if the resource at line is present. 

Raise an error in case of an invalid resource"
  (let ((check-result (javares--resource-invalid-p)))
    (if (not check-result)
        (javares--unmark-line)
      (javares--mark-line-with-warning)
      (error check-result))))

(defun javares-relevant-java-files ()
  "Return a list of java files that have access to the current resources

It uses the frontier convention that resources and java are subtrees of a common ancestor"
  (fwrapper-get-all-java-files (javares--current-java-path)))

(defun javares--file-contains-string (path string)
  "Returns the index of the string if it could be found, or nil otherwise"
  (save-excursion
    (find-file path)
    (let ((search-result (search-forward string nil t)))
      (kill-buffer (current-buffer))
      search-result)))

(defun javares--last-key-component (key)
  (split-string key "[.]"))

(defun javares--evaluate-file (path key)
  (or (javares--file-contains-string path (concat "\"" key "\""))
      (javares--file-contains-string path (concat key "\""))))

(defun javares--evaluate-sources-against-key (key sources)
  (seq-filter (lambda (path)
                (javares--evaluate-file path key))
              sources))

(defun javares--evaluate-all-sources-against-key (key)
  (javares--evaluate-sources-against-key key (javares-relevant-java-files)))

(defun javares-mark-unreferenced-resource ()
  (interactive)
  (let ((key (car (javares--parse-current-resource))))
    (if (javares--evaluate-all-sources-against-key key)
        (javares--unmark-line)
      (javares--mark-line-with-warning))))

(defun javares-check-line (beginning end)
  (javares-mark-unreferenced-resource))

(defun javares-check-all-resources ()
  (seql-for-each-line-boundaries 'javares-check-line))

(defun javares-remove-resource-with-file ()
  "Remove the resource and the file associated to it"
  (interactive)
  (let* ((parsed-resource (javares--parse-current-resource))
         (resource-file (javares--resource-path (cdr parsed-resource))))
    (delete-file resource-file)
    (beginning-of-line)
    (kill-line 1)))

(defun javares--current-resource-dependencies ()
  ((javares--evaluate-all-sources-against-key (car (javares--parse-current-resource)))))

(defun javares-safe-remove-resource-with-file ()
  "Remove the resource and the file associated with it, only if the resource is not referenced"
  (let ((dependencies (javares--current-resource-dependencies)))
    (if (not dependencies)
        (javares-remove-resource-with-file)
      (message (format "Unable to safely delete the resource, it has dependencies with: %s" dependencies)))))

;;; Font lock customization
(defvar javares--fontify-lock-key (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.*?\\)[[:blank:]]*$" 1 font-lock-keyword-face))
(defvar javares--fontify-lock-value (list "^[[:blank:]]*\\(.*?\\)[[:blank:]]*=[[:blank:]]*\\(.*?\\)[[:blank:]]*$"  2 font-lock-string-face))

(defun javares-add-fontify-keywords ()
  (interactive)
  (font-lock-add-keywords nil (list javares--fontify-lock-key
                                    javares--fontify-lock-value)))

(defun javares-remove-fontify-keywords ()
  (interactive)
  (font-lock-remove-keywords nil (list javares--fontify-lock-key
                                       javares--fontify-lock-value)))

;;; periodic check (not yet used)

;;; Timer related stuff. TBD
(defvar idle-timer)
(make-variable-buffer-local 'idle-timer)
(defvar idle-timer-timeout 2)
(make-variable-buffer-local 'idle-timer-timeout)

(defun javares--create-idle-timer ()
  (if idle-timer
      (message "Timer already set!")
      (progn
        (setq idle-timer (run-with-idle-timer idle-timer-timeout t (lambda () (message "** IDLE **")))))))

(defun javares--remove-idle-timer ()
  (if idle-timer
      (progn
        (cancel-timer idle-timer)
        (setq idle-timer nil))
    (message "No timer set!")))

;;; Activation/deactivation code

(defun javares--activate ()
  "Initialize Java Resources mode"
  (javares-add-fontify-keywords))

(defun javares--deactivate ()
  "Finalize Java Resources mode"
  (javares-remove-fontify-keywords))

;;; Boilerplate initialization code
(defvar javares-mode nil "Java resources manipulation minor mode")
(make-variable-buffer-local 'javares-mode)

(defun javares-mode (&optional arg)
  (interactive "P")
  (setq javares-mode
        (if (null arg)
            (not javares-mode)
          (> (prefix-numeric-value arg) 0)))
  (if javares-mode
      (javares--activate)
    (javares--deactivate)))

(if (not (assq 'javares-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(javares-mode " JAVAres")  minor-mode-alist)))
