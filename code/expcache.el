(require 'seq)

(defvar expcache-max-cache-age-seconds 120 "Maximum amount of time a cache can be alive since creation")


(defun expcache-new-cache (&optional expire-time)
  (seq-copy (cons (list 'meta (if (null expire-time)
                                  expcache-max-cache-age-seconds
                                expire-time))
                  '(values nil))))

(defun expcache--set-values (cache new-values)
  (setcdr (cdr cache)
          (list new-values)))

(defun expcache--should-keep (now max-time log-removals key save-time value)
  (if (> (- now save-time) max-time)
      (progn
        (if log-removals (message (format "Removing expired key '%s'…" key)))
        nil)
    t))

(defun expcache--clean-values (values max-time now log-removals)
  "Remove all values older than the timeout"
  (seq-filter (lambda (cache-value)
                (apply 'expcache--should-keep now max-time log-removals cache-value))
              values))


(defun expcache--get-values (cache)
  "Get the values from the cache"
  (car (cdr (cdr cache))))

(defun expcache--get-max-time (cache)
  "Get the maximum life of an entry"
  (car (cdr (car cache))))


(defun expcache--clean (cache log-removals)
  "Remove stale entries from the cache"
  (let ((now (float-time))
        (max-time (expcache--get-max-time cache))
        (values (expcache--get-values cache)))
    (if (and values (>= max-time 0))
        (expcache--set-values cache
                                    (expcache--clean-values values
                                                                  max-time
                                                                  now
                                                                  log-removals))))
  cache)

(defun delete-all-keys (key alist)
  (setq alist (seq-filter (lambda (element)
                 (not (equal key (car element))))
               alist)))

(defun expcache-set (cache key value)
  (let ((now (float-time))
        (values (expcache--get-values cache)))
    (setq values (delete-all-keys key values))
    (expcache--set-values cache (append values (list (list key now value))))
    cache))

(defun expcache-get (cache key)
  (expcache--clean cache t)
  (nth 2 (assoc key (expcache--get-values cache))))

(defun expcache-get-or (cache key function)
  (let ((result (expcache-get cache key)))
    (if (not result)
        (let ((new-result (funcall function)))
          (expcache-set cache key new-result)
          new-result)
        result)))

(provide 'expcache)
