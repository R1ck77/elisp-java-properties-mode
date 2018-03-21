(require 'seq)

(defvar expiring-cache-max-cache-age-seconds 120 "Maximum amount of time a cache can be alive since creation")


(defun expiring-cache-new-cache (&optional expire-time)
  (seq-copy (cons (list 'meta (if (null expire-time)
                                  expiring-cache-max-cache-age-seconds
                                expire-time))
                  '(values nil))))

(defun expiring-cache--set-values (cache new-values)
  (setcdr (cdr cache)
          (list new-values)))

(defun expiring-cache--should-keep (now max-time log-removals key save-time value)
  (if (> (- now save-time) max-time)
      (progn
        (if log-removals (message (concat "Removing expired key '" key "'…")))
        nil)
    t))

(defun expiring-cache--clean-values (values max-time now log-removals)
  "Remove all values older than the timeout"
  (seq-filter (lambda (cache-value)
                (apply 'expiring-cache--should-keep now max-time log-removals cache-value)) values))


(defun expiring-cache--get-values (cache)
  "Get the values from the cache"
  (car (cdr (cdr cache))))

(defun expiring-cache--get-max-time (cache)
  "Get the maximum life of an entry"
  (car (cdr (car cache))))


(defun expiring-cache--clean (cache log-removals)
  "Remove stale entries from the cache"
  (let ((now (float-time))
        (max-time (expiring-cache--get-max-time cache))
        (values (expiring-cache--get-values cache)))
    (if (and values (>= max-time 0))
        (expiring-cache--set-values cache
                                    (expiring-cache--clean-values values
                                                                  max-time
                                                                  now
                                                                  log-removals))))
  cache)

(defun delete-all-keys (key alist)
  (setq alist (seq-filter (lambda (element)
                 (not (equal key (car element))))
               alist)))

(defun expiring-cache-set (cache key value)
  (let ((now (float-time))
        (values (expiring-cache--get-values cache)))
    (setq values (delete-all-keys key values))
    (expiring-cache--set-values cache (append values (list (list key now value))))
    cache))

(defun expiring-cache-get (cache key)
  (expiring-cache--clean cache t)
  (assoc key (expiring-cache--get-values cache)))

(defun expiring-cache-get-or (cache key value function)
  (let ((result (expiring-cache-get cache key)))
    (if (not result)
        (let ((new-result (funcall function)))
          (expiring-cache-set cache key new-result)
          new-result)
        result)))

(provide 'expiring-cache)
