;;; time-tracker.el --- Time tracker -*- lexical-binding: t; -*-

(defvar time-tracker-idle-threshold 300
  "Threshold in seconds for considering the user idle.")

(defvar time-tracker-log-file (locate-user-emacs-file "time-tracker")
  "File where time tracking logs are stored.")

(defvar time-tracker--timer nil
  "Timer object for tracking user activity.")

(defvar time-tracker--start-time nil
  "Time when the time tracker was started.")

(defvar time-tracker--current-buffer-id nil
  "Buffer currently being tracked.")

(defun time-tracker--buffer-id ()
  "Get the identifier for the current buffer."
  (cond
   ((buffer-file-name) (abbreviate-file-name (buffer-file-name)))
   ((buffer-name) (buffer-name))
   (t "unknown")))

(defun time-tracker--buffer-active ()
  "Function called when the user starts interacting with a buffer."
  (unless time-tracker--start-time
      (setf time-tracker--start-time (current-time))
      (setf time-tracker--current-buffer-id (time-tracker--buffer-id))))

(defun time-tracker--buffer-inactive ()
  "Function called when the user stops interacting with a buffer."
  (when time-tracker--start-time
    (write-region
     (format "%.2f,%.2f,%s,%s\n"
             (float-time time-tracker--start-time)
             (float-time (current-time))
             time-tracker--current-buffer-id
             major-mode)
     nil time-tracker-log-file t 'silent)
    (setf time-tracker--start-time nil)
    (setf time-tracker--current-buffer-id nil)))

(defun time-tracker--buffer-switch (frame)
  "Function called when the user switches buffers."
  (time-tracker--buffer-inactive)
  (time-tracker--buffer-active))

(defun time-tracker--tick ()
  "Function called periodically to check user activity."
  (let ((idle-time (float-time (or (current-idle-time) 0))))
    (if (> idle-time time-tracker-idle-threshold)
        (time-tracker--buffer-inactive)
      (time-tracker--buffer-active))))

(defun time-tracker-stats ()
  "Display time tracking statistics."
  (interactive)
  (with-output-to-temp-buffer "*Time Tracker Stats*"
    (pop-to-buffer "*Time Tracker Stats*")
    (when (file-exists-p time-tracker-log-file)
      (let ((data (with-temp-buffer
                    (insert-file-contents time-tracker-log-file)
                    (split-string (buffer-string) "\n" t)))
            (stats (make-hash-table :test 'equal)))
        (dolist (line data)
          (let ((parts (split-string line ",")))
            (when (= (length parts) 4)
              (let* ((start-time (string-to-number (nth 0 parts)))
                     (end-time (string-to-number (nth 1 parts)))
                     (buffer-id (nth 2 parts))
                     ;; (mode (nth 3 parts))
                     (duration (- end-time start-time)))
                (puthash buffer-id (+ (gethash buffer-id stats 0) duration)
                         stats)))))
        (insert "Time Tracker Statistics\n\n")
        (maphash (lambda (key value)
                   (insert (format "%-75s%8.0fs\n" key value)))
                 stats)))))

(defun time-tracker-start ()
  "Start the time tracker."
  (interactive)
  (add-hook 'window-selection-change-functions #'time-tracker--buffer-switch)
  (setf time-tracker--start-time (current-time))
  (setf time-tracker--current-buffer-id (time-tracker--buffer-id))
  (setq time-tracker--timer (run-at-time t 1 #'time-tracker--tick)))

(defun time-tracker-stop ()
  "Stop the time tracker."
  (interactive)
  (remove-hook 'window-selection-change-functions #'time-tracker--buffer-switch)
  (when time-tracker--timer
    (cancel-timer time-tracker--timer)
    (setq time-tracker--timer nil)))

(define-minor-mode time-tracker-mode
  "Minor mode for tracking user activity and idle time."
  :global t
  (if time-tracker-mode (time-tracker-start) (time-tracker-stop)))
