;;; time-tracker.el --- Time tracker -*- lexical-binding: t; -*-

;; Copyright (C) 2026  rs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'vc-git)

(defvar time-tracker-idle-threshold 300
  "Threshold in seconds for considering the user idle.")

(defvar time-tracker-log-file (locate-user-emacs-file "time-tracker")
  "File where time tracking logs are stored.")

(defvar time-tracker--timer nil
  "Timer object for tracking user activity.")

(defvar time-tracker--context nil
  "Context for the current tracking buffer.")

(defun time-tracker--git-repo ()
  "Get the Git URI for the current buffer, if applicable."
  (let ((uri (vc-git-repository-url default-directory)))
    (when (string-match
           "\\(?:git@[^:]+:\\|https?://[^/]+/\\)\\(.+?\\)\\(?:\\.git\\)?$"
           uri)
      (match-string 1 uri))))

(defun time-tracker--buffer-name ()
  "Get the identifier for the current buffer."
  (cond
   ((and (vc-git-root default-directory) (or (buffer-file-name)
                                             (eq major-mode 'dired-mode)))
    (concat (file-relative-name (or (buffer-file-name) default-directory)
                                (vc-git-root default-directory))
            "<git:" (time-tracker--git-repo) ">"))
   ((eq major-mode 'dired-mode) (abbreviate-file-name default-directory))
   ((buffer-file-name) (abbreviate-file-name (buffer-file-name)))
   ((buffer-name) (buffer-name))
   (t "unknown")))

(defun time-tracker--project-name ()
  "Get the current project name, if applicable."
  (cond
   ((vc-git-root default-directory) (concat "git:" (time-tracker--git-repo)))
   ((project-current) (project-root (project-current)))))

(defun time-tracker--update-context ()
  "Update the context for the current tracking buffer."
  (setf time-tracker--context
        `((start . ,(current-time))
          (info . ,(format "%s,%s,%s"
                           (or (time-tracker--buffer-name) "")
                           (or (time-tracker--project-name) "")
                           major-mode)))))

(defun time-tracker--buffer-active ()
  "Function called when the user starts interacting with a buffer."
  (unless time-tracker--context (time-tracker--update-context)))

(defun time-tracker--buffer-inactive (&optional end-time)
  "Close the current tracking session.
When END-TIME (float seconds) is given, use it as the session end;
otherwise use the current time."
  (when time-tracker--context
    (let ((end (or end-time (float-time))))
      (write-region
       (format "%.2f,%.2f,%s\n"
               (float-time (alist-get 'start time-tracker--context))
               end
               (alist-get 'info time-tracker--context))
       nil time-tracker-log-file t 'silent)
      (setf time-tracker--context nil))))

(defun time-tracker--buffer-switch (frame)
  "Function called when the user switches buffers."
  (time-tracker--buffer-inactive)
  (time-tracker--buffer-active))

(defun time-tracker--tick ()
  "Function called periodically to check user activity."
  (let ((idle-time (float-time (or (current-idle-time) 0))))
    (if (> idle-time time-tracker-idle-threshold)
        (time-tracker--buffer-inactive
         (+ (- (float-time) idle-time) time-tracker-idle-threshold))
      (time-tracker--buffer-active))))

(defun time-tracker--format-duration (seconds)
  "Format SECONDS into a human-readable string."
  (let* ((hours (floor seconds 3600))
         (minutes (floor (mod seconds 3600) 60))
         (secs (floor (mod seconds 60))))
    (cond
     ((> hours 0) (format "%dh %dm" hours minutes))
     ((> minutes 0) (format "%dm %ds" minutes secs))
     (t (format "%ds" secs)))))

(defun time-tracker-stats ()
  "Display time tracking statistics."
  (interactive)
  (with-output-to-temp-buffer "*Time Tracker Stats*"
    (pop-to-buffer "*Time Tracker Stats*")
    (when (file-exists-p time-tracker-log-file)
      (let* ((data (with-temp-buffer
                     (insert-file-contents time-tracker-log-file)
                     (split-string (buffer-string) "\n" t)))
             (stats (make-hash-table :test 'equal))
             (choices '((buffer . 2) (project . 3) (mode . 4)))
             (group (completing-read "Group by: " choices nil t))
             (index (alist-get (intern group) choices)))
        (dolist (line data)
          (let ((parts (split-string line ",")))
            (when (= (length parts) 5)
              (let* ((start-time (string-to-number (nth 0 parts)))
                     (end-time (string-to-number (nth 1 parts)))
                     (duration (- end-time start-time))
                     (name (nth index parts)))
                (unless (string-empty-p name)
                  (puthash name (+ (gethash name stats 0) duration) stats))))))
        (insert "Time Tracker Statistics\n\n")
        (let* ((stats-alist '())
               (_ (maphash (lambda (k v) (push (cons k v) stats-alist)) stats))
               (sorted-stats (sort stats-alist
                                   (lambda (a b) (> (cdr a) (cdr b))))))
          (dolist (entry sorted-stats)
            (insert (format "%-75s%8s\n"
                            (car entry)
                            (time-tracker--format-duration (cdr entry))))))))))

(defun time-tracker-start ()
  "Start the time tracker."
  (interactive)
  (add-hook 'window-selection-change-functions #'time-tracker--buffer-switch)
  (time-tracker--update-context)
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

;;;; Footer

(provide 'time-tracker)

;;; time-tracker.el ends here
