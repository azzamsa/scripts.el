(require 's)
(require 'ts)

(defun save-all-buffers-silently ()
  (save-some-buffers t))

;;;###autoload
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (when (y-or-n-p "Save and kill all other buffers ? ")
    (save-all-buffers-silently)
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Saved & killed %d buffer(s)" killed-bufs))))

(defun aza-today (&optional arg)
  "Insert today's date.

A prefix ARG specifies how many days to move;
negative means previous day.

If region selected, parse region as today's date pivot."
  (interactive "P")
  (let ((date (if (use-region-p)
                  (ts-parse (buffer-substring-no-properties (region-beginning) (region-end)))
                (ts-now)))
        (arg (or arg 0)))
    (if (use-region-p)
        (delete-region (region-beginning) (region-end)))
    (insert (ts-format "%A, %B %e, %Y" (ts-adjust 'day arg date)))))

;;;###autoload
(defun insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (s-join " " (s-split-words (file-name-sans-extension (buffer-name)))))))

;;;###autoload
(defun now ()
  (interactive)
  (insert (format-time-string "%F %H:%M")))

;;;###autoload
(defun hour ()
  (interactive)
  (insert (format-time-string "%H:%M")))

;;;###autoload
(defun rm-mysecrets ()
  "Remove all confidential information."
  (interactive)
  (dolist (pair (list-my-secrets))
    (save-excursion
      (replace-string (car pair) (cdr pair)))))

;;;###autoload
(defun light-set-value ()
  "Set light value directly inside Emacs"
  (interactive)
  (let* ((current-value (s-trim (shell-command-to-string "light -G")))
         (light-value (read-string "Set Value: " current-value)))
    (start-process "" nil "light" "-S" light-value)))

(defun start-mpv (path &optional playlist-p)
  "Start mpv with specified arguments"
  (let* ((default-cmd "mpv --force-window")
         (cmd (if playlist-p
                  (s-append " --loop-playlist --playlist=" default-cmd)
                (s-append " --loop " default-cmd))))
    (message "%s" (s-concat cmd path))
    (call-process-shell-command (s-concat cmd (shell-quote-argument path)) nil 0)))

;;;###autoload
(defun mpv ()
  "Play a file in current line"
  (interactive)
  (start-mpv (dired-get-filename)))

;;;###autoload
(defun mpv-dir ()
  "Play all multimedia files in current directory"
  (interactive)
  (start-mpv default-directory))

;;;###autoload
(defun mpv-playlist ()
  "Play a playlist in current line"
  (interactive)
  (start-mpv (dired-get-filename) t))

;;;###autoload
(defun what-day ()
  "Show day name from spesific time"
  (interactive)
  (let ((date (read-string "Date: "))
        (month (read-string "Month: "))
        (year (read-string "Year: " (number-to-string (ts-year (ts-now))))))
    (message (ts-day-name (ts-parse (s-join " " (list date month year)))))))

(defun play-reminder-sound ()
  (start-process "" nil "mpv" coin-work-medium-sound))

;;;###autoload
(defun remind-me ()
  "Notify with a sound after certain time"
  (interactive)
  (let ((time (read-string "Time (min|sec): " "10 min")))
    (message "I will remind you after %s" time)
    (run-at-time time nil #'play-reminder-sound)))

;;;###autoload
(defun to-snake-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))

(provide 'scripts.el)
