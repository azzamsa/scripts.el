# Scripts

A collection of my tiny but useful Emacs Lisp code.

Most of the code here extracted from my .emacs.d [aza-script.el](https://github.com/azzamsa/emacs.d/blob/master/aza-packages/aza-scripts.el)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Scripts](#scripts)
    - [Buffers](#buffers)
        - [Kill ALL buffers](#kill-all-buffers)
    - [Date Time](#date-time)
        - [Insert today's date](#insert-todays-date)
    - [Org](#org)
        - [Insert filename as heading](#insert-filename-as-heading)
    - [Programming](#programming)
        - [Compile UI file to py](#compile-ui-file-to-py)
    - [Text Manipulation](#text-manipulation)
        - [Remove secrets from region](#remove-secrets-from-region)
        - [Smart delete line](#smart-delete-line)

<!-- markdown-toc end -->


## Buffers

### Kill ALL buffers

I use `crux-kill-other-buffers` for some time. But crux didn't kill
dired buffers. So I made my own. This will kill ALL buffer including
dired, but special buffers.

``` elisp
(defun aza-kill-other-buffers ()
  "Kill all buffers but current buffer and special buffers.
(Buffer that start with '*' and white space ignored)"
  (interactive)
  (when (y-or-n-p "Really kill all other buffers ? ")
    (let ((killed-bufs 0))
      (dolist (buffer (delq (current-buffer) (buffer-list)))
        (let ((name (buffer-name buffer)))
          (when (and name (not (string-equal name ""))
                     (/= (aref name 0) ?\s)
                     (string-match "^[^\*]" name))
            (cl-incf killed-bufs)
            (funcall 'kill-buffer buffer))))
      (message "Killed %d buffer(s)" killed-bufs))))

```

Before:

![image](https://user-images.githubusercontent.com/17734314/51159142-3ee9bc80-18ba-11e9-82d1-6255ccd59b58.png)

With `aza-kill-other-buffers`:

![image](https://user-images.githubusercontent.com/17734314/51159246-d222f200-18ba-11e9-9f61-91fe0984868c.png)

With `crux-kill-other-buffers`: (dired buffer didn't get killed)

![image](https://user-images.githubusercontent.com/17734314/51159188-77899600-18ba-11e9-9aba-a6567e6dce4b.png)

## Date Time

### Insert today's date

![aza-today](https://user-images.githubusercontent.com/17734314/52390960-a10a9b80-2acd-11e9-90c2-c15f4fcb06c8.gif)

I work with huge org files that involve many plain timestamp. It's
easy to get previous/next days using prefix arguments e.g +2 or
-4. But it's painful if you need to get previous/next timestamp based
on certain date.

This function will insert today's date if invoked without arguments
and no active region. Prefix arguments specifies how many days to
move, arguments can be negative, negative means previous day. If
region selected, make fake today's date according to the date under
region.

You need to load [ts.el](https://github.com/alphapapa/ts.el) for this
to work. I decide to use ts.el rather than `format-time-string`. It's
more readable, beautiful and less code. To duplicate line/region
faster, I use [crux-duplicate-current-line-or-region](https://github.com/bbatsov/crux)

``` elisp
(require 'ts) 

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
```

The 'beautiful' way to do region is using `(interactive "r")` but it
always complain 'The mark is not set now, so there is no region' if
you never invoke mark region before (e.g after Emacs start up).


## Org

### Insert filename as heading

![insert-heading](https://user-images.githubusercontent.com/17734314/52390975-acf65d80-2acd-11e9-9c48-4761ceef2e25.gif)

Most of the time my org first heading name is the same as filename. So I
made this function. My habit is to separate word in filename by
dash. If you want more robust function, take the second.

``` elisp
(defun insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (replace-regexp-in-string "-" " " (file-name-sans-extension (buffer-name))))))

```

![s-header-name](https://user-images.githubusercontent.com/17734314/52455913-04a3d000-2b85-11e9-88d0-b66ffa35c7f8.gif)

This is the more robust function. It can deal with almost separator in
filename. You have to load [s.el](https://github.com/magnars/s.el) to
make this function works. Many packages already use s.el, probably
it's installed in your Emacs.

``` elisp
(defun insert-filename-as-heading ()
  "Take current filename (word separated by dash) as heading."
  (interactive)
  (insert
   (capitalize
    (s-join " " (s-split-words (file-name-sans-extension (buffer-name)))))))
```

## Programming

### Open External Terminal and Tmux from Dired

![term-here](https://user-images.githubusercontent.com/17734314/56504785-dd359000-6543-11e9-9a2d-48e42861c701.gif)

If no external terminal opened, start one. Else attach to it and open
new pane in current path

But if you prefer to attach to current pane (don't open new pane) and
change directory path your self. Use:

`urxvtc -e bash -c "tmux -q has-session && exec tmux attach-session -d || exec tmux new-session -n$USER -s$USER@$HOSTNAME"`

PS: You need `urxvtd` (daemon) running at OS startup. Because we're using
`urxvtc` (client)

``` elisp
(defun term-here ()
  (interactive)
  (start-process "" nil "urxvtc"
                 "-e" "bash"
                 "-c" "tmux -q has-session && exec tmux new-window || exec tmux new-session -n$USER -s$USER@$HOSTNAME"))
```

### Compile UI file to py

Compile Qt user interfaces to Python code directly from Emacs. Put
your point in `foo.ui` then invoke this function.

``` elisp
(defun compile-ui-to-py ()
  "Compile Qt5 user interfaces to Python code directly from
Emacs."
  (interactive)
  (let ((inputfile (dired-get-filename))
        (outputfile
         (file-name-sans-extension
          (file-name-nondirectory (dired-get-filename)))))
    (start-process "" nil "pyuic5" inputfile
                   (concat "--output=" default-directory outputfile ".py"))))
```

## Text Manipulation

### Remove secrets from region

![rm-mysecrets](https://user-images.githubusercontent.com/17734314/52390983-bc75a680-2acd-11e9-94b5-00980acb8eca.gif)

Most of the time I have to attach the program output / log to bug
report. I don't my all my secrets words there.

Of course you need to put `(list-my-secrets)` in your non published files.

``` elisp
(defun list-my-secrets ()
  "The list of my secrets"
  '(("johndoe" . "user")
    ("johndoemachine" . "machine")
    ("johndoe@jdoe.com" . "myemail")))

(defun rm-mysecrets ()
  "Remove all confidential information."
  (interactive)
  (dolist (pair (list-my-secrets))
    (save-excursion
      (replace-string (car pair) (cdr pair)))))

```

### Smart delete line

![crux-smart-delete](https://user-images.githubusercontent.com/17734314/56504768-cc851a00-6543-11e9-835e-fc74c08185f8.gif)

Rather than having separate key to delete line, or having to invoke
prefix-argument. You can use [crux-smart-kill-line](https://github.com/bbatsov/crux/blob/308f17d914e2cd79cbc809de66d02b03ceb82859/crux.el#L199)
which will "kill to the end of the line and kill whole line on the next
call". But if you prefer `delete` instead of `kill`, you can use the
code below.

For point-to-string operation (kill/delete) I recommend to use [zop-to-char](https://github.com/thierryvolpiatto/zop-to-char)

``` elisp
(defun aza-delete-line ()
  "Delete from current position to end of line without pushing to `kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun aza-delete-whole-line ()
  "Delete whole line without pushing to kill-ring."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun crux-smart-delete-line ()
  "Kill to the end of the line and kill whole line on the next call."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (aza-delete-whole-line)
      (goto-char orig-point)
      (aza-delete-line))))
```
