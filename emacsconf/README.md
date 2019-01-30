# Emacs Memo
## Cask
### Install cask

    $ curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
    $ export PATH="$HOME/.cask/bin:$PATH"

### Deployment & update

    $ cd ~/.emacs.d && cask

## Quick Edit
### Input special characters
C-x 8 RET, C-x 8 C-h

### Removing blank lines in a buffer
M-x flush-lines RET ^$ RET

### Join two lines
M-^ => M-x delete-indentation

### Mark word
M-@ => M-x mark-word, set mark then M-f (`forward-word`)

### Navigate back to the positions you visited in buffers
- C-u C-@ => M-x set-mark-command, jump back in current buffer
- C-x C-@ => M-x pop-global-mark, jump back in all buffers
- C-x p   => M-x pop-to-mark-command, jump to mark (does not affect global mark ring)

### Input 100 times 1
C-u 100 C-u 1

### Rectangle edit
C-x r k, Kill a marked rectangle region
C-x r y, Yank the last killed rectangle with *upper left corner* at point.
C-x r o, Blank out the region-rectangle, shifting text right.

### Copy/insert a region with register
C-x r s, Copy region into register
C-x r r, Copy rectangular region into register
C-x r i, Insert contents of register

### Save and restore window configuration with register
C-x r w, C-x r j

### Repeat last command
C-x z

### Run a shell command and insert result into current buffer
C-u M-!

### Send a region to a shell command
- M-| => M-x shell-command-on-region
- C-u M-| run and replace the region

### org-mode 7.0+ support easy-template
eg: input <s then press TAB, it will be expand automatically.

### Set the current horizontal position as a goal for C-n and C-p.
C-x C-n => M-x set-goal-column

### Move forward down one level of parentheses
C-M-d => M-x down-list

### Make the current definition and/or comment visible.
C-M-l => M-x reposition-window

### Copy something to system clipboard when use emacs in terminal
Select anything you want to copy, and then
- M-| xclip, in linux
- M-| pbcopy, in Mac OSX

### Get output of external command in the buffer
`shell-command` execute command and insert output to buffer
- C-u M-! df RET

`shell-command-on-region` execute command on region, and replace region with output
- C-u M-| xmllint --format - RET

### Quickly deal with symbols at point
#### isearch
Yank:
- C-w     => isearch-yank-word-or-char
- M-s .   => isearch-forward-symbol-at-point
- C-y     => isearch-yank-kill

Toggle:
- M-c     => isearch-toggle-case-fold
- M-r     => isearch-toggle-regexp
- M-s w   => isearch-toggle-word

Other:
- M-e     => isearch-edit-string
- M-s o   => isearch-occur
- M-s h r => isearch-highlight-regexp
#### occur
M-s o   => M-x occur
#### highlight
- M-s h . => M-x highlight-symbol-at-point
- M-s h u => M-x unhighlight-regexp

## Dired
### Dired open directory in the same buffer
M-x dired-find-alternate-file

### Recursively find and replace in text files not alread opened
M-x find-name-dired, to execute `find` command, then

Press `t` to "Toggle Mark" for all files found,
Press `Q` to "Query-Replace in Files ..."

### Edit dired buffer with wdired
C-x C-q => M-x dired-toggle-read-only

### Diff this file with its backup file or vice versa.
M-= => M-x dired-backup-diff, in dired-mode buffer

### Replace string in a directory recursively
M-x find-dired, specify (1) directory and (2) find arguments, eg:
`-name ".h" -o -name ".cpp"`, and `RET`.

Then (1) press `m` to select files, or `t` to select them all, (2) press
`Q` to query regexp string, eg: `\<DEBUG\>`, (3) press `RET`.

## Setting
### Default keymap and key bindings
refer to `mode-specific-command-prefix` in `bindings.el`

### Treat underline _ as a part of words
(modify-syntax-entry ?_ "w")

## Others
### Profiling emacs performance
- elp.el (old instrumenting profiler)
- profiler.el, Emacs 24.3+

M-x profiler-start
M-x profiler-report
M-x profiler-stop
http://stackoverflow.com/questions/568150/tips-for-profiling-misbehaving-emacs-lisp

### Customize local variables for a project or a directory
M-x add-dir-local-variable

.dir-locals.el, for more information see (info "(emacs) Directory Variables")

### Expand a defmacro
M-x macroexpand, or M-x macroexpand-all

### Regexp in ido-mode
If `ido-enable-regexp` is enabled, M-x ido-find-file RET c\++ to escape regexp.
Or, press `C-t` in ido-completion-map to disable regexp

### Print keystrokes you entered
M-x view-lossage

### View command you entered
M-x command-history

### How long did you run this Emacs
M-x emacs-uptime

### Local time of sunrise and sunset for today
M-x sunrise-sunset

## Reference
- https://github.com/emacs-tw/awesome-emacs
