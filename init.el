;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/.emacs by bth@
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dependencies:

;; Emacs > 24.

;; important installed packages:
;; eglot
;; use-package
;; format-all
;; no-littering
;; markdown-mode
;; multiple-cursors

;; ...and more. see package-selected-packages within custom-set-variables.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problems to solve:

;; deploy use-package everywhere. It will be in v29 anyway.

;; forward-sexp and mark-sexp to work better with Python: next-(), next-,
;; tree-sitter?
;; smartparens?

;; backward-kill-paragraph should kill only section dividers before paragraphs,
;; in markdown-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom commands to highlight / remember:

;; M-l : mark-symbol-at-point
;; M-s o : multi-occur-in-all-buffers
;; M-- : new binding for query-replace
;; C-M-n/p : markdown-forward/backward-section
;; C-M-h : markdown-mark-section
;; C-c c compile-again.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existing commands to use more:

;; M-s . : isearch-forward-symbol-at-point
;; M-. : xref-find-definitions
;; M-s o : multi-occur-in-matching-buffers : searches in all open buffers
;; M-g n/p : next/previous-error in compile and occur modes
;; C-x C-SPC : pop-global-mark : jump to next mark in *global* mark ring
;; C-x z repeat
;; C-x M-: repeat-complex-command
;; M-^ Join line to previous.
;; M-: eval-expression -- use to print value of variables
;; M-h mark-paragraph
;; C-M-h mark-defun
;; C-M-a/e jump to beg/end of defun.
;; C-M-f/b jump to beg/end of balanced region.
;; C-M-l : Cycle move cursor to mid, top, bot
;; M-x whitespace-mode : show all whitespace chars.
;; M-@ mark-word. Hit again to expand to next word.
;; C-M-SPC mark-sexp: Place mark same place C-M-f would go. Repeatable.
;; M-= count-words-region : also line and char count

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Available bindings:

;; M-'. Tried mark-symbol-at-point, but changed to M-l.
;; M-u. Changed to M-U for case switching. (M-c and M-l already rebound.)
;; C-z. Removed binding to stop process. Would make an interesting prefix group.
;; M-{} : unbound from next-paragraph

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ELPA is installed by default on v24. Just need to point to repos.

(require 'package)

(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")

                         ;; melpa is neglected
                         ;; ("melpa" . "http://melpa.org/packages/")

                         ;; melpa-stable is sometimes behind. this is problematic.
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ))

(package-initialize)

;; for my own code, should I want to move it out:
(add-to-list 'load-path (concat user-emacs-directory "defuns/" ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-theme-directory (concat user-emacs-directory "themes/"))

;; nice baseline:
(require 'solarized)
(load-theme 'solarized-gruvbox-dark t)

;; my own:
(load-theme 'bth-washedout t)

(defun high-constrast-mode ()
  (interactive)
  (set-face-background 'default "#000")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic appearance

;; Line highlighting
(global-hl-line-mode 1)

;; NOTE: I want C-k and C-a/e to work on logical lines in programming modes,
;; but still wrap long lines.
;; DISABLED:
;; (global-visual-line-mode t)

;; this gets the wrapping, without the overriding of kill/move functions.
(setq-default word-wrap t)

 ;; Turn off top menu
(menu-bar-mode 0)

;; highlight matching parenthesis
(show-paren-mode 1)

;; Line and column numbers
(global-linum-mode 1)
(setq column-number-mode t)

(setq linum-format "%3d\u2502") ;; 3 columns of digits, sep by |

;; Enable narrow view:
;; C-x n n : narrow to region
;; C-x n w : expand to whole
(put 'narrow-to-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line formatting.

;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
(setq-default mode-line-format
          (list
           ;; value of current buffer name
           '(:eval (propertize "%b " 'face 'font-lock-string-face
                               'help-echo (buffer-file-name)))

           ;; Line and column
           ;; "(%l,%c) "

           ;; value of `mode-name'
           "%m "

           ;; too noisy, but I'd like some minor modes listed.
           ;; mode-line-modes

           ;; File modification status.
           ;; mode-line-modified
           ;; better highlighting for modified status
           '(:eval
            (cond (buffer-read-only
                   (propertize " RO " 'face 'mode-line-read-only-face))
                  ((buffer-modified-p)
                   (propertize " ** " 'face 'mode-line-modified-face))
                  (t " -- ")))
           ;; Narrowing notifier
           " %n"
           ;; percentage of file scrolled to
           ;; " %P"
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File system, saving, startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scratch buffer should be clean.
;; Also remove all the startup screens and other BS.
(setq initial-scratch-message nil
      inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

;; Change all annoying yes or no prompts to the more convenient y/n prompts.
(fset 'yes-or-no-p (symbol-function 'y-or-n-p))

;; no-littering
(setq no-littering-etc-directory
      (expand-file-name "config/" user-emacs-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" user-emacs-directory))
(require 'no-littering)

;; this makes .~ files. Have it enabled for now.
(setq make-backup-files t)

;; Make all backup files in the user's backup directory.
;; (setq backup-directory-alist (list (cons "." "~/.emacs.d/backups/")))
(setq backup-directory-alist (list (cons "." (no-littering-expand-var-file-name "backup/"))))

;; auto saves. #files#
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; (setq auto-save-default nil)
;; 300 chars is default. Trying a little longer since this SSD is slow.
;; (setq auto-save-interval 600)

;; NOTE: the #file -> symlink thing is something else again:
;; (setq create-lockfiles nil)

;; On every save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't expand files when scrolling past the end.
(setq next-line-add-newlines nil)

;; What to do with files that don't have a final newline.
(setq require-final-newline t)

;; When emacs is invoked with --debug-init, we want logging not to be truncated.
(when (or init-file-debug debug-on-error)
  (setq message-log-max t) )

;; This will stop the process unless unbound.
(global-set-key (kbd "C-z") nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers / ido / smex

;; ido mode is default by v24.
(require 'ido) ;; for better C-x b
(ido-mode t)

(global-set-key (kbd "C-x C-r") 'revert-buffer)

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "M-i") 'switch-to-previous-buffer)

;; smex : much cooler ido-like interface to M-x
(require 'smex)

(global-set-key (kbd "M-x") 'smex)

;; just what the current major mode offers:
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; use this occasionally:
;; (smex-show-unbound-commands)

;; the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In my bindings, the following concepts prevail:

;;  keys        concept     control          meta           ctrl+meta meta+shift
;;  ==========  ----------  ---------------  -------------  --------- ----------
;;  f/b         (horiz)     character        word           sexp      symbol
;;  d/Backspace (delete)    del chars        del words      del sexp  -
;;  n/p         (vert)      lines            paragraphs     list      scroll
;;  a/e         (beg/end)   (beg/end)line    sentence       functions -

;; These bindings have been carefully chosen with respect to the defaut emacs
;; bindings for moving around.

;; Orthogonal to this is the notion that the shift key makes the vertical
;; movement scroll rather that move the cursor.  The cursors stays in the same
;; place.

;; TODO: Decouple marking from M-S-* bindings. Habit of C-SPC is deep enough
;; that I don't need it. Actually just getting this for free, because Shift is
;; acting like C-SPC for the sake of non-emacs users. But it's in the way.

;; C-l cursor management
(setq recenter-positions '(0.1 0.5 0.9)) ;; percentages of screen.
(global-set-key (kbd "C-M-l") 'move-to-window-line-top-bottom)

;; Unbind M-{} : globally it should be something interesting.
(global-set-key (kbd "M-{") nil)
(global-set-key (kbd "M-}") nil)

;; Rebind to M-p/n
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; forward-symbol

;; My own attempt.
;; See the discussion below at mark-next-symbol

(defun forward-symbol-skip-comments (&optional arg)
  "Invokes forward-symbol, but skips any intervening commented regions."
  (interactive "p")
  ;; first move forward one. If not, this won't detect the presence of a
  ;; commented region until we're already in it (eg, nothing between current pos
  ;; and next non-comment symbol)
  (forward-symbol arg)
  (let (beg end)
    ;; unfortunately this means we won't detect end-of-line comments, but I
    ;; almost never use those anyway. Otherwise we'd have to look backwards
    ;; to see where we are.
    (setq beg (line-beginning-position))
    (setq end (line-end-position))
    ;; this comes from newcomment.el
    (while (comment-only-p beg end)
      (forward-symbol arg)
      (setq end (line-end-position))
      ;; if we're going backward
      (if (< arg 0)
          (setq beg (line-beginning-position))
          )
      )
    )
  )

(defun backward-symbol ()
  (interactive)
  (forward-symbol-skip-comments -1)
)
(global-set-key (kbd "M-F") 'forward-symbol-skip-comments)
(global-set-key (kbd "M-B") 'backward-symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; avy

;; jump to words using vim-like brevity.
(require 'avy)

;; override default goto-char, which I never use:
(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-u") 'avy-goto-word-1)

;; dims everything else:
(setq avy-background t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling

;; Function that scrolls but that keeps the cursor in the same place in the
;; window.
(defun scroll-up-still (&optional n)
  "Scroll some lines but move the cursor the same amount of lines the opposite
way"
  (let ((nn (or n 1)))
    (scroll-up nn)
    (forward-line nn)) )

;; Scroll one line down and keep the cursor at its same relative pos.
(defun scroll-up-short (&optional n)
  (interactive)
  (let ((n (or n 10)))
    (scroll-up-still n)))

(defun scroll-down-short (&optional n)
  (interactive)
  (let ((n (or n 10)))
    (scroll-up-still (- n))))

;; Note the naming is confusing. "up" here means move the text up: thus
;; intuitively move the frame "down".
(global-set-key (kbd "M-N") 'scroll-up-short)
(global-set-key (kbd "M-P") 'scroll-down-short)

;; Trying this new binding also: replacing the old scroll completely
;; Easier on the fingers
(global-set-key (kbd "C-v") 'scroll-up-short)
(global-set-key (kbd "M-v") 'scroll-down-short)

;; Affects C-v and M-v default scroll, makes it less dramatic
(setq next-screen-context-lines 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filling, auto text, spelling, case switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto adds matching parens and such. Kinda annoying outside of programming.
;; try smartparens? https://github.com/Fuco1/smartparens
;; https://ebzzry.com/en/emacs-pairs/#slurpandbarf
(electric-pair-mode 1)

(setq default-fill-column 80)
(setq-default fill-column 80)
(setq-default resize-column 80)

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; wraps region in punctuation or < >.
(wrap-region-mode t)

;; `wrap-region-table' contains the default punctuations
;; that wraps. You can add and remove new wrappers by using the
;; functions `wrap-region-add-wrapper' and
;; `wrap-region-remove-wrapper' respectively.
(wrap-region-add-wrapper "`" "`" "`" '(markdown-mode))

(setq sentence-end-double-space nil)   ; One SPC ends a sentence
(setq adaptive-fill-mode t)    ; fill on second line of paragraph

;; use aspell, assuming it's installed. It's easy.
;; http://aspell.net/
(setq ispell-program-name "aspell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case switching

;; Unbind these, force addition of Shift. So rarely used and easy to type.
;; These all now get rebound elsewhere.
;; (global-set-key (kbd "M-c") nil)
;; (global-set-key (kbd "M-l") nil)
;; (global-set-key (kbd "M-u") nil)

(global-set-key (kbd "M-C") 'capitalize-word)
(global-set-key (kbd "M-L") 'downcase-word)
(global-set-key (kbd "M-U") 'upcase-word)

;; Normally disabled by default.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil) ;; C-x C-l

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Killing, duplicating, zap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Kills the ending newline, but only when at beg of line. Otherwise only to \n
(setq kill-whole-line t)

;; Replace the selection with any keystroke.
(delete-selection-mode 1)

;; Trying this keybinding.
(global-set-key (kbd "M-o") 'backward-kill-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Kill ring

(require 'browse-kill-ring)
;; this sets M-y to invoke browse-kill-ring, but not when following C-y.
(browse-kill-ring-default-keybindings)

;; Copy region or line: M-w
;; Replaces kill-ring-save with a version that copies the current
;; line if the region is not active.
(defun kill-ring-save-region-or-line ()
  "Like kill-ring-save, but if the region is not active save until the end of
the current line. If the region is not active and there is a numerical
prefix-argument, copy that many lines from the current line. If the numerical
prefix argument is negative, copy that many lines before the current line."

  (interactive)
  (if mark-active
      (call-interactively 'kill-ring-save)
    (progn
      (if (not current-prefix-arg)
          (let ((bol (point))
                (eol (min (+ (line-end-position) 1) (point-max))))
            (progn
              (kill-ring-save bol eol)
              (princ "Copied current line.")))
        (let ((bol (point))
              (eol (save-excursion
                     (forward-line (prefix-numeric-value current-prefix-arg))
                     (min (line-beginning-position) (point-max)))))
          (progn
            (kill-ring-save bol eol)
            (princ "Copied many lines.")))
        ))))

(substitute-key-definition 'kill-ring-save 'kill-ring-save-region-or-line
                           (current-global-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy to clipboard in osx

;; https://stackoverflow.com/a/10003300/8813860
;; https://gist.github.com/dcalacci/34dee28d39130070ff67

;; without my hack, this puts duplicates into the kill ring.
;; 1. dupe when yanking from text provided by clipboard
;; 2. triplicate when yanking from from emacs

;; The docstring of interprogram-paste-function says:

;; Note that the function should return a string only if a program
;; other than Emacs has provided a string for pasting; if Emacs
;; provided the most recent string, the function should return nil.
;; If it is difficult to tell whether Emacs or some other program
;; provided the current string, it is probably good enough to return
;; nil if the string is equal (according to ‘string=’) to the last
;; text Emacs provided.

;; naive version:
;; (defun copy-from-osx ()
;;   (shell-command-to-string "pbpaste"))

(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~")
              (pbstring (shell-command-to-string pbpaste)))
          ;; borrowed from kill-new.
          ;; this is the hack:
          (unless (string= pbstring (car kill-ring))
            pbstring))
      )))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; this reduces it by one, but still duplicate when from emacs itself without
;; the logic above.
(setq kill-do-not-save-duplicates t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zap

;; This is a portion of the original zap-to-char that I reuse in its
;; redefinition, and in a few functions.
(defun compute-zap-position (arg char &optional alt)
  (save-excursion
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char)))
    (let* ((case-fold-search nil) ;; make case sensitive.
              (begin (point))
                 (origchar (string-to-char
                                  (if (> arg 0)
                                        (buffer-substring (point) (1+ (point)))
                                    (buffer-substring (1- (point)) (point)))))
                    (end (progn
                             (when (and alt (= origchar char))
                                   (goto-char (if (> arg 0) (1+ (point)) (1- (point)))))
                               (search-forward (char-to-string char) nil nil arg)
                                 (when (or alt (not (= origchar char)))
                                       (goto-char (if (> arg 0) (1- (point)) (1+ (point)))))
                                   (point)))
                       )
      (list begin end)
      )))

;; It is better redefined this way rather than to implement this with an advice,
;; because the kill-region would have to be adjusted otherwise, which is
;; unclean.
(defun zap-up-to-char (arg char)
  "A version of zap-to-char that does not include the char we're searching for.
This version deletes the char if it is right next to the cursor when
the function is invoked."
  (interactive "p\ncZap up to char: ")
  (let ((begend (compute-zap-position arg char)))
    (apply 'kill-region begend)
    ))

(defalias 'zap-to-char 'zap-up-to-char)

(defun zap-up-to-char-reverse (char)
  "Like zap-up-to-char but reversed."
  (interactive "cZap backwards up to char: ")
  (let ((begend (compute-zap-position -1 char)))
    (apply 'kill-region begend)
    ))

(defun move-to-char (arg char)
  "Like zap-to-char but don't remove the contents."
  (interactive "p\ncMove forward to char: ")
  (let ((begend (compute-zap-position arg char t)))
    (goto-char (cadr begend))))

(defun move-to-char-reverse (arg char)
  (interactive "p\ncMove backward to char: ")
  (move-to-char (- arg) char)
  ;(backward-char (if (< arg 0) -1 +1))
  )

(defun copy-to-char (arg char)
  "Like zap-to-char but copy the contents without moving the cursor."
  (interactive "p\ncCopy up to char: ")
  (let ((begend (compute-zap-position arg char t)))
    (apply 'kill-ring-save begend)
    ))

;; NOTE: M-z is by default bound to zap-to-char, overridden by zap-up-to-char.
(global-set-key (kbd "M-r") 'zap-up-to-char-reverse)
(global-set-key (kbd "M-Z") 'move-to-char)
(global-set-key (kbd "M-R") 'move-to-char-reverse)
(global-set-key (kbd "M-c") 'copy-to-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Duplicating

;; From http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;; Modified to include comment out feature for regions as well.
(defun duplicate-line-or-region ()
  "Duplicate current line, or region if active. With prefix C-u,
comment out original."
  (interactive)
  (let ((use-region (use-region-p)))
    (save-excursion
      ;; Get region if active, otherwise line
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      ;; Go to beginning of next line, or make a new one
                      (if (< 0 (forward-line 1))
                          (newline))))))

        ;; Comment out original with prefix
        (if (consp current-prefix-arg)
            (comment-region (region-beginning) (region-end)))
        ;; insert the dupe
        (insert text)
        ))

    ;; Only if we're working with a line (not a region)
    ;; FIXME: this behaves oddly, as though (point) were actually calling (mark)
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        ;; Comment out original with prefix
        (if (consp current-prefix-arg)
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))
    ))

(global-set-key (kbd "C-M-d") 'duplicate-line-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commenting

;; Enhanced commenting for M-; Customized from:
;; http://stackoverflow.com/questions/9688748
;; (Added support for empty lines).
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        ;; When the line is empty.
        (if (= beg end) (comment-dwim nil)
          (comment-or-uncomment-region beg end))))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search, replace, mark, highlight, multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A version of query-replace, which uses its internals to do what I want:
(defun query-replace-symbol-at-point (from-string to-string)
  "Gets the symbol under point, puts it into query-replace and
loads the same string into the minibuffer as the replacement,
ready to be modified. Essentially does [M-% M-n C-m M-p]."
  (interactive (let ((from (thing-at-point 'symbol t)))
                 (let ((to
                        (read-from-minibuffer
                         (concat "Replace " from ": ") from nil nil nil from)
                       ))
                   ;; (interactive) expects either a string literal, in which
                   ;; case it will do the prompting, or a list of args all ready
                   ;; to be fed back into the defun arg list.
                 (list from to))
                 )
               )

  ;; query-replace won't work on the symbol under point unless we're at the
  ;; beginning of it.
  (let ((b (bounds-of-thing-at-point 'symbol)))
      (goto-char (car b))
      )

  ;; query-replace uses this internally:
  (perform-replace from-string to-string t nil t)
  ;; Have to manually add the query to the history. Note that we have to add the
  ;; from and to strings separately, because this history is used both for
  ;; complete from-to pairs, and for to-string defaults.
  (add-to-history query-replace-from-history-variable from-string nil t)
  (add-to-history query-replace-to-history-variable to-string nil t)
  (add-to-history 'query-replace-defaults (cons from-string to-string) nil t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-0 | M-- | M-= : all parallel: search at point, replace, replace at point

(global-set-key (kbd "M-0") 'isearch-forward-symbol-at-point)

;; M-% is awkward. But I'll leave it for now until I decide what to do with it.
;; NOTE: getting to isearch-query-replace from isearch mode still requires M-%.
;; Use M-n to get symbol at point. With this binding, the left thumb
;; doesn't leave Meta.
(global-set-key (kbd "M--") 'query-replace)

;; rebind from count-words-region
(global-set-key (kbd "M-=") 'query-replace-symbol-at-point)

;; Not often enough to require a keybinding:
(defalias 'qrr 'query-replace-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving and marking symbols

;; Current solutions:

;; M-l : mark-symbol-at-point : very reliable, as it uses thingatpt.el
;; M-s . : isearch-forward-symbol-at-point : powerful

;; M-F/B : forward-symbol-skip-comments : my own cooking. not sure yet.
;; M-9 : mark-sexp : flexible, but a little unpredictable
;; M-s g : rgrep : very powerful
;; C-c h : highlight-and-nav : cute, but shallow integration into core emacs.

;; I want these modular functions:

;; 1. Go to the next symbol, skipping whitespace and comments. : M-F/B

;; 2. Select the symbol at point, with ability to expand it, either by repeating
;; or a separate binding. : M-l

;; 3. Integration with isearch and query-replace. M-s . | M-=

;; Nice to haves:

;; * Would be nice if it played with highlight-symbol also. But isearch does
;; highlighting already.

;; * multiple-cursors integration is probably not the right way to go. Can be
;; slow or problematic. But should remember to use mc/mark-all-dwim

;; Deadends:

;; mark-word ? M-@ : hate this binding. But it does expand.
;; mc/mark-all-dwim : C-x C-d

;; this is too much override:
;; superword-mode : treats words as symbols. could add to hooks ...

;; this is a too hacky:
;; (modify-syntax-entry ?_ "w" python-mode-syntax-table)
;; See current syntax table with C-h s

;; stolen from mc--mark-symbol-at-point, so that i can modify.
(defun mark-symbol-at-point ()
  "Select the symbol under point."
  (interactive)
  ;; there's a symbol here and the region is inactive
  (when (and (thing-at-point 'symbol) (not (use-region-p)))
    (let ((b (bounds-of-thing-at-point 'symbol)))
      ;; point where it would be after a forward-symbol
      (goto-char (cdr b))
      (set-mark (car b)))))

(global-set-key (kbd "M-l") 'mark-symbol-at-point)

;; this is interesting, and maybe works well enough.
(global-set-key (kbd "M-9") 'mark-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grep and occur

;; NOTE: `grep', `occur', and `compile' all share functionality for visiting
;; matches in various files and buffers.

;; M-g n/p : next-error
;; M-{} works in compilation buffer only.

(global-set-key (kbd "M-s g") 'rgrep)

;; affects grep mode and others. We want this behavior.
(setq compilation-auto-jump-to-first-error t)

(add-hook 'grep-mode-hook
  (lambda ()
    ;; this will fail before grep-mode has been called.
    (add-to-list 'grep-find-ignored-directories "__pycache__")
    (add-to-list 'grep-find-ignored-directories ".git")
    ))

;; multi-occur works much like grep mode
;; default asks for the buffer regex, which is annoying. We just want to search
;; all open buffers.
(defun multi-occur-in-all-buffers (regexp)
  (interactive "Mgrep in all open buffers: ")
  (multi-occur-in-matching-buffers ".*" regexp)
)

;; rebind from M-x occur
(global-set-key (kbd "M-s o") 'multi-occur-in-all-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight symbol

;; Highlights all symbols under mark as defined by the language
(require 'highlight-symbol)

(defun highlight-and-nav ()
  "Turn on highlight-symbol-mode in the current buffer if not already,
and toggle highlight-symbol-nav-mode, so that M-n/p works."
  (interactive)
  ;; intended as a buffer-constant feature. If I really want it gone I can just
  ;; call this manually.
  (highlight-symbol-mode 1)
  (highlight-symbol-nav-mode 'toggle)
  (if (eval highlight-symbol-nav-mode)
      (message "highlight-symbol-nav-mode on : M-n/p bound to highlight-symbol-next/prev")
      (message "highlight-symbol-nav-mode off")
    )
  )

;; unbind these, because I use them too often.
(define-key highlight-symbol-nav-mode-map (kbd "M-n") nil)
(define-key highlight-symbol-nav-mode-map (kbd "M-p") nil)

;; try this:
(define-key highlight-symbol-nav-mode-map (kbd "M-{") 'highlight-symbol-prev)
(define-key highlight-symbol-nav-mode-map (kbd "M-}") 'highlight-symbol-next)

(global-set-key (kbd "C-c h") 'highlight-and-nav)
(setq highlight-symbol-idle-delay 1)

;; NOTE: I like this mode, except that it puts a symbol count in the
;; minibuffer, obscuring auto-documentation provided by eldoc.

;; I changed the code myself, added a highlight-symbol-quiet variable.
(setq highlight-symbol-quiet t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors.

(require 'multiple-cursors)

;; Simplest most used function. Highlight lines and spawn cursors.
(global-set-key (kbd "C-x m") 'mc/edit-lines)

;; this is better, since it skips empty lines that don't match the column position:
(global-set-key (kbd "C-x d") nil) ; unbind from Dired: I don't invoke it that way.
(global-set-key (kbd "C-x d") 'set-rectangular-region-anchor)

;; Should try this, as orthogonal to isearch-forward-symbol-at-point:
(global-set-key (kbd "C-x C-d") 'mc/mark-all-dwim)
(global-set-key (kbd "C-x !") 'mc/mark-all-like-this)

; Use arrow keys to quickly mark/skip next/previous occurances.
(global-set-key (kbd "C-x i") 'mc/mark-more-like-this-extended)

(global-set-key (kbd "C-x C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x C-p") 'mc/mark-previous-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General defalias and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun e ()
  (interactive)
  (find-file "~/.emacs.d/init.el")
)

(defun books ()
  (interactive)
  (find-file "~/writing/projects/books.md")
)

;; For use with global screen inversion
(defun invert-color ()
   (interactive)
   (load-theme 'adwaita)
   (set-face-background hl-line-face "#ddd")
   (set-face-foreground 'highlight nil)
   (set-face-foreground 'markdown-blockquote-face "#510")
   (set-face-foreground 'markdown-code-face "#510")
)

;; parallel to posix tool:
(defalias 'wc 'count-words)

(defalias 'dc 'describe-char)
(defalias 'ic 'insert-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode

(require 'markdown-mode)

(add-hook 'markdown-mode-hook (lambda ()
  ;; autofill adds newlines. I rely on wrapping rather than newlines.
  (auto-fill-mode 0)

  ;; if global-visual-line-mode is off
  ;; NOTE: text-mode is parent mode and already calls this.
  (visual-line-mode 1)

  ;; Change the behavior of C-n to respect visual buffer display,
  ;; not newlines.
  (setq line-move-visual t)

  ;; line number is irrelevant
  (linum-mode 0)

  ;; auto-save is annoyingly slow with cheap USB SSDs
  ;; (auto-save-mode 0)
 ))

;; for navigating between my <hr/> sections.
(defun markdown-find-section (&optional direction nocenter)
  (setq-local markdown-section "---
")
  ;; NOTE: setting noerror to non-nil and not t, means go to end of buffer
  (let ((pos (search-forward markdown-section nil 1 direction)))
  (when pos
    (goto-char pos)
    ;; go to the beg of the following paragraph.
    ;; doesn't work for backward yet, because if I go forward again it won't
    ;; move, since we'd then be below the same section.
    (if (= 1 direction)
      (forward-char 1)
      )
    (unless nocenter
      (recenter-top-bottom 12))
    )
  )
)

(defun markdown-forward-section ()
  "Move to next section marker."
  (interactive)
  (markdown-find-section 1)
  )

(defun markdown-backward-section ()
  "Move to previous section marker."
  (interactive)
  (markdown-find-section -1)
  )

(defun markdown-mark-section ()
  "Mark text between section markers."
  (interactive)
  ;; find the section marker before this point.
  (markdown-find-section -1 t)
  (previous-line 1)
  (set-mark (point))

  ;; go back to beg of where we were.
  (markdown-find-section 1 t)
  ;; this will take us to the end of buffer, if no next one.
  (markdown-find-section 1 t)
  (unless (eq (point) (buffer-end 1))
    (previous-line 3))
  )

;; Trying to keep these all parallel:

(define-key markdown-mode-map (kbd "M-p") 'backward-paragraph)
(define-key markdown-mode-map (kbd "M-n") 'forward-paragraph)

(define-key markdown-mode-map (kbd "C-M-n") 'markdown-forward-section)
(define-key markdown-mode-map (kbd "C-M-p") 'markdown-backward-section)

(define-key markdown-mode-map (kbd "C-M-h") 'markdown-mark-section)

(define-key markdown-mode-map (kbd "C-M-f") 'markdown-next-visible-heading)
(define-key markdown-mode-map (kbd "C-M-b") 'markdown-previous-visible-heading)

(define-key markdown-mode-map (kbd "C-c i") 'markdown-insert-italic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown macros

(defun journal ()
  "Generate a date stamp in ISO order, with a Markdown level 1
heading, plus the likely locale."
   (interactive)
   ;; date stamp in ISO ordering, but using . rather than - cuz I like it.
   (insert (format-time-string "%Y.%m.%d"))
   (insert " : Fool's Bluff\n")
   (insert "===\n\n")
)

(defun section ()
   (interactive)
   (delete-blank-lines)
   (insert "\n---\n\n"))

(defun insert-special-char (char)
  "Access a custom map of non-ASCII characters frequently needed,
for German, French, etc. Just enter the base char and get back
the special version."
  (interactive "cInsert special char: ")
  (if (eq char (string-to-char "$"))
    (insert "§"))
  (if (eq char (string-to-char "o"))
    (insert "ö"))
  (if (eq char (string-to-char "u"))
    (insert "ü"))
  (if (eq char (string-to-char "e"))
    (insert "é"))
  (if (eq char (string-to-char "i"))
    (insert "ï"))
  (if (eq char (string-to-char "a"))
    (insert "ä"))
  (if (eq char (string-to-char "S"))
    (insert "Ş"))
)

(defun delete-refs ()
  (interactive)
  (query-replace-regexp "\[+[[:digit:]]+.*?\]" "")
)

(define-key markdown-mode-map (kbd "C-c s") 'section)
(define-key markdown-mode-map (kbd "C-c c") 'insert-char)
(define-key markdown-mode-map (kbd "C-c v") 'insert-special-char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode

;; NOTE: text-mode is parent for markdown-mode

(add-hook 'text-mode-hook (lambda ()
  (visual-line-mode 1)

  ;; turned off because I tend to use wrapping instead.
  ;; (auto-fill-mode t)
))

(add-to-list 'auto-mode-alist '("\\.desc" . text-mode))
(add-to-list 'auto-mode-alist '("\\(CHANGES\\|TODO\\|README\\)" . text-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prog-modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation

;; FIXME: tree-sitter should eventually make true AST navigation and editing
;; possible.

;; easier than C-M-a/e.

;; python has its own python-nav-forward/backward-block bound to M-a/e, which
;; works well.
;; NOTE: rust-mode has rust-beginning/end-of-defun, but it's not as reliable.
(dolist (mode-iter '(c-initialization-hook rust-mode-hook go-mode-hook))
  (add-hook mode-iter (lambda ()
    (local-set-key (kbd "M-a") 'beginning-of-defun)
    (local-set-key (kbd "M-e") 'end-of-defun)

    ;; this doesn't work, due to the way global-visual-line-mode overrides.
    ;; (local-set-key (kbd "C-k") 'kill-line)
))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company

(use-package company
  :ensure

  :bind (:map prog-mode-map
              ("M-TAB" . company-complete)
              )

  :config
  ;; disable auto popups
  (setq company-idle-delay nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eglot

;; What I want from an IDE-like experience with "language server protocol":

;; 1. xref
;; 2. goto-next-error in compile : actually works out of the box with most langs.
;; 3. minimal eldoc : just minibuffer hints.
;; 4. company-complete : not strictly necessary, but good for lib exploration.

;; Note: this will be included in Emacs 29
;; https://joaotavora.github.io/eglot/
(require 'eglot)

;; I dislike syntax checkers: too noisy.
(add-to-list 'eglot-stay-out-of 'flymake)

;; eglot is nice and minimal. enables interactively with just M-x eglot. But, I
;; also want to turn on company-mode and configure it.
(defun eglot-on ()
  (interactive)
  (eglot-ensure)

  (company-mode 1)
)

(defun eglot-off ()
  (interactive)
  (eglot-shutdown (eglot-current-server))
  (company-mode -1)
)

;; this breaks in lisp mode
;; (add-hook 'prog-mode-hook #'eglot-ensure)

(add-hook 'python-mode-hook 'eglot-on)
(add-hook 'c-initialization-hook 'eglot-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; formatting

(use-package format-all
  :ensure

  :bind (:map prog-mode-map
              ("C-c f" . format-all-buffer)
              )

  :config
  ;; (setq format-all-formatters '(("Python" . black)))
  (add-hook 'prog-mode-hook 'format-all-ensure-formatter)
  )

;; used in python especially
(use-package highlight-indentation
  :ensure
  :config
  (add-hook 'prog-mode-hook #'highlight-indentation-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python

(add-hook 'python-mode-hook (lambda ()

 ;; see the not about .style.yapf config below.
 (setq python-indent 2)

 ;; Variables: doesn't seem to recognize them well in Python...
 ;; FIXME: tree-sitter should change this.
 (set-face-attribute 'font-lock-variable-name-face nil
                     :foreground nil
                     )

 ;; trying this:
 ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=13642
 (setq forward-sexp-function nil)
 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python macros

(defun python-insert-print (var)
  ;; "M" means: "Any string.  Inherits the current input method."
  (interactive "Mprint: ")
  (insert "print(\'" var ":\', " var ")")
  (backward-char 1))

(defun python-insert-self-documenting-format-print (var)
  (interactive "Mprint: ")
  (insert "print(f\'{" var " = }\')")
  (backward-char 6))

(defun python-insert-main ()
  (interactive)
  (insert "if __name__ == '__main__':
  pass"))

(defun python-insert-class ()
  (interactive)
  (insert "class Foo(object):
  def __init__(self):
    pass
")
  (previous-line 3)
  (forward-word 2)
)

(defun python-insert-test ()
  (interactive)
  (insert "class TestModule(unittest.TestCase):
  def setUp(self):
    pass

  def testEmpty(self):
    self.assertEqual(None, None)

if __name__ == '__main__':
  unittest.main()
")
  (previous-line 7)
  (back-to-indentation)
)

(defun python-insert-numpy ()
  (interactive)
  (insert "from functools import reduce
import numpy as np
import matplotlib.pyplot as plt
"))


(defun python-insert-file-open (arg)
  (interactive "Mwhole, lines, csv, csvd (headers), write: ")
(cond
    ((equal arg "whole") (python-insert-file-open-whole))
    ((equal arg "lines") (python-insert-file-open-lines))
    ((equal arg "csv") (python-insert-csv-lists))
    ((equal arg "csvd") (python-insert-csv-dicts))
    ((equal arg "write") (python-insert-file-write))
    (t (message "not recognized")))
)

(defun python-insert-file-open-whole ()
  (insert "
def OpenFileWhole(filename: str):
  s = ''
  with open(filename, 'r') as f:
    s = f.read()
  return s
"))

(defun python-insert-file-open-lines ()
  (insert "
def OpenFileLines(filename: str):
  lines = []
  with open(filename, 'r') as f:
    for line in f:
      lines.append(line.rstrip())
  return lines
"))

(defun python-insert-csv-lists ()
  (insert "
def OpenCsv(filename: str, delimiter: str=','):
  rowlist = []
  with open(filename, 'r') as f:
    r = csv.reader(
      f,
      delimiter=delimiter,
    )
    for row in r:
      rowlist.append(row)
  return rowlist
"))

(defun python-insert-csv-dicts ()
  (insert "
def OpenCsvDict(filename: str, delimiter: str=','):
  dlist = []
  with open(filename, 'r') as f:
    reader = csv.DictReader(f, delimiter=delimiter)
    dlist = [d for d in reader]
  return dlist
"))

(defun python-insert-file-write ()
  (insert "
def WriteFile(filename: str, output: str):
  with open(filename, 'w') as f:
    f.write(output)
  return
"))

;; Nota bene: default python-mode has many skeleton functions, accessed via:
;; C-c C-t $0, where the last char specifies the structure. C-c C-t c spits out
;; a class. But I don't much like the interactive interface and find it
;; more of a pain than convenience.

(add-hook 'python-mode-hook
  (lambda ()
    ;; this is used too often to obey the C-c C-t pattern:
    ;; (define-key python-mode-map (kbd "C-c p") 'python-insert-print)
    (define-key python-mode-map (kbd "C-c p") 'python-insert-self-documenting-format-print)
    (define-key python-mode-map (kbd "C-c C-t m") 'python-insert-main)
    (define-key python-mode-map (kbd "C-c C-t c") 'python-insert-class)
    (define-key python-mode-map (kbd "C-c C-t t") 'python-insert-test)
    (define-key python-mode-map (kbd "C-c C-t o") 'python-insert-file-open)
    (define-key python-mode-map (kbd "C-c C-t n") 'python-insert-numpy)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C macros

(defun c++-insert-printf (var)
  ;; "M" means: "Any string.  Inherits the current input method."
  (interactive "M")
  (insert "printf(\"" var ": %s\\n\", " var ");")
  (backward-char 2))
(add-hook 'c-initialization-hook
  (lambda () (define-key c-mode-base-map "\C-cp" 'c++-insert-printf)))

;; Google-internal
(defun c++-insert-log (var)
  (interactive "M")
  (insert (concat "LOG(WARNING) << \"" var ": \" << " var ";"))
  (backward-char 1))
(add-hook 'c-initialization-hook
  (lambda () (define-key c-mode-base-map "\C-cl" 'c++-insert-log)))

(defun c++-insert-cout (var)
  (interactive "M")
  (insert "cout << \"" var ": \" << " var " << \"\\n\";")
  (backward-char 9))
(add-hook 'c-initialization-hook
  (lambda () (define-key c-mode-base-map "\C-co" 'c++-insert-cout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go

(add-hook 'go-mode-hook (lambda ()
  (setq tab-width 2)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rust

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rustic

;; https://robert.kra.hn/posts/rust-emacs-setup/#rustic

;; (use-package rustic
;;   ;; :disabled t
;;   :ensure

;;   ;; :bind (:map rustic-mode-map
;;   ;;             ;; ("C-c C-c l" . flycheck-list-errors)
;;   ;;             ;; ("M-j" . lsp-ui-imenu)
;;   ;;             ;; ("M-?" . lsp-find-references)
;;   ;;             )

;;   :config

;;   (setq rustic-lsp-client 'eglot)

;;   ;; learned this from python mode:
;;   (setq company-idle-delay nil)

;;   ;; comment to disable rustfmt on save
;;   (setq rustic-format-on-save nil)

;;   ;; (add-hook 'rustic-mode-hook (lambda ()
;;   ;;   ;; ugh shutup
;;   ;;   (flycheck-mode -1)
;;   ;;   ))

;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust macros

(defun rust-insert-println (var)
  (interactive "Mprintln!: ")
  (insert "println!(\"" var ": {:#?}\", " var ");")
  (backward-char 2))

(defun rust-insert-dbg (var)
  (interactive "Mdbg!: ")
  (insert (concat "dbg!(&" var ");"))
  (backward-char 2))

(defun rust-insert-allow ()
  (interactive)
  (insert "#![allow(dead_code, unused_variables)]
"))

(defun rust-insert-derive-debug ()
  (interactive)
  (insert "#[derive(Debug)]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust-mode

(add-hook 'rust-mode-hook (lambda ()
  ;; (local-set-key (kbd "C-c c") 'rust-compile)
  (local-set-key (kbd "C-c f") 'rust-format-buffer)
  (local-set-key (kbd "C-c p") 'rust-insert-println)
  (local-set-key (kbd "C-c C-t d") 'rust-insert-dbg)
  (local-set-key (kbd "C-c C-t a") 'rust-insert-allow)
  (local-set-key (kbd "C-c C-t d") 'rust-insert-derive-debug)
  (local-set-key (kbd "C-c r") 'rust-run)

  ;; (local-set-key (kbd "M-a") 'rust-beginning-of-defun)
  ;; (local-set-key (kbd "M-e") 'rust-end-of-defun)

  ;; prefer to enable as needed?
  (eglot-ensure)
  (company-mode 1)
  ;; this disables auto popups
  (setq company-idle-delay nil)
  (local-set-key (kbd "M-TAB") 'company-complete)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell

(defun shell-split ()
  (interactive)
  (split-window-horizontally)
  (next-multiframe-window)
  (shell)
  ;; magic for disabling the command echo zsh does.
  (setq comint-process-echoes t)
  ;; FIXME: this should ideally set the size absolutely
  (enlarge-window -23 t)
  (linum-mode 0)
  ;; this is a script defined in .profile which changes the PS1 to a minimal version.
  (insert "psalt")
  ;; as though the user hit enter:
  (comint-send-input)
)

(defalias 'ss 'shell-split)

;; https://stackoverflow.com/questions/7733668
;; this is almost the same as default comint-clear-buffer,
;; but setting the size to 1 allows for the .oh-my-zsh prompt.
(defun shell-mode-clear ()
  (interactive)
  (let ((comint-buffer-maximum-size 1))
    (comint-truncate-buffer)))

(add-hook 'shell-mode-hook (lambda ()
  (local-set-key (kbd "C-l") 'shell-mode-clear)
))

;; xterm-color
;; https://github.com/atomontage/xterm-color
(use-package xterm-color :ensure)

;; Must also set TERM=xterm-256color in the shell itself.
(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; this fucks up my zsh prompt:
            ;; Disable font-locking in this buffer to improve performance
            ;; (font-lock-mode -1)
            ;; Prevent font-locking from being re-enabled in this buffer
            ;; (make-local-variable 'font-lock-function)
            ;; (setq font-lock-function (lambda (_) nil))

            ;; this makes proper color again:
            (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation: C-c c
;; For C, Rust, Go, even Python.

;; NOTE: Use C-u C-c c to get compile-again to behave like compile

;; NOTE: parts are disabled for now, because interfering with the rgrep behavior
;; I want.

;; TODO: if compile-again command doesn't match current buffer, use
;; current-buffer buffer-name

;; See http://stackoverflow.com/questions/8309769
;; What I wanted:
;; 1. Window resized reasonably.
;; 2. Window closed automatically upon success.
;; 3. Bonus: taken to error in code. But I think that's default.
(require 'compile)

;; (setq compilation-last-buffer nil)

(defun compile-again (ARG)
  "Run the same compile as the last time.

If there is no last time, or there is a prefix argument, this
acts like M-x compile."
  (interactive "p")
  (if (and (eq ARG 1)
           compilation-last-buffer)
      (progn
        (set-buffer compilation-last-buffer)
        (revert-buffer t t))
    (progn
      (call-interactively 'compile)
      (setq cur (selected-window))
      (setq w (get-buffer-window "*compilation*"))
      (select-window w)
      (setq h (window-height w))
      (shrink-window (- h 10))
      (select-window cur))))

(defun compilation-exit-autoclose (STATUS code msg)
  "Close the compilation window if there was no error at all."
  ;; If M-x compile exists with a 0
  (when (and (eq STATUS 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer)
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*")))
    )
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code)
  )

(setq compilation-exit-message-function 'compilation-exit-autoclose)

(add-hook 'c-initialization-hook
  (lambda ()
    (define-key c-mode-base-map (kbd "C-c c") 'compile-again)
    (setq compile-command (concat "g++ " (buffer-file-name)))
    ))

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "C-c c") 'compile-again)
    ;; tried pyflakes, mypy, etc, but actually running it will find errors they
    ;; won't: because it's Python. Problem is that it will then execute every
    ;; time, such as when starting up a matplotlib graph.
    (setq compile-command (concat "python " (buffer-file-name)))
    ))

(add-hook 'rust-mode-hook
  (lambda ()
    (define-key rust-mode-map (kbd "C-c c") 'compile-again)
    (setq compile-command "cargo build ")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server and emacsclient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'server)

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ==========================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bbaf656e55d463eddf9534d6874ad8f833f53ed1016429fab35fd907f2cd5281" default))
 '(package-selected-packages
   '(xterm-color format-all no-littering eglot smex rust-mode solarized-theme avy wrap-region highlight-symbol use-package go-mode js2-mode browse-kill-ring multiple-cursors markdown-mode))
 '(warning-suppress-types '((server))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
