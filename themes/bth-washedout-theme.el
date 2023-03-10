;;; bth-washedout-theme.el --- bth-washedout
;;; Version: 1.0
;;; Commentary:
;;; A theme called bth-washedout
;;; Code:

;; like an old black t-shirt.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - warm black background
;; - very lightly yellow default text
;; - bright blue, burnt yellow, burnt orange, and faded red highlights
;; - light brown comments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yellow

;; slightly yellow, for text
(setq bth-yellow "#cb9")

;; keywords
(setq bth-faded-yellow "#e09930")

;; numbers
(setq bth-brighter-burnt-yellow "#e90")

;; markdown italic
(setq bth-burnt-yellow "#cc7700")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orange

(setq bth-dark-orange "#e67128")

(setq bth-orange "#ff7700")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blue

;; dark bluegreen, old background:
(setq bth-bluegreen "#002029")

;; for shading this background:
(setq bth-bluegreen-shade "#002631")

;; darker blue, highlight line:
(setq bth-dark-blue "#001020")

;; stands way out:
(setq bth-bright-blue "#429489")

(setq bth-pale-blue "#6ca9b9")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; green

;; nice pale green
(setq bth-green "#74af68")

(setq bth-pea-green "#993")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; purple

(setq bth-purple "#aaf")

(setq bth-pale-purple "#bab")

;; brighter accent of bth-bluegreen
(setq bth-dark-purple "#224")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; red

;; warnings, errors:
(setq bth-red "#d43")

(setq bth-faded-red "#b65")

(setq bth-dark-red "#2a1818")

(setq bth-brown "#724e3b")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mono

(setq bth-offwhite "#9ab")

(setq bth-black "#111111")
(setq bth-warm-black "#222")
(setq bth-warmer-black "#242424")

(setq bth-grey "#888")
(setq bth-light-grey "#555")

(setq bth-warm-grey "#7c6f64")
(setq bth-warmlight-grey "#5c4f44")
(setq bth-dark-grey "#282828")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors

;; TODO : reconcile these two lists ^

;; Should be a balance between themed, and not too much of one shade.
;; What is purely syntax should recede, what's semantic should stand out.
;; however, the structure should also be obvious, so keywords should be bold.

;; from solarized list:
;; TODO: set these correctly:
(setq bth-washedout-base03      "#26180a")
(setq bth-washedout-base02      "#201309")
(setq bth-washedout-base01      "#875845")
(setq bth-washedout-base00      "#99624d")
(setq bth-washedout-base0       "#896843")
(setq bth-washedout-base1       "#b69384")
(setq bth-washedout-base2       "#e6dbd7")
(setq bth-washedout-base3       "#ebe4e1")

;; these names are all switched
(setq bth-washedout-blue        "#1e726f")
(setq bth-washedout-yellow      "#be7f28")

(setq bth-washedout-orange      "#225786")
(setq bth-washedout-red         "#4b4ca0")
(setq bth-washedout-magenta     "#805ea5")
(setq bth-washedout-violet      "#bb3d3f")
(setq bth-washedout-cyan        "#b18923")
(setq bth-washedout-green       "#9a3932")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line faces

;; TODO: put these in a separate themes/custom_faces.el definition.

(make-face 'mode-line-modified-face)
(make-face 'mode-line-read-only-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator faces

;; created by adding a regex of keywords, and assigning custom faces:
;; https://stackoverflow.com/a/47614692/8813860
(make-face 'font-lock-operator-face)
(make-face 'font-lock-number-face)
(make-face 'font-lock-class-face)
(make-face 'font-lock-dunder-method-face)

(dolist (mode-iter '(python-mode c-mode c++-mode java-mode javascript-mode rust-mode go-mode))
  (font-lock-add-keywords mode-iter
   '(
     ("\\<[\\+-]?[0-9]+\\(.[0-9]+\\)?\\>" 0 'font-lock-number-face)
     ("\\([][{}()@&\|!~^<>:=,.\\+*/%-]\\)" 0 'font-lock-operator-face)

     ;; to get underline just for "class Foo", without adding to types.
     ;; 1 supposed to be like \1 : first match group
     ;; https://stackoverflow.com/a/53575870
     ("^class \\_<\\([[:alnum:]]+\\)\\_>" 1 'font-lock-class-face)
     ))
  )

(font-lock-add-keywords 'python-mode
    '(
      ;; to disable underline set by font-lock-function-name-face
     ("\\_<\\(__[[:alnum:]]+__\\)\\_>" 1 'font-lock-dunder-method-face)
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deftheme

(deftheme bth-washedout "Like an old black t-shirt.")

(custom-theme-set-faces 'bth-washedout

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; basics
                        `(default ((t (
                                :foreground ,bth-yellow
                                :background ,bth-dark-grey
                                ))))
                        ;; no effect in tty:
                        `(cursor ((t (
                                :background ,bth-brighter-burnt-yellow
                                ))))
                        `(fringe ((t (
                                :background ,bth-dark-grey
                                ))))
                        `(linum ((t (
                                 ;; fades even more
                                :foreground ,bth-warmlight-grey
                                :background ,bth-dark-grey
                                ))))

                        ;; mode line
                        `(mode-line ((t (
                                :foreground ,bth-light-grey
                                :background ,bth-dark-grey
                                :underline nil
                                :overline t
                                ;; :box `(:line-width 1 :color ,bth-warm-grey)
                                ))))
                        `(mode-line-inactive ((t (
                                :foreground ,bth-light-grey
                                :background ,bth-dark-grey
                                :underline nil
                                ))))
                        `(mode-line-modified-face ((t (
                                :inherit mode-line-face
                                :foreground ,bth-red
                                :background ,bth-warm-black
                                ;; :box `(:line-width 2 :color ,bth-red)
                                ))))
                        `(mode-line-read-only-face ((t (
                                :inherit mode-line-face
                                :foreground ,bth-pale-purple
                                ;; :box '(:line-width 2 :color ,bth-pale-purple)
                                ))))
                        `(minibuffer-prompt ((t (
                                :foreground ,bth-bright-blue
                                :bold nil
                                ))))
                        `(region ((t (
                                :foreground ,bth-black
                                :background ,bth-light-grey
                                ))))
                        `(secondary-selection ((t (
                                :background ,bth-grey
                                ))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; font-lock faces
                        `(font-lock-builtin-face ((t (
                                :foreground ,bth-washedout-blue
                                ;; :foreground ,bth-faded-red
                                :bold nil
                                ))))
                        `(font-lock-function-name-face ((t (
                                :foreground ,bth-bright-blue
                                :bold nil
                                :underline t
                                ))))
                        `(font-lock-type-face ((t (
                                :foreground ,bth-orange
                                :bold nil
                                ))))
                        `(font-lock-constant-face ((t (
                                :foreground ,bth-faded-red
                                :bold t
                                ))))
                        `(font-lock-keyword-face ((t (
                                :foreground ,bth-faded-yellow
                                :bold nil
                                ))))
                        `(font-lock-comment-face ((t (
                                :foreground ,bth-warm-grey
                                ))))
                        `(font-lock-string-face ((t (
                                :foreground ,bth-burnt-yellow
                                ))))
                        `(font-lock-doc-face ((t (
                                :foreground ,bth-green
                                ))))
                        `(font-lock-variable-name-face ((t (
                                :foreground nil
                                ))))
                        `(font-lock-warning-face ((t (
                                :foreground ,bth-red
                                :bold t
                                ))))

                        ;; custom faces
                        `(font-lock-operator-face ((t (
                                :foreground ,bth-brown
                                ))))
                        `(font-lock-number-face ((t (
                                :foreground ,bth-faded-red
                                ;; :foreground ,bth-brighter-burnt-yellow
                                ))))
                        `(font-lock-class-face ((t (
                                :underline t
                                :inherit ,font-lock-type-face
                                ))))
                        `(font-lock-dunder-method-face ((t (
                                :underline nil
                                :inherit ,font-lock-function-name-face
                                ))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; mode-specific faces
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                        ;; same as highlight-indentation-face, so they mesh
                        `(hl-line ((t (
                                :foreground nil
                                :background ,bth-warmer-black
                                ;; don't inherit from default highlight face
                                :inherit nil
                                ))))
                        `(highlight-indentation-face ((t (
                                :inherit nil
                                :background ,bth-warmer-black
                                ))))
                        `(isearch ((t (
                                :foreground ,bth-black
                                :background ,bth-orange
                                ))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; markdown
                        `(markdown-italic-face ((t (
                                :foreground ,bth-burnt-yellow
                                :bold t
                                :underline nil
                                ))))
                        `(markdown-header-face ((t (
                                :foreground ,bth-orange
                                :bold nil
                                :underline t
                                ))))
                        `(markdown-blockquote-face ((t (
                                :foreground ,bth-pea-green
                                :bold nil
                                ))))
                        `(markdown-code-face ((t (
                                :foreground ,bth-pea-green
                                :bold nil
                                ))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; avy
                        `(avy-lead-face ((t (
                                :background ,bth-black
                                :foreground ,bth-burnt-yellow
                                ))))
                        `(avy-lead-face-0 ((t (
                                :background ,bth-black
                                :foreground ,bth-burnt-yellow
                                ))))
                        `(avy-background-face ((t (
                                :inherit font-lock-comment-face
                                ))))

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;; python tools
                        `(elpy-codecell-boundary ((t (
                                :inherit nil
                                ))))
                        `(flymake-error ((t (
                                :foreground ,bth-red
                                :bold nil
                                ))))

                        ;; company
                        `(company-tooltip ((t (
                                :foreground ,bth-yellow
                                :background ,bth-dark-blue
                                ))))
                        `(company-tooltip-selection ((t (
                                :background ,bth-dark-purple
                                ))))
                        `(company-tooltip-annotation ((t (
                                :foreground ,bth-green
                                ))))

                        )

;; don't need this if you set the custom-theme-load-path in .emacs:
;; ;;;###autoload
;; (and load-file-name
;;     (boundp 'custom-theme-load-path)
;;     (add-to-list 'custom-theme-load-path
;;                  (file-name-as-directory
;;                   (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'bth-washedout)

;;; bth-washedout-theme.el ends here
