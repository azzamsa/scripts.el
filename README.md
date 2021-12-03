<div align="center">
<h1>scripts.el</h1>
<img src='https://raw.githubusercontent.com/azzamsa/emacs.d/master/assets/emacs-logo.svg' width=150px/>

A collection of my tiny but useful Emacs Lisp code

</div>

---

This is the repository companion for the ["Useful Emacs Lisp scripts collection"](https://azzamsa.com/n/scripts-el/)

## Usage:

``` elisp
(use-package scripts.el
  :after secrets.el
  :straight (aza-scripts :type git :host github :repo "azzamsa/scripts.el")
  :bind (("C-c k" . aza-kill-other-buffers)
         ("C-c t" . aza-today)
         ("C-c i" . insert-filename-as-heading)))
```
