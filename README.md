# emacs-uglify

# Features
* CSS, and JavaScript/JSON uglify

# Requirements

* `uglify-js` installed by typing: `npm -g install uglify-js`
* `uglifycss` installed by typing: `npm -g install uglifycss`

## Manual

Just drop `emacs-uglify.el`. somewhere in your `load-path`.

```lisp
(add-to-list 'load-path "~/somewhere")
```


# Usage

## Basic setup

Add the following to your emacs init file.

(require 'emacs-uglify)

then you can M-x `uglifycss` or `uglifyjs`