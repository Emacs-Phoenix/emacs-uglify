;;; emacs-uglify.el --- uglify HTML, CSS and JavaScript/JSON by js-beautify

;; Copyright (C) 2015 Aby Chan  <abchan@outlook.com>

;; Author: Aby Chan <abychan@outlook.com>
;; Version: 0.1
;; URL: https://github.com/Emacs-Phoenix/emacs-uglify

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; For more information, See URL https://github.com/Emacs-Phoenix/emacs-uglify.

;;; Commentary:
;;nil now

;;; Code:

;;https://github.com/mishoo/UglifyJS2
(defvar uglifyjs-program "uglifyjs"
  "The executable to use for uglifyjs")

;;https://github.com/fmarcia/UglifyCSS
(defvar uglifycss-program "uglifycss"
  "The executable to use for uglifycss")

(defun uglifyjs-command-not-found-message (program)
  "construct a message about about PROGRAM not found."
  (format
   "%s not found. Install it by typing: \"[sudo]npm install uglify-js -g\" "
   program))

(defun uglifycss-command-not-found-message (program)
  "construct a message about about PROGRAM not found."
  (format
   "%s not found. Install it by typing: \"[sudo]npm install uglifycss -g\" "
   program))

(defun uglify-error-message (bufname)
  "Construct a format error meage with BUFNAME"
  (format
   "Could not apply uglify. See %s to check errors for details"
   bufname))

(defun uglify-region (program beg end extenstion uglify-command-not-found-message)
  "By PROGRAM, format each line in the BEG... END region."
  (if (executable-find program)
      (save-excursion
        (let* ((tmpfile (make-temp-file "uglify-file" nil
                                        (format ".%s" extenstion)))
               (outputbufname (format "*uglify-file-%s" extenstion))
               (outputbuf (get-buffer-create outputbufname))
               (args (list tmpfile)))
          (unwind-protect
              (progn
                (with-current-buffer outputbuf (erase-buffer))
                (write-region beg end tmpfile)
                (apply 'call-process-region beg end program t (list t nil) t args))
            (progn
              (delete-file tmpfile)))))
    (message (funcall uglify-command-not-found-message program))))

(defun uglify-buffer (program extenstion uglify-command-not-found-message)
  "By PROGRAM, uglify current buffer with EXTENSTION."
  (if (executable-find program)
      (uglify-buffer-now program extenstion)
    (message (funcall uglify-command-not-found-message program))))


(defun uglify-buffer-now (program extenstion)
  "Internal function of `ugligyjs-buffer'. "
  (let* ((tmpfile (make-temp-file "uglify-file" nil
                                  (format ".%s" extenstion)))
         (outputbufname (format "*uglify-file-%s*" extenstion))
         (outputbuf (get-buffer-create outputbufname))
         (args  (list tmpfile)))
    ;;一个unwind-protect接收任意数量的参数，返回第一个的值。然而，如果第一个的求值被中断了，那么剩下的表达式将会被求值。
    (unwind-protect
        (progn
          (with-current-buffer outputbuf (erase-buffer))
          (write-region nil nil tmpfile)
          ;;call-process  &optional infile destination(输出buf) display &rest args
          (if (zerop (apply 'call-process program nil outputbuf nil args))
              (let ((p (point)))
                (save-excursion
                  (with-current-buffer (current-buffer)
                    (erase-buffer)
                    (insert-buffer-substring outputbuf)))
                (goto-char p)
                (message "Applied uglifyjs")
                (kill-buffer outputbuf))
            (message (uglify-error-message outputbufname))
            (display-buffer outputbuf)))
      (progn
        (delete-file tmpfile)))))

;;;###autoload
(defun uglifyjs ()
  "Format region if active, otherwise the current buffer."
  (interactive)
  (if (use-region-p)
      (uglify-region uglifyjs-program (region-beginning) (region-end) "js" 'uglifyjs-command-not-found-message)
    (uglify-buffer uglifyjs-program "js" 'uglifyjs-command-not-found-message)))

;;;###autoload
(defun uglifycss ()
  "Format region if active, otherwise the current buffer."
  (interactive)
  (if (use-region-p)
      (uglify-region uglifycss-program (region-beginning) (region-end) "css" 'uglifycss-command-not-found-message)
    (uglify-buffer uglifycss-program "css" 'uglifycss-command-not-found-message)))

(provide 'emacs-uglify)


;;(require 'emacs-uglify)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; js-uglify.el ends here
