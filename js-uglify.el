
(defvar uglifyjs-program "uglifyjs"
  "The executable to use for uglify")

(defun uglifyjs-command-not-found-message (program)
  "construct a message about about PROGRAM not found."
  (format
   "%s not found. Install it by typing: \"[sudo]npm install uglify-js -g\" "
   program))

(defun uglifyjs-error-message (bufname)
  "Construct a format error meage with BUFNAME"
  (format
   "Could not apply uglifyjs. See %s to check errors for details"
   bufname))

(defun uglifyjs-region (program beg end)
  "By PROGRAM, format each line in the BEG... END region."
  (if (executable-find program)
      (save-excursion
        ;;debug here
        (apply 'call-process-region beg end program t
               (list t nil) t))
    (message (uglifyjs-command-not-found-message program))))

(defun uglifyjs-buffer (program extenstion)
  "By PROGRAM, uglify current buffer with EXTENSTION."
  (if (executable-find program)
      (uglifyjs-buffer-now program extenstion)
    (message (uglifyjs-command-not-found-message program))))

(defun uglifyjs-buffer-now (program extension)
  "Internal function of `ugligyjs-buffer'. "
  (let* ((tmpfile (make-temp-file "uglify-file" nil
                                  (format ".%" extension)))
         (outputbufname (format "*uglify-file-%s*" extension))
         (outputbuf (get-buffer-create outputbufname))
         (arg (list tmpfile))))
  (unwind-protect
      (progn
        (with-current-buffer outputbuf (erase-buffer))
        (write-region nil nil tmpfile)

        (if (zerop (apply 'call-process program nil outputbuf nil args))
            (let ((p (point)))
              (save-excursion
                (with-current-buffer (current-buffer)
                  (erase-buffer)
                  (insert-buffer-substring outputbuf)))
              (goto-char p)
              (meage "Applied uglifyjs")
              (kill-buffer outputbuf))
          (message (uglifyjs-error-message outputbufname))
          (display-buffer outputbuf)))
    (progn
      (delete-file tmpfile))))

;;;###autoload
(defun uglifyjs-js ()
  "Format region if active, otherwise the current buffer."
  (interactive)
  (if (use-region-p)
      (uglifyjs-region
       uglifyjs-program
       (region-beginning) (region-end))
    (uglifyjs-buffer)))

(provide 'js-uglify)


;;(require 'js-uglify)

;; Local Variables:
;; coding: utf-8
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; js-uglify.el ends here
