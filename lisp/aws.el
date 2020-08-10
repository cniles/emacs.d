;;; aws --- aws utils

;;; Commentary:

;;; Code:
(require 'request)

(defvar aws-profile nil
  "The profile used for AWS commands.  Overrides AWS_PROFILE if not nil.")

(defun prep-command-part (part)
  "Conditionally handle PART from command form to a string."
  (cond ((stringp part) part)
	((symbolp part) (eval part))
	((numberp part) (number-to-string part))
	((listp part)
	 (let ((r (eval part)))
	   (if (stringp r) r
	     (error "All parts of form must return string"))))))

(defun prep-command (command-form)
  "Map the values in COMMAND-FORM into a string to be executed."
  (string-join (mapcar 'prep-command-part command-form) " "))

(defun aws-make-command (command subcommand args)
  "Makes an AWS command from the provided command, subcommand and command arguments.")

(defun aws-lambda-invoke-buffer-as-payload (profile function-name buffer)
  "Invokes a lambda function with the current buffer as the payload" 
  (interactive
   (let ((profile (read-string "Profile: " aws-profile nil aws-profile nil))
	 (fn-name (read-string "Function name: " aws-lambda-function-name nil aws-lambda-function-name nil)))
     (list profile fn-name (current-buffer))))
  (let* ((outfile (make-temp-file "out" nil ".json"))
	 (payload (replace-regexp-in-string "\r?\n" "" (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" (buffer-substring-no-properties (point-min) (point-max))) "\"")))
	 (cmd (prep-command
	       '("aws" "lambda" "invoke"
		 "--cli-binary-format" "raw-in-base64-out"
		 "--profile" profile
		 "--function-name" function-name
		 "--payload" payload
		 outfile)))
	 (out (shell-command-to-string cmd)))
    (with-output-to-temp-buffer "*aws-lambda-invoke*"
      (with-current-buffer "*aws-lambda-invoke*"
	(insert-file-contents outfile)
	(read-only-mode))
      (princ out t))))

(princ (replace-regexp-in-string "\"" "\\\\\"" "\"hello\""))

(provide 'aws)
;;; aws.el ends here

;;; Local Variables:
;;; aws-profile: "test"
;;; aws-lambda-function-name: "test-fn"
;;; End:
