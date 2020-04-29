;;; uuid --- Provides functions for inserting uuids
;;; Commentary:
;;; Code:

(defconst null-uuid "00000000-0000-0000-0000-000000000000")

(defun insert-null-uuid ()
  "Insert a null uuid."
  (interactive)
  (insert null-uuid))

(defun insert-random-uuid ()
  "Insert a (insufficiently) random uuid."
  (interactive)
  (insert (mapconcat (lambda (c) (if (eql c 48) (format "%x" (random 16)) "-")) null-uuid "")))

(provide 'uuid)
;;; uuid.el ends here
