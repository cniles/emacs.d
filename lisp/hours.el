(provide 'hours)

(defun insert-timesheet ()
  "Insert a timecard timesheet"
  (interactive)
  (insert-file "~/workspace/.timecard-table.org"))
