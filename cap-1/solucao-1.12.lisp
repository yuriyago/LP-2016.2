
(defun pascal (line position)
  (cond ((or (< line 0)
	     (< position 1)
	     (> position (+ line 1)))
	 (error "line or position invalid."))
	((or (= position 1)
	     (= position (+ line 1)))
	 1)
	(t (+ (pascal (- line 1) position)
	      (pascal (- line 1) (- position 1))))))



