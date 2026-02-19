;; Smart Traffic Signal System using Predicate Logic

;; Predicates
(defun high-density-p (density)
  (equal density 'high))

(defun no-accident-p (accident)
  (equal accident 'no))

;; Rule: Green if high density AND no accident, else Red
(defun traffic-signal (density accident)
  (if (and (high-density-p density) (no-accident-p accident))
      'GREEN
      'RED))

;; Main - Accept input and output decision
(defun main ()
  (format t "Enter traffic density (high/low): ")
  (let ((density (read)))
    (format t "Any accident? (yes/no): ")
    (let ((accident (read)))
      (format t "~%--- Decision ---~%")
      (format t "Density: ~a~%" density)
      (format t "Accident: ~a~%" accident)
      (format t "Signal: ~a~%" (traffic-signal density accident)))))

(main)