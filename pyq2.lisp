;; Loan Approval System using Backward Chaining Inference

;; Knowledge Base - Facts (initially empty, filled by user input)
(defvar *facts* '())

;; Add a fact
(defun add-fact (fact)
  (pushnew fact *facts*))

;; Check if a fact is known
(defun fact-known-p (fact)
  (member fact *facts*))

;; Ask user for a fact if not already known
(defun ask-fact (fact prompt)
  (unless (fact-known-p fact)
    (format t "~a (yes/no): " prompt)
    (let ((answer (read)))
      (when (equal answer 'yes)
        (add-fact fact)))))

;; --- Backward Chaining ---
;; Goal: Loan Approved
;; Rule: Loan Approved IF Salary is High AND Credit History is Good

(defun backward-chain ()
  (format t "~%--- Backward Chaining Inference ---~%")
  (format t "Goal: Is Loan Approved?~%~%")

  ;; Step 1: To prove Loan Approved, we need Salary=High AND Credit=Good
  (format t "To approve loan, checking required conditions...~%~%")

  ;; Step 2: Ask required conditions
  (ask-fact 'high-salary "Is salary high?")
  (ask-fact 'good-credit "Is credit history good?")

  ;; Step 3: Apply rule and conclude
  (format t "~%--- Reasoning ---~%")
  (format t "Rule: Loan Approved IF (Salary=High AND Credit=Good)~%")
  (format t "Salary High: ~a~%" (if (fact-known-p 'high-salary) "YES" "NO"))
  (format t "Credit Good: ~a~%" (if (fact-known-p 'good-credit) "YES" "NO"))

  (format t "~%--- Decision ---~%")
  (if (and (fact-known-p 'high-salary) (fact-known-p 'good-credit))
      (format t "Result: LOAN APPROVED ✓~%")
      (format t "Result: LOAN REJECTED ✗~%")))

;; Run
(setf *facts* '())  ; reset facts
(backward-chain)
