;; ========================================
;; BASIC LISP SYNTAX - Quick Reference
;; ========================================

;; --- 1. PRINTING OUTPUT ---
(print "Hello, World!")
(format t "Hello ~a!~%" "Lisp")    ; ~a = placeholder, ~% = newline

;; --- 2. VARIABLES ---
;; Global variable
(defvar *name* "Alice")             ; *earmuffs* convention for globals
(defvar *age* 21)

;; Local variable
(let ((x 10)
      (y 20))
  (print (+ x y)))                  ; prints 30

;; Changing a variable
(setf *age* 22)

;; --- 3. DATA TYPES ---
42                ; Integer
3.14              ; Float
"hello"           ; String
T                 ; True
NIL               ; False / Empty
'apple            ; Symbol
'(1 2 3)          ; List

;; --- 4. ARITHMETIC ---
(+ 2 3)           ; 5
(- 10 4)          ; 6
(* 3 4)           ; 12
(/ 10 2)          ; 5
(mod 10 3)        ; 1

;; --- 5. COMPARISON ---
(= 5 5)           ; T
(/= 5 3)          ; T  (not equal)
(< 3 5)           ; T
(> 5 3)           ; T
(<= 3 3)          ; T
(>= 5 3)          ; T

;; --- 6. LOGICAL OPERATORS ---
(and T T)          ; T
(or T NIL)         ; T
(not NIL)          ; T

;; --- 7. IF-ELSE ---
(if (> 5 3)
    (print "5 is greater")
    (print "3 is greater"))

;; COND - multiple conditions (like if-elseif-else)
(let ((marks 75))
  (cond
    ((>= marks 90) (print "Grade A"))
    ((>= marks 75) (print "Grade B"))
    ((>= marks 60) (print "Grade C"))
    (t              (print "Fail"))))

;; --- 8. FUNCTIONS ---
(defun add (a b)
  (+ a b))

(print (add 3 4))                   ; 7

(defun greet (name)
  (format t "Hello, ~a!~%" name))

(greet "Bob")

;; --- 9. LISTS ---
(list 1 2 3)            ; creates (1 2 3)

(car '(1 2 3))          ; 1       (first element)
(cdr '(1 2 3))          ; (2 3)   (rest of list)
(cons 0 '(1 2 3))       ; (0 1 2 3)  (add to front)
(length '(1 2 3))       ; 3
(nth 1 '(10 20 30))     ; 20      (element at index 1)
(append '(1 2) '(3 4))  ; (1 2 3 4)

;; --- 10. LOOPS ---
;; DOTIMES - repeat n times
(dotimes (i 5)
  (format t "i = ~a~%" i))

;; DOLIST - loop through a list
(dolist (item '(apple banana cherry))
  (print item))

;; LOOP - simple loop with collect
(loop for i from 1 to 5
      collect (* i i))              ; (1 4 9 16 25)

;; --- 11. RECURSION ---
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 5))               ; 120

;; --- 12. STRING OPERATIONS ---
(string-length "hello")             ; 5  (clisp)
(length "hello")                    ; 5  (works in both)
(concatenate 'string "hi" " " "there")  ; "hi there"

;; --- 13. USER INPUT ---
(defun ask-name ()
  (format t "What is your name? ")
  (let ((name (read-line)))
    (format t "Nice to meet you, ~a!~%" name)))


;; ========================================
;; AI CONCEPTS IN LISP
;; ========================================

;; --- 14. PROPOSITIONS ---
;; A proposition is a statement that is either TRUE or FALSE

(defvar *raining* T)        ; "It is raining" = TRUE
(defvar *have-umbrella* NIL) ; "I have umbrella" = FALSE

;; Combining propositions with logic
(and *raining* *have-umbrella*)      ; NIL (raining but no umbrella)
(or *raining* *have-umbrella*)       ; T   (at least one is true)
(not *raining*)                      ; NIL (it IS raining)

;; If raining AND no umbrella => stay home
(if (and *raining* (not *have-umbrella*))
    (print "Stay home")
    (print "Go out"))


;; --- 15. PREDICATES ---
;; A predicate is a function that returns T or NIL (true/false)
;; By convention, predicate names end with -p

(defun adult-p (age)
  (>= age 18))

(adult-p 21)     ; T
(adult-p 10)     ; NIL

(defun eligible-p (age income)
  (and (adult-p age) (> income 50000)))

(eligible-p 25 60000)  ; T
(eligible-p 16 60000)  ; NIL (not adult)


;; --- 16. FORWARD CHAINING ---
;; Start from KNOWN FACTS => apply rules => reach conclusion
;; "Data-driven" reasoning

(defvar *fc-facts* '(fever cough))

(defun forward-chain-example ()
  (format t "Known facts: ~a~%" *fc-facts*)

  ;; Rule 1: fever + cough => flu
  (when (and (member 'fever *fc-facts*)
             (member 'cough *fc-facts*))
    (push 'flu *fc-facts*)
    (format t "Rule fired: fever + cough => FLU~%"))

  ;; Rule 2: flu => prescribe-medicine
  (when (member 'flu *fc-facts*)
    (push 'prescribe-medicine *fc-facts*)
    (format t "Rule fired: flu => PRESCRIBE MEDICINE~%"))

  (format t "Final facts: ~a~%" *fc-facts*))

;; (forward-chain-example)
;; Output: fever + cough => flu => prescribe medicine


;; --- 17. BACKWARD CHAINING ---
;; Start from GOAL => trace back to required conditions => ask/verify
;; "Goal-driven" reasoning

(defvar *bc-facts* '())

(defun ask-user (fact prompt)
  "Ask if a fact is true, add to known facts if yes"
  (unless (member fact *bc-facts*)
    (format t "~a (yes/no): " prompt)
    (when (equal (read) 'yes)
      (push fact *bc-facts*))))

(defun backward-chain-example ()
  (format t "Goal: Can we prescribe medicine?~%~%")

  ;; To prescribe medicine, need flu
  ;; To prove flu, need fever + cough
  (format t "Step 1: Need to prove FLU~%")
  (format t "Step 2: FLU needs FEVER and COUGH~%~%")

  (ask-user 'fever "Does patient have fever?")
  (ask-user 'cough "Does patient have cough?")

  (if (and (member 'fever *bc-facts*)
           (member 'cough *bc-facts*))
      (progn
        (push 'flu *bc-facts*)
        (push 'prescribe-medicine *bc-facts*)
        (format t "~%Conclusion: FLU confirmed => PRESCRIBE MEDICINE~%"))
      (format t "~%Conclusion: Cannot confirm flu~%")))

;; (backward-chain-example)
