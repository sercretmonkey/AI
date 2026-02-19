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
