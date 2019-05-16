;;; dtaneli16@ku.edu.tr    Thu Nov  2 11:02:27 2017
;;; 	   	 	     
;;; Comp200 Project 3 	   	 	     
;;; 	   	 	     
;;; Due November ?, 2017 	   	 	     
;;; 	   	 	     
;;; 	   	 	     
;;; Before you start: 	   	 	     
;;; 	   	 	     
;;; * Please read the detailed instructions for this project from the
;;; file project3.pdf available in the course website.
;;; 	   	 	     
;;; 	   	 	     
;;; While you are working: 	   	 	     
;;; * Type all your work and notes in the appropriate sections of this file.
;;; * Please do not delete any of the existing lines.
;;; * Use the procedure names given in the instructions.
;;; * Remember to frequently save your file.
;;; * Use semicolon (;) to comment out text, tests and unused code.
;;; * Remember to document your code.
;;; * Remember our collaboration policy: you can discuss with your friends but:
;;; 	   	 	     
;;;   *** NOTHING WRITTEN GETS EXCHANGED ***
;;; 	   	 	     
;;; !!! Do that before submitting the project:
;;;  When you are done with the project3.scm, please click on the "file" button at
;;;  the upper left side, go to the "Save Other" section and click on the
;;; "Save Definitions As Text..." item. It is important for you to do that before the submission.
;;;  Don't worry, this won't change anything in your code; but is required for grading.
;;; 	   	 	     
;;; 	   	 	     
;;; When you are done: 	   	 	     
;;; 	   	 	     
;;; * Perform a final save and submit your work following the instructions.
;;; * Please do not make any modifications after midnight on the due date.
;;; * Please send an email comp200@ku.edu.tr if you have any questions.
;;; * Make sure your file loads without errors:
;;; 	   	 	     
;;; *** IF LOADING GIVES ERRORS YOUR PROJECT WILL NOT BE GRADED ***
;;; 	   	 	     
;;; Before the definition of each procedure, please write a description
;;; about what the procedure does and what its input and output should
;;; be, making sure the lines are commented out with semi-colons.
;;; 	   	 	     
;;; Code for this project: "databases.scm". You should read this file
;;; since you will use the procedures defined in it.
;;; 	   	 	     
;;;   Completing assignment by using Racket/DrRacket
;;;   By default DrRacket does not allow you to redefine primitives and
;;;   also load external files via load procedure. To fix these problems,
;;;   in the "Choose Language" panel, select R5RS language in "Other
;;;   Languages" section, click on "Show Details" and uncheck "Disallow
;;;   redefinition of initial bindings".
 	   	 	     
;;; The following lines are necessary, please do not delete:
 	   	 	     
(define your-answer-here -1) 	   	 	     
(load "databases.scm") 	   	 	     
 	   	 	     
;;; Let's start 0_0 	   	 	     
;;; problem 1 ;;; DONE	   	 	     
 	   	 	     
;; your code should have the following general form
 	   	 	     
(define example-table
  (make-empty-table (list (make-column 'name 'symbol) (make-column 'major 'number))))

(table-insert! (list 'ben 6) example-table)
(table-insert! (list 'jen 3) example-table)
(table-insert! (list 'amy 12) example-table)
(table-insert! (list 'kim 13) example-table)
(table-insert! (list 'alex 6) example-table)	   	 	     
 	   	 	     
;; test cases 	   	 	     
(table-display example-table)
;displayed correctly
 	   	 	     
;;; problem 2 ;;; DONE	   	 	     
(define (table-insert-all! lst table)
  (if (null? lst)
      table
      (table-insert-all!
       (cdr lst)
       (table-insert! (car lst) table))
  )
  )


;; test cases 	   	 	     
 	   	 	     
 (define books (make-empty-table
 	       (list (make-column 'title 'symbol)
 		     (make-column 'author 'symbol)
 		     (make-column 'rating 'number))))
 	   	 	     
 	   	 	     
 (table-insert-all! '((sicp abelson-sussman 8)
 		     (return-of-the-king jrr-tolkien 9)
 		     (treatment-of-subordinates darth-vader 4)
 		     (project-grading tom 2)
 		     (all-things-stata frank-gehry 5)
 		     (biting-the-hand-that-feeds-me my-cat 1))
 		   books) 	   	 	     
 (table-display books) 	   	 	     
 	   	 	     
;;; problem 3 ;;; DONE	   	 	     
;; Hint: Writing (filter predicate lst) might be helpful
;(define (filter predicate lst)
;  (if (null? lst) '()
;      (if (predicate (car lst))
;          (cons (car lst)
;                (filter predicate (cdr lst)))
;         (filter predicate (cdr lst)))))


(define (table-select selector table)
  (make-table (get-table-columns table) (filter selector (get-table-data table))))
  

 	   	 	     
;; test cases
 	   	 	     
; (display "Testing Problem 3\n")
; (table-display 	   	 	     
;  (table-select 	   	 	     
;   (lambda (row) 	   	 	     
;     (> (get 'rating row) 4))
;   books)) 	   	 	     
 	   	 	     
;;; problem 4 ;;; DONE 	   	 	     
 	   	 	     
;; Hint: Be careful about the comparator operator of the corresponding
;; row.  Writing a (get-column-type row column-name) might be helpful.
(define (get-column-type row column-name)
  (let ((collist1 (row-columns row)))
    (define (help collist column-name)
      (if (null? collist) (error "couldnt get column type")
          (if (eq? column-name (caar collist))
              (cadar collist)
              (help (cdr collist) column-name))))
    (help collist1 column-name))) 	   	 	     

;(get-column-type (car (get-table-data books)) 'title)
;works as expected
;but didnt need it

(define (table-order-by column table)
  (let ((unsortedrows (get-table-data table)))
   (make-table (get-table-columns table)
               (sort (make-row-comparator column table) unsortedrows))))


;; test cases 	   	 	     
; (display "Testing Problem 4\n")
; (table-display 	   	 	     
;  (table-order-by 'rating books)
; ) 	   	 	     
 	   	 	     
; (table-display 	   	 	     
;  (table-order-by 'title books)
; ) 	   	 	     
 	   	 	     
;;; problem 5 ;;; DONE	   	 	     
 	   	 	     
(define (table-delete! pred table)
  (let ((table-data (get-table-data table)))
    (let ((newdata (filter (lambda (row) (not (pred row))) table-data)))
          (change-table-data! table newdata)
          )))

;(define (table-delete! pred table)
;  (let ((table-data (get-table-data table)))
;    (let ((remove-list (filter pred table-data)))
;      (define (help pred table remove-list)
;               (if (null? (cdr remove-list))
;                   (remove (car remove-list) table-data)
;                   (begin (remove (car remove-list) table-data)
;                         (help pred table (cdr remove-list))))
;        table-data)
;          (change-table-data! table (help pred table remove-list))
;          )))

;(filter (lambda (row) 	   	 	     
;   (eq? (get 'author row) 'my-cat)) (get-table-data books))

;(remove (car (filter (lambda (row) 	   	 	     
;   (eq? (get 'author row) 'my-cat)) (get-table-data books))) (get-table-data books))

;(remove 1 (list 1 2 3 4))
 	   	 	     
;; test cases 	   	 	     
; (display "Testing Problem 5\n")
; (table-delete! 	   	 	     
;  (lambda (row) 	   	 	     
;   (eq? (get 'author row) 'my-cat))
; books)

 	   	 	     
; (table-display books) 	   	 	     
 	   	 	     
;;; problem 6 ;;; DONE	   	 	     
(define (table-update! pred column proc table)
  (define (help pred column proc table table-data)
    (if (null? table-data) table
        (if (pred (car table-data))
            (if (row-type-check (row-col-replace (car table-data) column (proc (car table-data))))
                (begin (set-car! table-data (row-col-replace (car table-data) column (proc (car table-data))))
                       (help pred column proc table (cdr table-data)))
                (error "type check failed in table-update!"))
            (help pred column proc table (cdr table-data))))
    )
  (help pred column proc table (get-table-data table)))

;(let ((unfiltdata (get-table-data table)))
;    (let ((filteddata (filter pred unfiltdata)))
;      (let ((modifieddata (map proc filteddata)))
;        (change-table-data! table modifieddata))))


;((lambda (row) (or (eq? (get 'name row) 'amy) (eq? (get 'name row) 'alex))) (car (get-table-data example-table)))
;(row-col-replace (car (get-table-data example-table)) 'major ((lambda (row) '9) (car (get-table-data example-table))))

;; test cases 	   	 	     
 	   	 	     
; (display "Testing Problem 6\n")
; (table-update! (lambda (row) (or (eq? (get 'name row) 'amy) (eq? (get 'name row) 'alex)))
;               'major 	   	 	     
;               (lambda (row) '9)
;               example-table)
; (table-display example-table)
 	   	 	     
;;; problem 7 ;;; DONE	   	 	     
 	   	 	     
(define *type-table* 	   	 	     
  (list 	   	 	     
        (list 'number number? <)
        (list 'symbol symbol? symbol<?)
        (list 'string string? string<?))) 	   	 	     
 	   	 	     
(define example-table2
  (make-empty-table (list (make-column 'name 'string) (make-column 'major 'number)))) 	   	 	     
 	   	 	     
;; test cases 	   	 	     
; (display "Testing Problem 7\n")
; (table-insert! '("jen" 3) example-table2)
; (table-insert! '("ben" 6) example-table2)
; (table-insert! '("alex" 6) example-table2)
; (table-insert! '("amy" 12) example-table2)
; (table-insert! '("kim" 13) example-table2)
 	   	 	     
 	   	 	     
; (table-display example-table2)
; (display "\nordered example-table2\n")
; (table-display 	   	 	     
;  (table-order-by 'name example-table2)
; ) 	   	 	     
 	   	 	     
;;; problem 8 ;;; DONE	   	 	     
 	   	 	     
;; Hint: Writing these two procedures might be helpful (contains? lst
;; x) returns true if x in the lst and (get-pos lst x) returns the
;; position of x if it is in the list.
;; Ex: (get-pos '(1 2 3 4) 2) => 2
;;     (get-pos '(1 2 3 4) 5) => 0

(define (contains? lst x)
  (if (null? lst) #f
      (if (eq? (car lst) x) #t
          (contains? (cdr lst) x))))

(define (get-pos lst x)
  (define (help lst x cnt)
    (if (null? lst) 0
        (if (eq? (car lst) x)
            cnt
            (help (cdr lst) x (+ 1 cnt)))))
  (help lst x 0))

(define (make-enum-checker lst)
  (lambda (x) (if (contains? lst x) #t #f))	   	 	     
  ) 	   	 	     
(define (make-enum-comparator lst)
  (lambda (x y) (if (< (get-pos lst x) (get-pos lst y)) #t #f))
) 	   	 	     
(define *days* '(sunday monday tuesday Wednesday thursday friday saturday))
(define day-checker (make-enum-checker *days*))
(define day-comparator (make-enum-comparator *days*))
 	   	 	     
;; test cases 	   	 	     
; (display "Testing Problem 8\n")
; (day-checker 'monday)   ;=> #t
; (day-checker 7)         ;=> #f
; (day-comparator 'monday 'tuesday)   ;=> #t (monday is "less than" tuesday)
; (day-comparator 'friday 'sunday)    ;=> #f (sunday is before friday)
 	   	 	     
 	   	 	     
(define *type-table* 	   	 	     
  (list 	   	 	     
        (list 'number number? <)
        (list 'symbol symbol? symbol<?)
        (list 'string string? string<?)
        (list 'day day-checker day-comparator))
) 	   	 	     
 	   	 	     
; (define example-table3 	   	 	     
;   (make-empty-table 	   	 	     
;    (list (make-column 'name 'string)
;          (make-column 'date 'day)
;          (make-column 'major 'number)))
;    ) 	   	 	     
 	   	 	     
; (table-insert! '("jen" monday 3) example-table3)
; (table-insert! '("ben" sunday 6) example-table3)
; (table-insert! '("alex" friday 6) example-table3)
; (table-insert! '("amy" tuesday 1) example-table3)
; (table-insert! '("kim" saturday 2) example-table3)
 	   	 	     
; (table-display example-table3)
; (display "\nordered example-table3\n")
; (table-display 	   	 	     
;  (table-order-by 'date example-table3)
; ) 	   	 	     
 	   	 	     
;;; Problem 9 ;;;DONE 	   	 	     
;; Hint: Similar with Problem 8

(define *gender* '(male female all))
(define gender-checker 	   	 	     
   (make-enum-checker *gender*)	   	 	     
) 	   	 	     
(define gender-comparator 	   	 	     
  (make-enum-comparator *gender*) 	   	 	     
)

(define *race* '(white black red all))
 	   	 	     
(define race-checker 	   	 	     
  (make-enum-checker *race*)
) 	   	 	     
(define race-comparator 	   	 	     
  (make-enum-comparator *race*)
) 	   	 	     
 	   	 	     
(define *type-table* 	   	 	     
  (list 	   	 	     
        (list 'number number? <)
        (list 'symbol symbol? symbol<?)
        (list 'string string? string<?)
        (list 'day day-checker day-comparator)
        (list 'gender gender-checker gender-comparator)
        (list 'race race-checker race-comparator)
        )
) 	   	 	     
 	   	 	     
;;; Problem 10 ;;;DONE	   	 	     
 	   	 	     
(define person-table 	   	 	     
  (make-empty-table
   (list
    (make-column 'name 'string)
    (make-column 'race 'race)
    (make-column 'gender 'gender)
    (make-column 'birthyear 'number)
   ))
) 	   	 	     
;;; tests 	   	 	     
; (display "Testing Problem 10\n")
; (table-insert! '("jen" white female 1983) person-table)
; (table-insert! '("axe" black male 1982) person-table)
; (table-display person-table)
 	   	 	     
 	   	 	     
;;; Problem 11 ;;;DONE
 	   	 	     
(define (make-person name race gender birthyear)
  (table-insert! (list name race gender birthyear) person-table)
  name) 	   	 	     
 	   	 	     
;; test cases 	   	 	     
 	   	 	     
; (display "Testing Problem 11\n")
 	   	 	     
; (define p1 (make-person "Alex" 'white 'male 1983))
; (define p2 (make-person "Clark" 'black 'male 1982))
; (define p3 (make-person "Sneijder" 'white 'male 1977))
; (table-display person-table)
 	   	 	     
;;; Note that you might delete the test people you created by typing
; (table-delete! (lambda (x) #t) person-table)
;;; And you can verify the removal operation by typing
; (display "\nDeleted Person Table\n")
; (table-display person-table)
;;; 	   	 	     
;;; Note that, you might need to create test people again in later questions
 	   	 	     
 	   	 	     
;;; Problem 12 	;;; DONE   	 	     
 	   	 	     
(define (person-name person) person)
 	   	 	     
(define (lookup-person-row person)
  (let ((person-data (get-table-data person-table)))
    (define (help person person-data)
    (if (null? person-data)
        (error "person not found")
        (if (eq? (person-name person) (cdadar person-data))
            (car person-data)
            (help person (cdr person-data)))))
    (help person person-data))
  )
	   	 	     
(define (person-race person) 	   	 	     
  (get 'race (lookup-person-row person)))
 	   	 	     
(define (person-gender person) 	   	 	     
  (get 'gender (lookup-person-row person)))
 	   	 	     
(define (person-birthyear person)
  (get 'birthyear (lookup-person-row person)))
 	   	 	     
(define (person-age person) 	   	 	     
; returns an approximation to the person's age in years
  (let ((*current-year* 2017)) 	   	 	     
    (- *current-year* (person-birthyear person))))
 	   	 	     
;; test cases 	   	 	     
;;; Note that, you can create test people to check the selectors.
; (display "Testing Problem 12\n")
; (lookup-person-row p1) 	   	 	     
; (person-race p1) 	   	 	     
; (person-gender p1) 	   	 	     
; (person-birthyear p1) 	   	 	     
; (person-age p1) 	   	 	     
; (lookup-person-row "Sneijder")
; (lookup-person-row "bogus")

 	   	 	     
;;; Problem 13 ;;;DONE 	   	 	     
 	   	 	     
(define (update-person-row! person colname newvalue)
  (let ((person-data (get-table-data person-table)))
    (define (help person colname newvalue person-data)
    (if (null? person-data)
        (error "person not found")
        (if (eq? (person-name person) (cdadar person-data))
            (set-car! person-data (row-col-replace (car person-data)
                                                  colname
                                                  newvalue))
            (help person colname newvalue person-data))))
    (help person colname newvalue person-data))
  ) 	   	 	     
 	   	 	     
(define (set-person-name! person newname)
  (update-person-row! person 'name newname))
 	   	 	     
(define (set-person-race! person newrace)
  (update-person-row! person 'race newrace))
 	   	 	     
(define (set-person-gender! person newgender)
  (update-person-row! person 'gender newgender))
 	   	 	     
(define (set-person-birthyear! person newbirthyear)
  (update-person-row! person 'birthyear newbirthyear))
 	   	 	     
;; QUESTION What happens? Why? Comments?
"(person-name alyssa) and (person-race alyssa) gave errors because 
 we used the person's name to get the information about the person in 
 (lookup-person-row person) but now the person's name has changed 
 so we can't reach the person's information" 	   	 	     
 	   	 	     
;;; test cases 	   	 	     
 	   	 	     
; (display "Testing Problem 13\n")
; (define alyssa (make-person "alyssa-p-hacker" 'black 'female 1978))
; (set-person-name! alyssa "alyssa-p-hacker-bitdiddle")  ; got married!
; (table-display person-table)
; (person-name alyssa) 	   	 	     
; (person-race alyssa) 	   	 	     
 	   	 	     
;;; Note: after running the test cases above, please comment out them again.
 	   	 	     
;;; Problem 14 ;;;DONE 	   	 	     
 	   	 	     
(define life-table 	   	 	     
  (make-empty-table
   (list (make-column 'year 'number)
         (make-column 'all-all 'number)
         (make-column 'all-male 'number)
         (make-column 'all-female 'number)
         (make-column 'white-all 'number)
         (make-column 'white-male 'number)
         (make-column 'white-female 'number)
         (make-column 'black-all 'number)
         (make-column 'black-male 'number)
         (make-column 'black-female 'number)
         ))
  ) 	   	 	     
 	   	 	     
(table-insert-all! life-expect-data life-table)
 	   	 	     
; (display "Selecting 1950\n")
; (table-display 	   	 	     
;  (table-select 	   	 	     
;  (lambda (row) 	   	 	     
;    (= (get 'year row) 1950))
;  life-table)) 	   	 	     
 	   	 	     
;;; Problem 15 ;;;DONE
(define (convert-one-year lst)
  (list (list (car lst) 'all 'all (cadr lst))
        (list (car lst) 'all 'male (caddr lst))
        (list (car lst) 'all 'female (cadddr lst))
        (list (car lst) 'white 'all (cadddr (cdr lst)))
        (list (car lst) 'white 'male (cadddr (cddr lst)))
        (list (car lst) 'white 'female (cadddr (cdddr lst)))
        (list (car lst) 'black 'all (cadddr (cddddr lst)))
        (list (car lst) 'black 'male (cadddr (cddddr (cdr lst))))
        (list (car lst) 'black 'female (cadddr (cddddr (cddr lst))))
        ))
(define (convert-lifetable lst)
;; Converts the data to the (year race gender expected) format
  (if (null? lst) '()
      (append (convert-one-year (car lst)) (convert-lifetable (cdr lst))))
) 	   	 	     

;; test cases 	   	 	     
  	 	      	   	 	     
(define life-expect-data-new (convert-lifetable life-expect-data))

;life-expect-data-new
 	   	 	     
(define life-table-new 	   	 	     
   (make-empty-table 	   	 	     
   (list (make-column 'year 'number)
         (make-column 'race 'race)
         (make-column 'gender 'gender)
         (make-column 'expected 'number)
   ))) 	   	 	     
(table-insert-all! life-expect-data-new life-table-new)
;(display "Selecting 1950 from new data\n")
;(table-display 	   	 	     
;(table-select 	   	 	     
; (lambda (row) 	   	 	     
;  (= (get 'year row) 1950)) 	   	 	     
;  life-table-new)) 	   	 	     
 	   	 	     

;;; Problem 16 ;;; DONE	   	 	     
(display "\nTesting Problem 16\n")
(define problem16-table ;unordered	   	 	     
  (table-select (lambda (row) (and (>= (get 'year row) 1950)
                                   (<= (get 'year row) 1959)
                                   (eq? (get 'race row) 'white)
                                   (eq? (get 'gender row) 'female))) life-table-new)) 	   	 	     
 	   	 	     
 	   	 	     
;;; QUESTION Was life expectancy for white women steadily increasing
;;; in this decade? 	   	 	     
"No. Life expectancy increased from 1950 to 1959 but it is not steady. 
 Some years it was lower than the year before" 	   	 	     
 	   	 	     
(table-display 	   	 	     
  (table-order-by 'expected problem16-table)
) 	   	 	     


;(define black-f-table 	   	 	     
;  (table-select (lambda (row) (and (>= (get 'year row) 1950)
;                                   (<= (get 'year row) 1959)
;                                   (eq? (get 'race row) 'black)
;                                   (eq? (get 'gender row) 'female))) life-table-new))
;(table-display 	   	 	     
;  (table-order-by 'expected black-f-table)
;)


;; Paste the output of Problem 16 for black female
;; between 1950 and 1959 	   	 	     
;;
"year	race	gender	expected	
1950	black	female	62.9	
1951	black	female	63.4	
1952	black	female	63.8	
1953	black	female	64.5	
1957	black	female	65.5	
1958	black	female	65.8	
1954	black	female	65.9	
1956	black	female	66.1	
1955	black	female	66.1	
1959	black	female	66.5"

  	 	      	   	 	     
;;; Problem 17 	;;;DONE   	 	     
 	   	 	     
(define p3 (make-person "GeorgeBest" 'white 'female 1987))
(define p4 (make-person "Lizarazu" 'white 'male 1940))
 	   	 	     
(define (expected-years person)
  (let ((tbl (table-select (lambda (row) (and (= (get 'year row) (person-birthyear person))
                                               (eq? (get 'race row) (person-race person))
                                               (eq? (get 'gender row) (person-gender person)))) life-table-new)))
    (- (get 'expected (car (get-table-data tbl)))
       (person-age person))
    )) 	   	 	     
;; test cases 	   	 	     
(display "Testing problem 17 \n") 	   	 	     
(expected-years p3) 	   	 	     
(expected-years p4) 	   	 	     
