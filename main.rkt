; ali gokcek
; 2021400012
; compiling: yes
; complete: yes
#lang racket

(provide (all-defined-out))

;binary_to_decimal
(define (binary_to_decimal binary)
  (string->number binary 2))

;relocator
(define (relocator args limit base)
  (map (lambda (addr)
         (let ([decimal (binary_to_decimal addr)])
           (if (> decimal limit)
               -1
               (+ decimal base))))
       args))

;divide_address_space
(define (divide_address_space address page_size)
  (let* ([page_off_bits (+ 10 (ceiling (log page_size 2)))]   ; output is an inexact integer like 5.0 because of log func
         [address_length (string-length address)]
         [page_number (substring address 0 (inexact->exact (- address_length page_off_bits)))]  ;convert inexact to exact int 5.0 -> 5
         [page_offset (substring address (inexact->exact (- address_length page_off_bits)))])
    (list page_number page_offset)))

;page
(define (page args page_table page_size)
  (map (lambda (addr)
         (let* ([divided (divide_address_space addr page_size)]
                [page_number (car divided)]
                [offset (cadr divided)]
                [frame (list-ref page_table (binary_to_decimal page_number))])  
           (string-append frame offset)))
       args))

;find_sin

;helper functions
(define (degrees_to_radians deg)
  (* deg (/ pi 180))
  )

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))


(define (taylor x num)
  (letrec ((term (lambda (n)
                   (* (/ (expt -1 n) (factorial (+ (* 2 n) 1))) (expt x (+ (* 2 n) 1)))))
           (helper (lambda (n acc)
                     (if (= n num)
                         acc
                         (helper (add1 n) (+ acc (term n)))))))
    (helper 0 0.0)))
;wrapper function
(define (find_sin value num)
  (taylor (degrees_to_radians value) num))


;myhash

(define (myhash arg table_size)
  (let* ([decimal (binary_to_decimal arg)]
         [num_terms (+ 1 (modulo decimal 5))]
         [sin (abs (find_sin decimal num_terms))]  ;taking the abs value of find_sin for easier calculation of first 10 digits after the dec. point 
         [digit_list(string->list (substring (number->string sin) 2 12))]
         [sum (apply + (map (lambda (d) (string->number (string d))) ;first convert digit strings to integers then sum them with (apply +) func 
                                   digit_list))])
    (modulo sum table_size)))

;hashed_page

(define (hashed_page arg table_size page_table page_size)
  (let* ([divided (divide_address_space arg page_size)]
         [page_number (car divided)]
         [offset (cadr divided)]
         [hash (myhash page_number table_size)]
         [hash_table_pairs (list-ref page_table hash)]
         [frame (my_find page_number hash_table_pairs)])
    (string-append frame offset)))
    
(define (my_find page_number pairs)  ;my_find func uses tail recursion to look for each element on the list for the match
  (cond
    [(string=? (car (car pairs)) page_number) (cadr (car pairs))]
    [else (my_find page_number (cdr pairs))]))

;split_addresses

(define (split_addresses args size)
  (let ([len (string-length args)])
    (map (lambda (i)
           (list (substring args (* i size) (* (+ i 1) size)))) ; for size=8: for i=0 idx 0-7, for i=1 idx 8-15 ...
         (range 0 (/ len size)))))       ; if len is 40 and size is 8 then i is "0 1 2 3 4"

;map_addresses

(define (map_addresses args table_size page_table page_size address_space_size)
  (map (lambda (addr)
         (hashed_page (car addr) table_size page_table page_size))
       (split_addresses args address_space_size)))


