(ns church-factorial)

; church-numerals: 0-2
; ------------

(def zero (fn [f]
            (fn [x] x)))

(def one (fn [f]
           (fn [x]
             (f ((zero f) x)))) )

(def two (fn [f]
           (fn [x]
             (f ((one f) x)))))

; church-numeral->int
; ---------------

(defn church-numeral->int [church-numeral]
  ((church-numeral inc) 0))

(comment
  (map church-numeral->int [zero one two]))

; church-inc
; --------------

(def church-inc
  (fn [church-numeral]
    (fn [f]
      (fn [x] (f ((church-numeral f) x))))))

(comment (church-numeral->int (church-inc (church-inc one))))

; int->church-numeral
; -----------------------------

(def int->church-numeral
  (fn [clojure-int]
    (if (zero? clojure-int)
      zero
      (church-inc (int->church-numeral (dec clojure-int))))))

(comment
  (church-numeral->int (int->church-numeral 5)))

; church-*
; --------------

(def church-*
  (fn [church-numeral-a]
    (fn [church-numeral-b]
      (fn [f]
        (fn [x]
          (
           (church-numeral-a
             (church-numeral-b f))
           x))))))

(comment
  (church-numeral->int
    ((church-* (int->church-numeral 5))
     (int->church-numeral 5))))

; church-booleans
; ---------------

(def church-true (fn [a]
                   (fn [b]
                     a)))

(def church-false (fn [a]
                    (fn [b]
                      b)))

; church-bool->bool
; -----------------------

(defn church-bool->bool [church-bool]
  ((church-bool true) false))

(comment
  (church-bool->bool church-true)
  (church-bool->bool church-false))

; church-if
; ---------

(def church-if (fn [predicate]
                 (fn [consequent]
                   (fn [alternative]
                     ((predicate consequent)
                      alternative)))))

(comment
  (church-numeral->int
    (((church-if church-true)
      one)
     two))
  (church-numeral->int
    (((church-if church-false)
      one)
     two)))


; church-zero?
; ------------

(def church-zero?
  (fn [church-numeral]
    ((church-numeral
       (fn [x] church-false))
     church-true)))

(comment
  (church-bool->bool (church-zero? zero))
  (church-bool->bool (church-zero? one)))


; church-dec
; -----------

(def church-dec
  "oh boy. this will need some serious unpacking"
  (fn [n]
    (fn [f]
      (fn [x]
        (((n
            (fn [g]
              (fn [h]
                (h
                  (g f)))))
          (fn [u]
            x))
         (fn [u]
           u))))))

(comment
  (church-numeral->int (church-dec (int->church-numeral 10))))

; church-
; ------------

(def church- (fn [church-numeral-a]
               (fn [church-numeral-b]
                 ((church-numeral-b church-dec)
                  church-numeral-a))))

(comment
  (church-numeral->int
    ((church- (int->church-numeral 5)) two)))

; church-<=
; ---------

(def church-<= (fn [church-numeral-a]
                 (fn [church-numeral-b]
                   (church-zero?
                     ((church- church-numeral-a)
                      church-numeral-b)))))

(comment
  (church-bool->bool ((church-<= two) one))
  (church-bool->bool ((church-<= one) two))
  (church-bool->bool ((church-<= one) one)))

; factorial-v0
; ------------

(def factorial-v0
  (fn [church-numeral-n]
    ((((church-if
         ((church-<= church-numeral-n) one))
       (fn [] church-numeral-n))
      (fn []
        ((church-*
           church-numeral-n)
         (factorial-v0 (church-dec church-numeral-n))))))))

(comment
  (church-numeral->int (factorial-v0 (int->church-numeral 5))))

; y-combinator
; ------------

(def y-combinator (fn [f]
                    ((fn [x] (x x))
                     (fn [x] (f (fn [y]
                                  ((x x) y)))))))


; factorial-yc
; ------------

(def factorial-f (fn [factorial]
                   (fn [church-numeral-n]
                     ((((church-if
                          ((church-<= church-numeral-n) one))
                        (fn [] church-numeral-n))
                       (fn []
                         ((church-*
                            church-numeral-n)
                          (factorial (church-dec church-numeral-n)))))))))

(def factorial-yc (y-combinator factorial-f))

(comment
  (church-numeral->int (factorial-yc (int->church-numeral 5))))

; expand without variable names!
; ------------------------------
(comment
  (church-numeral->int
    (
     (
      ; y-combintaor
      (fn [f]
        ((fn [x] (x x))
         (fn [x] (f (fn [y]
                      ((x x) y))))))
      ; factorial-f
      (fn [factorial]
        (fn [church-numeral-n]
          ((((
              ; church-if
              (fn [predicate]
                (fn [consequent]
                  (fn [alternative]
                    ((predicate consequent)
                     alternative))))
               (
                (
                 ; church-<=
                 (fn [church-numeral-a]
                   (fn [church-numeral-b]
                     (
                      ; church-zero?
                      (fn [church-numeral]
                        ((church-numeral
                           (fn [x]
                             ; church-false
                             (fn [a]
                               (fn [b]
                                 b))))
                         ; church-true
                         (fn [a]
                           (fn [b]
                             a))))
                       (
                        (
                         ; church-
                         (fn [church-numeral-a]
                           (fn [church-numeral-b]
                             ((church-numeral-b
                                ; church-dec
                                (fn [n]
                                  (fn [f]
                                    (fn [x]
                                      (((n
                                          (fn [g]
                                            (fn [h]
                                              (h
                                                (g f)))))
                                        (fn [u]
                                          x))
                                       (fn [u]
                                         u))))))
                              church-numeral-a)))
                         church-numeral-a)
                        church-numeral-b))))
                 church-numeral-n)
                ; one
                (
                 ; church-inc
                 (fn [church-numeral]
                   (fn [f]
                     (fn [x] (f ((church-numeral f) x)))))
                 ; zero
                 (fn [f]
                   (fn [x] x))))
              )
             (fn [] church-numeral-n))
            (fn []
              ((
                ; church-*
                (fn [church-numeral-a]
                  (fn [church-numeral-b]
                    (fn [f]
                      (fn [x]
                        (
                         (church-numeral-a
                           (church-numeral-b f))
                         x)))))
                 church-numeral-n)
               (factorial (
                           ; church-dec
                           (fn [n]
                             (fn [f]
                               (fn [x]
                                 (((n
                                     (fn [g]
                                       (fn [h]
                                         (h
                                           (g f)))))
                                   (fn [u]
                                     x))
                                  (fn [u]
                                    u)))))
                           church-numeral-n))))))))
      )
     ; 5
     (
      ; church-inc
      (fn [church-numeral]
        (fn [f]
          (fn [x] (f ((church-numeral f) x)))))
       (
        ; church-inc
        (fn [church-numeral]
          (fn [f]
            (fn [x] (f ((church-numeral f) x)))))
         (
          ; church-inc
          (fn [church-numeral]
            (fn [f]
              (fn [x] (f ((church-numeral f) x)))))
           (
            ; church-inc
            (fn [church-numeral]
              (fn [f]
                (fn [x] (f ((church-numeral f) x)))))
             (
              ; church-inc
              (fn [church-numeral]
                (fn [f]
                  (fn [x] (f ((church-numeral f) x)))))
              ; zero
              (fn [f]
                (fn [x] x))))))))))
