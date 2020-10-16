(ns church-factorial)

; church-numerals: 0-2
; ------------

(def zero (fn [f]
            (fn [v] v)))

(def one (fn [f]
           (fn [v]
             (f ((zero f) v)))) )

(def two (fn [f]
           (fn [v]
             (f ((one f) v)))))

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
      (fn [v] (f ((church-numeral f) v))))))

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
        (fn [v]
          (
           (church-numeral-a
             (church-numeral-b f))
           v))))))

(comment
  (church-numeral->int
    ((church-* (int->church-numeral 5))
     (int->church-numeral 5))))

; church-booleans
; ---------------

(def church-true (fn [when-true]
                   (fn [when-false]
                     when-true)))

(def church-false (fn [when-true]
                    (fn [when-false]
                      when-false)))

; church-bool->bool
; -----------------------

(defn church-bool->bool [church-bool]
  ((church-bool true) false))

(comment
  (church-bool->bool church-true)
  (church-bool->bool church-false))

; church-if
; ---------

(def church-if (fn [church-bool]
                 (fn [when-true]
                   (fn [when-false]
                     ((church-bool when-true)
                      when-false)))))

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
       (fn [v] church-false))
     church-true)))

(comment
  (church-bool->bool (church-zero? zero))
  (church-bool->bool (church-zero? one)))

; pair
; ----

(def church-pair (fn [a]
                   (fn [b]
                     (fn [selector]
                       ((selector a) b)))))

(def church-first (fn [pair]
                    (pair church-true)))

(def church-second (fn [pair]
                     (pair church-false)))

(comment
  (let [p ((church-pair one) two)]
    (map church-numeral->int [(church-first p) (church-second p)])))

; shift-and-inc
; -------------

(def shift-and-inc (fn [pair]
                     ((church-pair
                        (church-second pair))
                      (church-inc (church-second pair)))))

(comment
  (let [p (shift-and-inc ((church-pair one) two))]
    (map church-numeral->int [(church-first p) (church-second p)])))

; church-dec
; -----------

(def church-dec
  (fn [church-numeral]
    (church-first
      ((church-numeral shift-and-inc)
       ((church-pair zero) zero)))))

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

(def call-arg-with-arg (fn [x] (x x)))

(def y-combinator
  (fn [f]
    (call-arg-with-arg
      (fn [recurr]
        (f (fn [next-arg-to-f]
             ((recurr recurr) next-arg-to-f)))))))

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

