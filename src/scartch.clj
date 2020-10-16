(ns scartch)

(def λ 1)
(def x 1)

  (λ x . x)

 (fn [f x] (f x))


(λ x . x)

(fn [x] x)
(def a)
(def b)

((fn [a b]
   (fn [selector]
     (selector a b)))
 a b)

((λ a. λ b. λ selector . selector a b)
 a b)

(def make-pair (fn [a b]
                 (make-pair ...)))
