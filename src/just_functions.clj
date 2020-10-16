(ns just-functions
  (:require [church-factorial :as cf]))

(comment
  (def factorial-of-5
    (
     (
      ; make-recursable
      (fn [injectable-f]
        ((fn [recursion-handler] (recursion-handler recursion-handler))
         (fn [recursion-handler]
           (injectable-f (fn [next-arg]
                           ((recursion-handler recursion-handler) next-arg))))))
      ; injectable-factorial
      (fn [factorial-cb]
        (fn [church-numeral-n]
          ((
            ; church-if
            (fn [church-bool when-true when-false]
              (church-bool when-true when-false))

            (
             ; church-zero?
             (fn [church-numeral]
               (church-numeral
                 (fn [v]
                   ; church-false
                   (fn [when-true when-false]
                     when-false))
                 ; church-true
                 (fn [when-true when-false]
                   when-true)))

             church-numeral-n)
            (fn []
              ; one
              (
               ; church-inc
               (fn [church-numeral]
                 (fn [f v]
                   (f (church-numeral f v))))
               ; zero
               (fn [f v] v)
               )
              )
            (fn []
              (
               ; church-*
               (fn [num-a num-b]
                 (fn [f v]
                   (num-a (partial num-b f) v)))
               church-numeral-n
               (factorial-cb
                 (
                  ; church-dec
                  (fn [church-numeral]
                    (
                     ; church-first
                     (fn [pair]
                       (pair
                         ; take-first-arg
                         (fn [a b] a)))
                     (church-numeral
                       (fn [pair]
                         (
                          ; church-pair
                          (fn [a b]
                            (fn [selector]
                              (selector a b)))
                          (
                           ; church-second
                           (fn [pair]
                             (pair
                               ; take-second-arg
                               (fn [a b] b)))
                           pair)
                          (
                           ; church-inc
                           (fn [church-numeral]
                             (fn [f v]
                               (f (church-numeral f v))))

                           (
                            ; church-second
                            (fn [pair]
                              (pair
                                ; take-second-arg
                                (fn [a b] b)))
                            pair))))

                       (
                        ; church-pair
                        (fn [a b]
                          (fn [selector]
                            (selector a b)))
                        ; zero
                        (fn [f v] v)
                        ; zero
                        (fn [f v] v)))))

                  church-numeral-n)))))))))

     ; 5
     (
      ; church-inc
      (fn [church-numeral]
        (fn [f v]
          (f (church-numeral f v))))
      (
       ; church-inc
       (fn [church-numeral]
         (fn [f v]
           (f (church-numeral f v))))
       (
        ; church-inc
        (fn [church-numeral]
          (fn [f v]
            (f (church-numeral f v))))
        (
         ; church-inc
         (fn [church-numeral]
           (fn [f v]
             (f (church-numeral f v))))
         (
          ; church-inc
          (fn [church-numeral]
            (fn [f v]
              (f (church-numeral f v))))
          ; zero
          (fn [f v] v)
          )))))))
  (cf/church-numeral->int factorial-of-5))

