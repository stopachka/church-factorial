(ns church-factorial)

; def#
; ----

(defmacro def#
  "A light wrapper around `def`, that keeps track of the
  _source code_ for each definition

  This let's us _unwrap_ all the definitions later : >"
  [name v]
  `(do
     (def ~name ~v)
     (alter-meta! (var ~name) assoc :source {:name '~name :v '~v})
     (var ~name)))

; pair
; ----

(def# church-pair (fn [a b]
                    (fn [selector]
                      (selector a b))))

(def# take-first-arg (fn [a b] a))
(def# church-first (fn [pair]
                     (pair take-first-arg)))

(def# take-second-arg (fn [a b] b))
(def# church-second (fn [pair]
                      (pair take-second-arg)))

(comment
  (def# ex-pair (church-pair 0 1))
  (church-first ex-pair)
  (church-second ex-pair))

; factorial-clj
; -------------

(defn factorial-clj [n]
  (if (zero? n)
    1
    (* n (factorial-clj (dec n)))))

(comment (factorial-clj 5))

; church-numerals: 0-2
; ------------

(def# zero (fn [f v] v))

(def# one (fn [f v]
            (f (zero f v))))

(def# two (fn [f v]
            (f (one f v))))

; church-numeral->int
; ---------------

(defn church-numeral->int [church-numeral]
  (church-numeral inc 0))

(comment
  (map church-numeral->int [zero one two]))

; church-inc
; --------------

(def# church-inc
      (fn [church-numeral]
        (fn [f v]
          (f (church-numeral f v)))))

(comment (church-numeral->int (church-inc (church-inc one))))

; int->church-numeral
; -----------------------------

(defn int->church-numeral [clojure-int]
  (if (zero? clojure-int)
    zero
    (church-inc (int->church-numeral (dec clojure-int)))))

(comment
  (church-numeral->int (int->church-numeral 5)))

; shift-and-inc
; -------------

(def# shift-and-inc (fn [pair]
                      (church-pair
                        (church-second pair)
                        (church-inc (church-second pair)))))

(comment
  (let [p (shift-and-inc (church-pair one two))]
    (map church-numeral->int [(church-first p) (church-second p)])))

; church-dec
; -----------

(def# church-dec
      (fn [church-numeral]
        (church-first
          (church-numeral shift-and-inc
                          (church-pair zero zero)))))

(comment
  (church-numeral->int (church-dec (int->church-numeral 10))))

; church-*
; --------------

(def# church-*
      (fn [num-a num-b]
        (fn [f v]
          (num-a (partial num-b f) v))))

(comment
  (church-numeral->int
    (church-* (int->church-numeral 5) (int->church-numeral 5))))

; church-booleans
; ---------------

(def# church-true (fn [when-true when-false]
                    when-true))

(def# church-false (fn [when-true when-false]
                     when-false))

; church-bool->bool
; -----------------------

(defn church-bool->bool [church-bool]
  (church-bool true false))

(comment
  (church-bool->bool church-true)
  (church-bool->bool church-false))

; church-if
; ---------

(def# church-if (fn [church-bool when-true when-false]
                  (church-bool when-true when-false)))

(comment
  (church-numeral->int
    (church-if church-true one two))
  (church-numeral->int
    (church-if church-false one two)))

; church-zero?
; ------------

(def# church-zero?
      (fn [church-numeral]
        (church-numeral (fn [v] church-false) church-true)))

(comment
  (church-bool->bool (church-zero? zero))
  (church-bool->bool (church-zero? one)))


; factorial-v0
; ------------

(def# factorial-v0
      (fn [church-numeral-n]
        ((church-if
           (church-zero? church-numeral-n)
           (fn [] one)
           (fn []
             (church-*
               church-numeral-n
               (factorial-v0 (church-dec church-numeral-n))))))))

(comment
  (church-numeral->int (factorial-v0 (int->church-numeral 5))))

; y-combinator
; ------------

(def# make-recursable
      (fn [injectable-f]
        ((fn [recursion-handler] (recursion-handler recursion-handler))
         (fn [recursion-handler]
           (injectable-f (fn [next-arg]
                           ((recursion-handler recursion-handler) next-arg)))))))

; factorial-yc
; ------------

(def# injectable-factorial
      (fn [factorial-cb]
        (fn [church-numeral-n]
          ((church-if
             (church-zero? church-numeral-n)
             (fn [] one)
             (fn []
               (church-*
                 church-numeral-n
                 (factorial-cb (church-dec church-numeral-n)))))))))

(def# factorial-yc (make-recursable injectable-factorial))

(comment
  (church-numeral->int (factorial-yc (int->church-numeral 5))))

; expand
; ------

(defn expand
  "This takes a form like

  (church-numeral->int (factorial-yc (int->church-numeral 5)))

  And expands all the function definitions, to give
  us the intuition for how our 'lambda calculus' way would look!"
  [form]
  (cond
    (symbol? form)
    (if-let [source (some-> (str *ns* "/" form)
                            symbol
                            find-var
                            meta
                            :source)]
      (expand (:v source))
      form)

    (seq? form)
    (map expand form)

    :else form))

(comment (expand '(factorial-yc (church-inc (church-inc (church-inc two))))))
