(ns different.core)

(require '[clojure.core.match :refer [match]])

(defn self-evaluating?
  [expr]
  (or (number? expr)
      (and 
        (not (symbol? expr))
        (match expr
               ([op & els] :seq) (and (symbol? op) (every? self-evaluating? els))))))

(defn reduce-numbers
  [numbers]
  (def to-reduce
    (map eval (filter self-evaluating? numbers)))
  (if (empty? to-reduce)
    to-reduce
    (list (apply + to-reduce))))

(defn number-of-occurences
  [sym col]
  (count (filter #(= % sym) col)))

(defn simplify
  "Simplifies expression"
  [expr]
  (match expr
         (['* & els] :seq) (concat (cons '* (reduce-numbers els))
                                   (let [symbols (filter (complement self-evaluating?) els)]
                                     (map (fn [el]
                                            (def occurence-count (number-of-occurences el symbols))
                                            (if (= occurence-count 1)
                                              el
                                              (list 'expt el occurence-count))) (set symbols))))
         :else expr))

(defn diff
  "Differentiate expression by symbol"
  [by expr]
  (let [expr (simplify expr)]
    (match expr
           by 1 ; x' => 1
           (['quote el] :seq) (diff by el)
           (['* a] :seq) (diff by a)
           (['* a b & els] :seq) (concat
                                   (list '+ `(* ~(diff by a) ~b) `(* ~(diff by b) ~a))
                                   (if (empty? els) 
                                     els
                                     (diff by (cons '(+) els))))
           (['expt by c] :seq) (cond (number? c) (list '* c (list 'expt by (dec c)))
                                     (list? c) (list '* (list 'Math/log by) expr))
           (['expt a x] :seq) (if (and (seq? x) (self-evaluating? x))
                                0
                                `(* (Math/log ~a) ~expr))
           (['Math/sin x] :seq) (if
                                  (self-evaluating? x) 0
                                  `(* (Math/cos ~x) ~(diff by x)))
           (['Math/cos x] :seq) (if
                                  (self-evaluating? x) 0
                                  `(* (- (Math/cos ~x)) ~(diff by x)))

           (['+ & els] :seq) (concat '(+) (map #(diff by %) els))
           (['- & els] :seq) (concat '(+) (map #(diff by %) els))
           c 0)))
