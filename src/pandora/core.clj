(ns pandora.core
  (:require
   [clojure.tools.macro :as macros]))

;; --------------------------------------------------------------------------------
;; General Utility
;; --------------------------------------------------------------------------------

(defn symb
  "Construct a symbol from a list of strings, symbols, or keywords."
  [& s]
  (symbol (clojure.core/apply str s)))

(defn find-by
  "Find the first item in a sequence that satisfies a predicate."
  [pred seq & [result]]
  (for [x seq :when (pred x)]
    (if result (result x) x)))

(defn atom?
  "Check if an item is an atom."
  [item]
  (instance? clojure.lang.Atom item))

(defn nthcdr
  "Return the nth cdr of a list"
  [n l]
  (if (zero? n) l
      (recur (dec n) (rest l))))

(defn group
  "Group a sequence into n elements"
  [source n]
  (letfn [(rec [source acc]
            (let [r (nthcdr n source)]
              (cond (empty? r)
                    (reverse (cons source acc))
                    (seq? r)
                    (recur (vec r)
                           (cons (subvec source 0 n) acc))
                    :else
                    (reverse
                     (cons source acc)))))]
    (if source (rec source nil) nil)))

;; --------------------------------------------------------------------------------
;; Helper Functions and Macros for Pandoric Scope
;; --------------------------------------------------------------------------------

(defn let-binding-transform
  "Transforms let bindings"
  [letargs]
  (let [pos (atom 0)]
    (for [x letargs]
      (if (even? @pos)
        (do (swap! pos inc) (symb x "#"))
        (do (swap! pos inc)
            `(atom  ~x))))))

(defn macrolet-binding-transform
  "Transforms macrolet bindings"
  [letargs]
  (let [pos (atom 0) ll (atom nil)]
    (vec (concat (for [x letargs]
                   (if (even? @pos)
                     (do (swap! pos inc) (reset! ll x) x)
                     (do (swap! pos inc)
                         `(deref ~(symb @ll "#")))))
                 (for [x letargs]
                   (if (even? @pos)
                     (do (swap! pos inc)
                         (reset! ll x)
                         (symb "x" x))
                     (do (swap! pos inc)
                         (symb ll "#"))))))))

(defn pget
  "Constructor for pget accessor"
  [letargs]
  `(case (symb "%" ~'sym)
     ~@(mapcat (fn [x]
                 `(~(symb "%" (first x)) ~(first x))) letargs)
     "none found"))

(defn pget-all
  "Constructor for pget-all accessor"
  [letargs]
  `(mapcat (fn [~'n]
          (list ~'n
             (case (symb "%" ~'n)
               ~@(mapcat (fn [x]
                               `(~(symb "%" (first x)) ~(first x)))
                             letargs))))
           '~(map first letargs)))

(defn pset
  "Constructor for pset accessor"
  [letargs]
  `(case (symb "%" ~'sym)
     ~@(mapcat (fn [x]
               `[~(symb "%" (first x))
                 (reset! ~(symb (first x) "#") ~'val)])
             letargs)
     :else "none found"))

(defn pswap
  "Constructor for pswap accessor"
  [letargs]
  `(case (symb "%" ~'sym)
     ~@(mapcat (fn [x]
               `[~(symb "%" (first x))
                 (swap! ~(symb (first x) "#") ~'fn)])
             letargs)
     :else "none found"))

(defn atom-scope
  "Provides set macro atoms bound within the scope."
  [& body]
  `(macros/macrolet [(~'set [~'sym ~'val]
                           `(reset! ~(symb ~'sym "#") ~~'val))]
                    ~@body))

;; --------------------------------------------------------------------------------
;; Core Pandoric Macros
;; --------------------------------------------------------------------------------
(defmacro d-fn
  "
  --------------------------------------------------------------------------------
  Destructing or Dispatching Macro
  --------------------------------------------------------------------------------
  Provides a macro that allows for the dispatching of functions based on the first argument.
  "
  [& ds]
  `(fn [& ~'args]
     (case (first ~'args)
       ~@(mapcat
          (fn [d]
            (if (= true (first d))
              `((apply (fn ~@(rest d)) ~'args))
              `(~(first d)
                (apply (fn ~@(rest d))
                       (rest ~'args)))))
          ds))))


(defmacro plet
  "
  --------------------------------------------------------------------------------
  Pandoric Let
  --------------------------------------------------------------------------------
  The idea is to provide a let macro that allows for lexical bindings to be 'opened', thus the pandora's box analogy.
  The plet macro provides a set of functions that allow for access and manipulation of the lexical bindings,
  which are stored in atoms to allow for thread safe operations on the lexical bindings.
  Provides:
  * pget - get the value of a lexical binding
  * pget-all - get all the lexical bindings
  * pset - set the value of a lexical binding using reset!
  * pswap - swap the value of a lexical binding using swap!
  * pget-vars - get the names of all the lexical bindings
  --------------------------------------------------------------------------------
"
  [letargs & body]
  (let [letargs# (let-binding-transform (concat '(this (atom nil)) letargs))
        letlist# (group letargs 2)]
    `(let [~@letargs#]
       (macros/symbol-macrolet
        ~(macrolet-binding-transform letargs)
        ~@(butlast body)
        ~(atom-scope
          `(d-fn
            (:pget [~'sym]
                   ~(pget letlist#))
            (:pget-all []
                       ~(pget-all letlist#))
            (:pset [~'sym ~'val]
                   ~(pset letlist#))
            (:pswap [~'sym ~'fn]
                    ~(pswap letlist#))
            (:pget-vars []
                        (map #'first '~letlist#))
            (true [& ~'args]
                  (apply ~(last body) ~'args))))))))

(defn get-p
  "Returns the value of a lexical binding from a pandoric function."
  [box sym]
  (box :pget sym))

(defn set-p
  "Set the value of a lexical binding from a pandoric function."
  [box sym val]
  (box :pset sym val))

(defn swap-p
  "Swap the value of a lexical binding from a pandoric function using swap!."
  [box sym fn]
  (box :pswap sym fn))

(defn get-all-p
  "Returns all the lexical bindings from a pandoric function."
  [box]
  (box :pget-all))

(defmacro with-p
  "Provides access to the provided lexical bindings within the scope."
  [syms box & body]
  `(macros/symbol-macrolet
    (~@(mapcat
        (fn [x]
          `(~x (get-p ~box '~x)))
        syms))
    ~@body))

(defmacro with-all-p
  "Provides access to all the lexical bindings within the scope.
  Allows a lexical scope to be passed around to different contexts, and operate on the lexical bindings."
  [box & body]
  `(macros/macrolet [(~'set [~'sym ~'val]
                            `(set-p ~'~box '~~'sym ~~'val))]
                    (macros/symbol-macrolet
                     (~@(mapcat
                         (fn [x]
                           `(~x (get-p ~box '~x)))
                         ((or (resolve box) box) :pget-vars)))
                     ~@body)))

(defmacro rescope-p
  "Resets the default function call of a pandoric function to a new function, with the same lexical bindings."
  [pfn fn-args & body]
  `(with-all-p ~pfn
     (~(or (resolve pfn) pfn) :rescope
                       (fn ~fn-args
                         ~@body))))

(defmacro pfn
  "
  --------------------------------------------------------------------------------
  Pandoric Lambda Function
  --------------------------------------------------------------------------------
  A lambda function that provides access to the lexical bindings of the scope in which it was created.
  Similar to plet, but with an implicit lambda in which the body is evaluated.
  Provides:
  * pget - get the value of a lexical binding
  * pget-all - get all the lexical bindings
  * pset - set the value of a lexical binding using reset!
  * pswap - swap the value of a lexical binding using swap!
  * pget-vars - get the names of all the lexical bindings
  * rescope - rescope the lexical bindings to a new function
  --------------------------------------------------------------------------------
  "
  [fn-args pan-args & body]
  (let [letargs# (let-binding-transform (concat '(this nil) pan-args))
        letlist# (group pan-args 2)]
    `(let [~@letargs#]
       (macros/symbol-macrolet
        ~(macrolet-binding-transform (concat '(this nil) pan-args))
        ~(atom-scope
          `(reset! ~'this#
                   (fn ~fn-args ~@body))
          `(d-fn
            (:pget [~'sym]
                   ~(pget letlist#))
            (:pget-all []
                       ~(pget-all letlist#))
            (:pset [~'sym ~'val]
                   ~(pset letlist#))
            (:pswap [~'sym ~'fn]
                    ~(pswap letlist#))
            (:pget-vars []
                        (map #'first '~letlist#))
            (:rescope [~'new]
                      (reset! ~'this# ~'new))
            (true [& ~'args]
                  (apply ~'@this# ~'args))))))))

(defmacro defp
  "
  --------------------------------------------------------------------------------
  Pandoric Function Definition
  --------------------------------------------------------------------------------
  A macro that provides a way to define a function with lexical bindings.
  Similar to defn, but with an implicit pandoric lambda function.
  "
  [name lambda-args pan-args & body]
  `(def ~name (pfn ~lambda-args ~pan-args ~@body)))

(def ^:dynamic p-eval-tunnel)

(defmacro p-eval
  "Evaluate an expression with lexical bindings.
  Allows for tunneling bindings into an eval expression.
  "
  [box expr]
  `(let [p-eval-tunnel# '~box]
     (eval `(~'with-all-p ~p-eval-tunnel#
                          ~~expr))))

(defmacro p-eval*
  "A simplified version of p-eval that does not require the use of the with-p macro.
  Less powerful, but more convenient for simple use cases."
  [vars expr]
  `(eval
    `(macros/symbol-macrolet
      [~@'~(mapcat (fn [x] `(~(first x) ~(second x)))
                   (group vars 2))]
      ~~expr)))
