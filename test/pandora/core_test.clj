(ns pandora.core-test
  (:require [pandora.core :refer :all]
            [clojure.test :refer :all]))

(defp test [] [y 100]
  (set y (+ y 1)))
;; => #'pandora.core-test/test

(test)
;; => 101

(future
  (dotimes [x 1000000]
    (test)))
;; => #future[{:status :pending, :val nil} 0x56b182ed]
(test)
;; => 1000102

(defn do-something [box new]
  (when new
    (with-all-p box
      ;; even within a function we can access the vars
      (set y new))))

(do-something test 150)
;; => 150

(with-all-p test
  y)
;; => 150

(test :pget-all)
;; => (y 150)

(let [x 100]
  (with-all-p test
    (set y x))
  (p-eval test 'y))
;; => 100

(with-all-p test
  (print y))

(eval
 `(with-all-p test
    (print y)))

(rescope-p test [] (set y (* y y)))

(rescope-p test [] (set y (+ y y)))

(with-all-p test
  (set y 2))

(def test-p
  (time (plet [y 0 i 10]
              (fn []
                (set y (+ y 10))))))

(test-p)

(time (swap-p test-p 'i inc))

;; (time (dotimes [x 1000000]
;;         (test :pget 'y)))

;; (with-all-p test
;;   (set y (* y 10)))

;; (test :pget 'y)

;; (rescope-p test []
;;        (set y (* y y)))


;; (with-all-p test
;;   (set y))

;; (map first '[y 10])

;; (eval (p-eval [y 10] '(pp y)))

;; (with-all-p p-eval-tunnel (print i))

;; (eval (pp (p-eval [y 100] `(print ~'y))))

;; (with-all-p (p-eval-tunnel :pget-vars)
;;   (print i))

;; ;; (clifford.util/with-all-p
;; ;;   clifford.util/p-eval-tunnel
;; ;;   (clojure.core/print i))


;; (clifford.util/with-p
;;   clifford.util/vars
;;   clifford.util/p-eval-tunnel
;;   (clojure.core/println 'clifford.util/y))

;; (with-p [y 100]
;;   clifford.util/p-eval-tunnel
;;   (clojure.core/println 'clifford.util/y))

;; (clifford.util/with-p vars clifford.util/p-eval-tunnel nil)


;;                      ;; ]))

;; ;; --------------------------------------------------------------------------------
;; ;; Symbol macrolet injection into scope
;; ;; --------------------------------------------------------------------------------
;; (macros/macrolet [or [x]
;;                   ])
;; (defn add-macrolet [])

;; ;; Examples:
;; ;; --------------------------------------------------------------------------------



;; (time
;;  (dotimes [x 100000]
;;    (test-p)))

;; (test-p)
;; (test-p)

;; (with-p [y ]
;;   test-p
;;   (+ y i))

;; (with-all-p test-p
;;   (+ y i))

;; (test-p :pget-all)

