(defn evalexp [exp bindings] (simplify (bind-values bindings exp)))

(defn simplify [l]
  (let [l
    (if(seq? l)
      (map(fn [x]
        (if (seq? x)
          (simplify x) x)
        )l
      )l
    )
  ]
  (case (first l)
    and (simplify-and l)
    or (simplify-or l)
    not (simplify-not l)
  ))
)

(defn bindValues [m l]
  (map (fn [i] 
         (if (seq? i)
           (bind-values m i)
           (m i i)))
        l))


(defn simplifyOr [l]
(cond
   (some true? l)true
   (=(count l)2)(nth l 1)
   (some false? l)(evaluateOr(remove false? l))
   :else l
))


(defn evaluateOr [list]
(cond
   (=(count list)2)(second list)
   :else list
))

(defn simplifyAnd [list]
  (cond
    (some false? list)false
    (=(count list)2)(second list)
    (apply = (rest list))(nth list 1)
    (some true? list)(evaluateAnd(remove true? list))
    :else list
  ))

(defn evaluateAnd [list]
  (cond
    (=(count list)2)(second list)
    :else list
  )
)

(defn simplifyNot [list] 
  (cond
    (some false? list)true
    (some true? list)false
    :else list
  )
)
