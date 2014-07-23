; Source: https://gist.github.com/msszczep/12168573f29159e86dfc

; variable evaluation: find and substitute
; look for Var X in B and substitute a
; environment, symbol
 
(def MyEnv {"One" 1 
            "Two" 2 
            "Three" 3
            "MyAdd" +})
 
 
(defn MyEval [[term f & args]]
  (cond 
    (= term "MyApply") (apply (MyEnv f) (map MyEnv args))
    :else "You Failed"))
 
 
(MyEval ["MyApply" "MyAdd" "One" "Two" "TwoA"])
 
 
(MyEval ["cheese" "MyAdd" "One" "Two" "Three"])
