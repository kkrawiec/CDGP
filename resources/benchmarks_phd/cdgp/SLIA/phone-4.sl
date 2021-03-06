(set-logic SLIA)

 

(synth-fun f ((name String)) String
    ((Start String (ntString))
     (ntString String (name " " "(" ")" "-" "."
                       (str.++ ntString ntString)
                       (str.replace ntString ntString ntString)
                       (str.at ntString ntInt)
                       (int.to.str ntInt)
                       (str.substr ntString ntInt ntInt)))
      (ntInt Int (0 1 2 3 4 5
                  (+ ntInt ntInt)
                  (- ntInt ntInt)
                  (str.len ntString)
                  (str.to.int ntString)
                  (str.indexof ntString ntString ntInt)))
      (ntBool Bool (true false
                    (str.prefixof ntString ntString)
                    (str.suffixof ntString ntString)
                    (str.contains ntString ntString)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-fun ithSplit ((s String) (delimiter String) (i Int)) String
    (let ((firstSpacePos Int (str.indexof s delimiter 0)))
    (let ((SecondSpacePos Int (str.indexof s delimiter (+ firstSpacePos 1))))
    (let ((ThirdSpacePos Int (str.indexof s delimiter (+ SecondSpacePos 1))))

            (ite (= i 0)
                (ite (= firstSpacePos (- 1))
                     s ; Return the whole string, there was no space
                     (str.substr s 0 firstSpacePos)
                )
                (ite (= i 1)
                    (ite (= firstSpacePos (- 1))
                        "" ; There was no space, so index 1 is out of bounds
                        (ite (= SecondSpacePos (- 1))
                            (str.substr s (+ firstSpacePos 1) (str.len s)) ; till the end of the String
                            (str.substr s (+ firstSpacePos 1) (- (- SecondSpacePos 1) firstSpacePos)) ; to the next space; second arg of str.substr is shift, not position
                        )
                    )
                    (ite (= i 2)
                        (ite (or (= firstSpacePos (- 1)) (= SecondSpacePos (- 1)))
                            "" ; There was no space, so index 2 is out of bounds
                            (ite (= ThirdSpacePos (- 1))
                                (str.substr s (+ SecondSpacePos 1) (str.len s)) ; till the end of the String
                                (str.substr s (+ SecondSpacePos 1) (- (- ThirdSpacePos 1) SecondSpacePos)) ; to the next space; second arg of str.substr is shift, not position
                            )
                        )
                        "" ; Unhandled values of i (> 2)
                    )
                )
            )

    )))
)

(define-fun precond ((s String)) Bool
    (and (= (str.indexof s " " 0) (- 1))  ; no spaces
         (= (str.indexof s "-" 0) 3)
         (= (str.indexof s "-" 4) 7)
         (= (str.indexof s "-" 8) (- 1))
         (= (str.len s) 11)               ; xxx-xxx-xxx (11 chars)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (constraint (= (f "938-242-504") "938.242.504"))
; (constraint (= (f "308-916-545") "308.916.545"))
; (constraint (= (f "623-599-749") "623.599.749"))
; (constraint (= (f "981-424-843") "981.424.843"))
; (constraint (= (f "118-980-214") "118.980.214"))
; (constraint (= (f "244-655-094") "244.655.094"))

(declare-var s String)
(constraint (=> (precond s) (= (f s) (str.++ (ithSplit s "-" 0) "." (ithSplit s "-" 1) "." (ithSplit s "-" 2) ) )))

(check-synth)

