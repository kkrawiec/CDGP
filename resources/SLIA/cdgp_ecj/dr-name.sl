(set-logic SLIA)

(synth-fun f ((name String)) String
    ((Start String (ntString))
     (ntString String (name " " "." "Dr."
                       (str.++ ntString ntString)
                       (str.replace ntString ntString ntString)
                       (str.at ntString ntInt)
                       (int.to.str ntInt)
                       (str.substr ntString ntInt ntInt)))
      (ntInt Int (0 1 2
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
    (and (distinct (str.indexof s " " 0) (- 1))  ; there must be at least one space
         (= (str.indexof s " " (+ (str.indexof s " " 0) 1)) (- 1))  ; only one space
         (>= (str.len s) 3)                      ; at least 3 chars
         (distinct (str.at s 0) " ")             ; first char must be non-space
         (distinct (str.at s (- (str.len s) 1)) " ")  ; last char must be non-space
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (constraint (= (f "Nancy FreeHafer") "Dr. Nancy"))
; (constraint (= (f "Andrew Cencici") "Dr. Andrew"))
; (constraint (= (f "Jan Kotas") "Dr. Jan"))
; (constraint (= (f "Mariya Sergienko") "Dr. Mariya"))
; Example solution: (str.++ "Dr." (str.++ " " (str.substr name 0 (str.indexof name " " 0))))

(declare-var s String)
(constraint (=> (precond s)  (= (str.len (f s)) (+ (str.indexof s " " 0) 4))))
(constraint (=> (precond s)  (= (ithSplit (f s) " " 0) "Dr." )))
(constraint (=> (precond s)  (= (ithSplit (f s) " " 1) (ithSplit s " " 0) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-synth)

