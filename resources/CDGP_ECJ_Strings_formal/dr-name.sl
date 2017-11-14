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
            (ite (= i 0)
                (ite (= firstSpacePos (- 1))
                     s ; Return the whole string, there was no space
                     (str.substr s 0 firstSpacePos))
                (ite (= i 1)
                    (ite (= firstSpacePos (- 1))
                        "" ; There was no space, so index 1 is out of bounds
                        (ite (= SecondSpacePos (- 1))
                            (str.substr s (+ firstSpacePos 1) (str.len s)) ; till the end of the String
                            (str.substr s (+ firstSpacePos 1) (- (- SecondSpacePos 1) firstSpacePos)) ; to the next space; second arg of str.substr is shift, not position
                        )
                    )
                    "" ; Unhandled values of i
                )
            )

      )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (constraint (= (f "Nancy FreeHafer") "Dr. Nancy"))
; (constraint (= (f "Andrew Cencici") "Dr. Andrew"))
; (constraint (= (f "Jan Kotas") "Dr. Jan"))
; (constraint (= (f "Mariya Sergienko") "Dr. Mariya"))

(declare-var s String)
(constraint (= (ithSplit (f s) " " 0) "Dr." ))
(constraint (= (ithSplit (f s) " " 1) (ithSplit s " " 0) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(check-synth)
