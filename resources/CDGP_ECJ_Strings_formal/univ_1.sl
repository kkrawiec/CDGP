(set-logic SLIA)
 
(synth-fun f ((col1 String) (col2 String)) String
    ((Start String (ntString))
     (ntString String (firstname lastname " " ","
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
(declare-var col1 String)
; (constraint (= (f "University of Pennsylvania" "Phialdelphia, PA, USA") "University of Pennsylvania, Phialdelphia, PA, USA"))

(check-synth)