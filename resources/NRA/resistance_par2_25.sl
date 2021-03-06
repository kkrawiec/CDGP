(set-logic QF_NRA)
(synth-fun resistance_par2 ((r1 Real)(r2 Real)) Real)
(declare-var r1 Real)
(declare-var r2 Real)
(constraint (= (resistance_par2 8.38724 20.17358) 5.924222663046789))
(constraint (= (resistance_par2 18.25929 1.74098) 1.5894314778850485))
(constraint (= (resistance_par2 2.2424 9.6893) 1.8209715564420828))
(constraint (= (resistance_par2 10.40556 20.05244) 6.850642444231401))
(constraint (= (resistance_par2 13.79987 7.75852) 4.96635265399689))
(constraint (= (resistance_par2 19.97424 10.53623) 6.897736636479215))
(constraint (= (resistance_par2 7.46664 16.90049) 5.1786925523687035))
(constraint (= (resistance_par2 8.12253 20.61867) 5.827027599233853))
(constraint (= (resistance_par2 19.22185 19.93845) 9.78679670820959))
(constraint (= (resistance_par2 4.13129 1.17402) 0.9142193549104575))
(constraint (= (resistance_par2 12.82906 2.95222) 2.3999452207425507))
(constraint (= (resistance_par2 15.85486 18.48494) 8.534590644336891))
(constraint (= (resistance_par2 0.34152 20.89345) 0.3360273663678357))
(constraint (= (resistance_par2 4.99616 17.61543) 3.8922299028418608))
(constraint (= (resistance_par2 17.57266 19.79296) 9.30842192565251))
(constraint (= (resistance_par2 17.25502 8.80186) 5.8286437339082795))
(constraint (= (resistance_par2 15.83819 4.53737) 3.5269572056080913))
(constraint (= (resistance_par2 5.92192 18.03128) 4.457851045271613))
(constraint (= (resistance_par2 10.9409 3.52167) 2.6641350260016026))
(constraint (= (resistance_par2 3.5452 20.32105) 3.018580064316765))
(constraint (= (resistance_par2 16.06565 16.09436) 8.039996092476342))
(constraint (= (resistance_par2 5.88656 20.33971) 4.565305066164575))
(constraint (= (resistance_par2 1.97972 5.60353) 1.4628847013615536))
(constraint (= (resistance_par2 2.49795 5.91612) 1.7563642748396433))
(constraint (= (resistance_par2 2.69225 7.02518) 1.9463521584410692))

(constraint (and
    (=> (and (> r1 0.0) (> r2 0.0)) (= (resistance_par2 r1 r2) (resistance_par2 r2 r1)))
    (=> (and (> r1 0.0) (> r2 0.0)) (>= (resistance_par2 r1 r2) (+ r1 r2)))))
(check-synth)
