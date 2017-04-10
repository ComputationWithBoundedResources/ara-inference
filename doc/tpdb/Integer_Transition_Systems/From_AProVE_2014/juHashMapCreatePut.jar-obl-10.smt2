(declare-sort Loc 0)
(declare-const f1_0_main_Load Loc)
(declare-const f3875_0_random_ArrayAccess Loc)
(declare-const f617_0_createMap_Return Loc)
(declare-const f3797_0_createMap_LE Loc)
(declare-const f4358_0_put_InvokeMethod Loc)
(declare-const f4448_0_put_NULL Loc)
(declare-const f4529_0_put_EQ Loc)
(declare-const f4991_0_transfer_GE Loc)
(declare-const f5087_0_transfer_ArrayAccess Loc)
(declare-const __init Loc)
(assert (distinct f1_0_main_Load f3875_0_random_ArrayAccess f617_0_createMap_Return f3797_0_createMap_LE f4358_0_put_InvokeMethod f4448_0_put_NULL f4529_0_put_EQ f4991_0_transfer_GE f5087_0_transfer_ArrayAccess __init ))

(define-fun cfg_init ( (pc Loc) (src Loc) (rel Bool) ) Bool
  (and (= pc src) rel))

(define-fun cfg_trans2 ( (pc Loc) (src Loc)
                         (pc1 Loc) (dst Loc)
                         (rel Bool) ) Bool
  (and (= pc src) (= pc1 dst) rel))

(define-fun cfg_trans3 ( (pc Loc) (exit Loc)
                         (pc1 Loc) (call Loc)
                         (pc2 Loc) (return Loc)
                         (rel Bool) ) Bool
  (and (= pc exit) (= pc1 call) (= pc2 return) rel))

(define-fun init_main ( (pc Loc) (arg1 Int) (arg2 Int) (arg3 Int) (arg4 Int) (arg5 Int) (arg6 Int) (arg7 Int) (arg8 Int) (arg9 Int) (arg10 Int) (arg11 Int) (arg12 Int) (arg13 Int) ) Bool
  (cfg_init pc __init true))

(define-fun next_main (
                 (pc Loc) (arg1 Int) (arg2 Int) (arg3 Int) (arg4 Int) (arg5 Int) (arg6 Int) (arg7 Int) (arg8 Int) (arg9 Int) (arg10 Int) (arg11 Int) (arg12 Int) (arg13 Int)
                 (pc1 Loc) (arg1P Int) (arg2P Int) (arg3P Int) (arg4P Int) (arg5P Int) (arg6P Int) (arg7P Int) (arg8P Int) (arg9P Int) (arg10P Int) (arg11P Int) (arg12P Int) (arg13P Int)
             ) Bool
  (or
    (cfg_trans2 pc f1_0_main_Load pc1 f3875_0_random_ArrayAccess (exists ((x7 Int)) (and (and (and (and (> x7 (- 1)) (> arg2 0)) (> arg1 0)) (> arg1P 3)) (= arg2 arg2P))))
    (cfg_trans2 pc f617_0_createMap_Return pc1 f3875_0_random_ArrayAccess (and (and (and (and (and (and (and (and (and (<= arg1P arg2) (> arg1 0)) (> arg2 14)) (> arg1P 14)) (<= (+ arg5 3) arg2)) (= 16 arg4)) (= 12 arg6)) (= 16 arg3P)) (= arg5 arg4P)) (= 12 arg5P)))
    (cfg_trans2 pc f1_0_main_Load pc1 f3797_0_createMap_LE (and (and (and (and (and (and (and (and (and (> arg2P (- 1)) (> arg2 0)) (<= (- arg1P 14) arg1)) (> arg1 0)) (> arg1P 14)) (= arg2 arg3P)) (= 1 arg4P)) (= 16 arg5P)) (= 0 arg6P)) (= 12 arg7P)))
    (cfg_trans2 pc f3797_0_createMap_LE pc1 f3797_0_createMap_LE (exists ((x29 Int) (x30 Int)) (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg2 0) (< (+ arg4 1) arg3)) (> arg3 (- 1))) (> arg4 (- 1))) (> x29 (- 1))) (> x30 (- 1))) (> arg5 1)) (> arg1 3)) (> arg1P 3)) (<= (+ arg7 3) arg1)) (<= (+ arg6 3) arg1)) (= (- arg2 1) arg2P)) (= arg3 arg3P)) (= (+ arg4 2) arg4P))))
    (cfg_trans2 pc f3875_0_random_ArrayAccess pc1 f4358_0_put_InvokeMethod (exists ((x32 Int) (x38 Int) (x39 Int)) (and (and (and (and (and (and (and (and (and (and (and (and (and (> x32 (- 1)) (< (+ x32 1) arg2)) (> x38 (- 1))) (> x39 (- 1))) (> arg2 0)) (> arg3 1)) (<= arg1P arg1)) (> arg1 3)) (> arg1P 3)) (<= (+ arg5 3) arg1)) (<= (+ arg4 3) arg1)) (= arg3 arg2P)) (= arg4 arg3P)) (= arg5 arg4P))))
    (cfg_trans2 pc f3797_0_createMap_LE pc1 f4358_0_put_InvokeMethod (exists ((x48 Int) (x49 Int)) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg2 0) (< (+ arg4 1) arg3)) (> arg3 (- 1))) (> arg4 (- 1))) (> x48 (- 1))) (> x49 (- 1))) (> arg5 1)) (<= arg1P arg1)) (> arg1 3)) (> arg1P 3)) (<= (+ arg7 3) arg1)) (<= (+ arg6 3) arg1)) (= arg5 arg2P)) (= arg6 arg3P)) (= arg7 arg4P))))
    (cfg_trans2 pc f4358_0_put_InvokeMethod pc1 f4448_0_put_NULL (and (and (and (and (and (and (and (and (and (and (< arg3P arg2) (> arg2 1)) (<= arg1P arg1)) (> arg1 3)) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg4 3) arg1)) (<= (+ arg3 3) arg1)) (= arg2 arg5P)) (= arg3 arg6P)) (= arg4 arg7P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4529_0_put_EQ (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (> arg5 1)) (<= arg3P arg4)) (<= (+ arg9P 2) arg4)) (> arg1 3)) (> arg4 2)) (> arg1P 3)) (> arg3P 2)) (> arg9P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (>= arg4 (+ arg2 2))) (<= (+ arg10P 4) arg4)) (= arg3 arg2P)) (= 0 arg4P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)) (= arg2 arg8P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4529_0_put_EQ (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (> arg5 1)) (<= arg3P arg4)) (<= (+ arg9P 2) arg4)) (> arg1 3)) (> arg4 2)) (> arg1P 3)) (> arg3P 2)) (> arg9P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (>= arg4 (+ arg2 2))) (<= (+ arg10P 4) arg4)) (= arg3 arg2P)) (= 1 arg4P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)) (= arg2 arg8P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4448_0_put_NULL (exists ((x90 Int)) (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (< x90 arg2)) (<= (+ arg4P 1) arg4)) (> arg1 3)) (> arg4 0)) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (= arg2 arg2P)) (= arg3 arg3P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P))))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4448_0_put_NULL (exists ((x211 Int)) (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (> x211 arg2)) (<= (+ arg4P 1) arg4)) (> arg1 3)) (> arg4 0)) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (= arg2 arg2P)) (= arg3 arg3P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P))))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4448_0_put_NULL (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (> arg5 1)) (<= (+ arg4P 2) arg4)) (> arg1 3)) (> arg4 1)) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (>= arg4 (+ arg2 2))) (= arg2 arg2P)) (= arg3 arg3P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4448_0_put_NULL (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (> arg5 1)) (<= (+ arg4P 2) arg4)) (> arg1 3)) (> arg4 2)) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (>= arg4 (+ arg2 2))) (= arg2 arg2P)) (= arg3 arg3P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)))
    (cfg_trans2 pc f4529_0_put_EQ pc1 f4448_0_put_NULL (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (<= arg1P arg1) (<= (+ arg4P 2) arg3)) (<= arg4P arg9)) (> arg1 3)) (> arg3 2)) (> arg9 (- 1))) (> arg1P 3)) (> arg4P (- 1))) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (>= arg3 (+ arg8 2))) (<= (+ arg10 4) arg3)) (= 0 arg4)) (= arg8 arg2P)) (= arg2 arg3P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4991_0_transfer_GE (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg5 arg3) (> arg5 1)) (<= arg7 arg6)) (>= (* 2 arg5) 0)) (< arg5 1073741824)) (<= (- arg1P 1) arg1)) (<= (+ arg2P 3) arg1)) (<= (- arg2P 1) arg4)) (<= (+ arg3P 3) arg1)) (<= (- arg3P 1) arg4)) (> arg1 3)) (> arg4 (- 1))) (> arg1P 3)) (> arg2P 0)) (> arg3P 0)) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (= 0 arg4P)) (= (+ arg6 1) arg5P)) (= arg7 arg6P)) (= (* 2 arg5) arg7P)) (= arg5 arg8P)))
    (cfg_trans2 pc f4448_0_put_NULL pc1 f4991_0_transfer_GE (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg5 arg3) (> arg5 1073741824)) (>= (* 2 arg5) 0)) (<= arg7 arg6)) (<= (- arg1P 1) arg1)) (<= (+ arg2P 3) arg1)) (<= (- arg2P 1) arg4)) (<= (+ arg3P 3) arg1)) (<= (- arg3P 1) arg4)) (> arg1 3)) (> arg4 (- 1))) (> arg1P 3)) (> arg2P 0)) (> arg3P 0)) (<= (+ arg6 3) arg1)) (<= (+ arg7 3) arg1)) (= 0 arg4P)) (= (+ arg6 1) arg5P)) (= arg7 arg6P)) (= (* 2 arg5) arg7P)) (= arg5 arg8P)))
    (cfg_trans2 pc f4991_0_transfer_GE pc1 f5087_0_transfer_ArrayAccess (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg7 0) (> arg8 arg4)) (<= arg1P arg1)) (<= (+ arg2P 3) arg1)) (<= arg2P arg2)) (<= arg2P arg3)) (<= (+ arg6P 3) arg1)) (<= arg6P arg2)) (<= arg6P arg3)) (> arg1 3)) (> arg2 0)) (> arg3 0)) (> arg1P 3)) (> arg2P 0)) (> arg4P (- 1))) (> arg5P 0)) (> arg6P 0)) (<= (+ arg5 3) arg1)) (<= (+ arg6 3) arg1)) (= arg4 arg3P)) (= arg5 arg8P)) (= arg6 arg9P)) (= arg8 arg10P)) (= arg7 arg13P)))
    (cfg_trans2 pc f5087_0_transfer_ArrayAccess pc1 f5087_0_transfer_ArrayAccess (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg13 arg7) (> arg13 0)) (<= arg1P arg1)) (<= (+ arg2P 3) arg1)) (<= arg2P arg2)) (<= arg2P arg4)) (<= (+ arg2P 2) arg5)) (<= arg2P arg6)) (<= (+ arg4P 1) arg4)) (<= (+ arg4P 3) arg5)) (<= arg5P arg4)) (<= (+ arg5P 2) arg5)) (<= (+ arg6P 3) arg1)) (<= arg6P arg2)) (<= arg6P arg4)) (<= (+ arg6P 2) arg5)) (<= arg6P arg6)) (> arg1 3)) (> arg2 0)) (> arg4 0)) (> arg5 2)) (> arg6 0)) (> arg1P 3)) (> arg2P 0)) (> arg4P (- 1))) (> arg5P 0)) (> arg6P 0)) (<= (+ arg8 3) arg1)) (<= (+ arg9 3) arg1)) (<= (+ arg11P 2) arg4)) (<= (+ arg12P 2) arg4)) (<= (+ arg11 2) arg5)) (<= (+ arg11P 4) arg5)) (<= (+ arg12P 4) arg5)) (<= (+ arg12 2) arg5)) (= arg3 arg3P)) (= arg8 arg8P)) (= arg9 arg9P)) (= arg10 arg10P)) (= arg13 arg13P)))
    (cfg_trans2 pc f4991_0_transfer_GE pc1 f4991_0_transfer_GE (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg8 (- 1)) (> arg8 arg4)) (<= arg1P arg1)) (<= (+ arg2P 3) arg1)) (<= arg2P arg2)) (<= arg2P arg3)) (<= (+ arg3P 3) arg1)) (<= arg3P arg2)) (<= arg3P arg3)) (> arg1 3)) (> arg2 0)) (> arg3 0)) (> arg1P 3)) (> arg2P 0)) (> arg3P 0)) (<= (+ arg5 3) arg1)) (<= (+ arg6 3) arg1)) (= (+ arg4 1) arg4P)) (= arg5 arg5P)) (= arg6 arg6P)) (= arg7 arg7P)) (= arg8 arg8P)))
    (cfg_trans2 pc f5087_0_transfer_ArrayAccess pc1 f4991_0_transfer_GE (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (> arg10 (- 1)) (> arg13 arg7)) (<= arg1P arg1)) (<= (+ arg2P 3) arg1)) (<= arg2P arg2)) (<= (- arg2P 1) arg4)) (<= (+ arg2P 1) arg5)) (<= arg2P arg6)) (<= (+ arg3P 3) arg1)) (<= arg3P arg2)) (<= (- arg3P 1) arg4)) (<= (+ arg3P 1) arg5)) (<= arg3P arg6)) (> arg1 3)) (> arg2 0)) (> arg4 (- 1))) (> arg5 1)) (> arg6 0)) (> arg1P 3)) (> arg2P 0)) (> arg3P 0)) (<= (+ arg8 3) arg1)) (<= (+ arg9 3) arg1)) (<= (+ arg11 2) arg5)) (<= (+ arg12 2) arg5)) (= (+ arg3 1) arg4P)) (= arg8 arg5P)) (= arg9 arg6P)) (= arg13 arg7P)) (= arg10 arg8P)))
    (cfg_trans2 pc __init pc1 f1_0_main_Load true)
  )
)
