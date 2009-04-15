;;
(assign <reg1> (op car) (reg <reg2>))
(assign <reg1> (op cdr) (reg <reg2>))
  →
    (assign <reg1> (op vector-ref) (reg the-cars) (reg <reg2>))
    (assign <reg1> (op vector-ref) (reg the-cdrs) (reg <reg2>))

(perform (op set-car!) (reg <reg1>) (reg <reg2>))
(perform (op set-cdr!) (reg <reg1>) (reg <reg2>))
  →
    (perform (op vector-set!) (reg the-cars) (reg <reg1>) (reg <reg2>))
    (perform (op vector-set!) (reg the-cdrs) (reg <reg1>) (reg <reg2>))

;;
(assign <reg1> (op cons) (reg <reg1>) (reg <reg2>))
  →
    (perform (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
    (perform (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
    (assign <reg1> (reg free))
    (assign free (op +) (reg free) (const 1))

;;
(op eq?) (reg <reg1>) (reg <reg2>)
  →
    レジスタの全フィールドの等価をテスト

(op pair?)
(op null?)
(op symbol?)
(op member?)
  →
    型フィールドを調べるだけでよい

;;;;;;
(save <reg>)
  →
    (assign the-stack (op cons) (reg <reg>) (reg the-stack))

(restore <reg>)
  →
    (assign <reg> (op car) (reg the-stack))
    (assign the-stack (op cdr) (reg the-stack))

(perform (op initialize-stack))
  →
    (assign the-stack (const ()))


