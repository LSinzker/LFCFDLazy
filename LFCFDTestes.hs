module LFCFDTestes where

import LFCFDLazy

import Test.HUnit

v5 = Valor 5

let1 = Let "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))
          -- let x = 5 in (\y -> x+y)3
aplicacao = Let "x" (Valor 5) (Aplicacao (Lambda "y" (Soma (Ref "x") (Ref "y"))) (Valor 3))

let2 = Let "x" (Valor 3) (Soma(Ref "x")(Valor 1))
--teste imagem relativa ao exemplo no livro(cap. 8)
let3 = Let "x" (Soma(Valor 4)(Valor 5)) (Let "y" (Soma(Ref "x")(Ref "x")) (Let "z" (Ref "y") (Let "x" (Valor 4) (Ref "z"))))

let4 = Let "x" (Valor 3) (Ref "x")

let5 = Let "x" (Valor 4) (Soma (Ref "x")(Ref "x"))

let6 = Let "x" (Soma(Valor 3)(Valor 4)) ((Soma(Ref"x")(Ref"x")))
--     let x = 2 in let y 4 + x in x
let7 = Let "x" (Valor 2) (Let "y" (Soma(Valor 4)(Ref "x")) (Ref "x"))

let8 = Let "x" (Subtracao (Valor 4)(Valor 1)) (Let "y" (Multiplicacao(Ref "x")(Ref "x")) (Ref "y"))

if01 = If0 (Subtracao(Valor 2)(Valor 2)) (Valor 1) (Valor 0)

rec0 = Rec "fac" (Lambda "x" (If0 (Ref "x") (Valor 1) (Multiplicacao(Ref "x") (Rec "fac" (Ref "fac") (Subtracao(Ref"x")(Valor 1)))))) (Valor 4)

rec1 = Rec "fac" (Lambda "x" (If0 (Ref "x") (Valor 1) (Multiplicacao(Ref "x") (Rec "fac" (Ref "fac") (Subtracao(Ref"x")(Valor 1)))))) (Valor 5)
--          "nome"                            parada   --------------------------comportamento---------------------------
rec2 = Rec "sum" (Lambda "x" (If0 (Ref "x") (Valor 0) (Soma(Ref "x") (Rec "sum" (Ref "sum") (Subtracao(Ref"x")(Valor 1))))  ) ) (Valor 10)
                              ----------------------------------------------------------------------------------------------
rec3 = Rec "bin" (Lambda "x" (If0 (Ref "x") (Valor 1) (Multiplicacao(Valor 2) (Rec "bin" (Ref "bin") (Subtracao(Ref"x")(Valor 1)))))) (Valor 3)

if0 = (If0 (Ref "x") (Valor 1) (Multiplicacao(Ref "y") (Rec "pot" (Ref "pot") (Subtracao(Ref"x")(Valor 1)))))

rec4 = Let "y" (Valor 3) (Rec "pot" (Lambda "x" if0) (Valor 3))

teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) (avaliar v5 []))

teste2 = TestCase (assertEqual "avaliar let x = 5 in (\\y -> x + y) 3" (VInt 8) (avaliar aplicacao []))
--myLazyTests
teste3 = TestCase (assertEqual "avaliar let x = 3 in x+1" (VInt 4) (avaliar let2 []))

teste4 = TestCase (assertEqual "avaliar let x = 4+5 in let y = x+x in let z = y in let x = 4 in z" (VInt 18) (avaliar let3 []))

teste5 = TestCase (assertEqual "avaliar let x= 3 in x" (VInt 3) (avaliar let4 []))

teste6 = TestCase (assertEqual "avaliar let x=4 in x+x" (VInt 8) (avaliar let5 []))

teste7 = TestCase (assertEqual "avaliar let x=3+4 in x+x" (VInt 14) (avaliar let6 []) )

teste8 = TestCase (assertEqual "avaliar let x=2 in let y = 4 + x in x" (VInt 2) (avaliar let7 []) )

teste9 = TestCase (assertEqual "avaliar let x=4-1 in let y=x*x in y" (VInt 9) (avaliar let8 []) )
--myRecursionTests
teste10 = TestCase (assertEqual "If0" (VInt 1) (avaliar if01 []))

teste11 = TestCase (assertEqual "Recursao fatorial" (VInt 24) (avaliar rec0 []) )

teste12 = TestCase (assertEqual "Recursao fatorial" (VInt 120) (avaliar rec1 []) )

teste13 = TestCase (assertEqual "Recursao somatoria" (VInt 55) (avaliar rec2 []))

teste14 = TestCase (assertEqual "Recursao mult binarios" (VInt 8) (avaliar rec3 []))

teste15 = TestCase (assertEqual "Recursao potencia" (VInt 27) (avaliar rec4 []))

todosOsTestes = TestList [ teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         , teste7
                         , teste8
                         , teste9
                         , teste10
                         , teste11
                         , teste12
                         , teste13
                         , teste14
                         , teste15
                         ]

executarTestes = runTestTT todosOsTestes
