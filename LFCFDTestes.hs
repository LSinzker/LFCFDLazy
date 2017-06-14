module LFCFDTestes where

import LFCFD

import Test.HUnit

v5 = Valor 5

let1 = Let "x" (Valor 5) (Lambda "y" (Soma (Ref "x") (Ref "y")))

          -- let x = 5 in (\y -> x+y)3
aplicacao = Let "x" (Valor 5) (Aplicacao (Lambda "y" (Soma (Ref "x") (Ref "y"))) (Valor 3))

let2 = Let "x" (Valor 3) (Soma(Ref "x")(Valor 1))
--avaliar (Let "x" (Valor 3) (Soma(Ref "x")(Valor 1))) []
--avaliar (Aplicacao (Lambda "x" (Soma(Ref "x")(Valor 1))) (Valor 3)) []
--    v = avaliacaoStrict (avaliar (Lambda "x" (Soma(Ref "x")(Valor 1))) [])
--    v = avaliacaoStrict (FClosure "x" (Soma(Ref "x")(Valor 1)) [] ) []
--    v = FClosure "x" (Soma(Ref "x")(Valor 1)) []
--    e = EClosure (Valor 3) env
--avaliar (Soma(Ref "x")(Valor 1)) [("x", EClosure (Valor 3) env):env']
--avaliarExpBin (Ref "x") (Valor 1) (+) [("x", EClosure (Valor 3) env): env']
--    (VInt ve) = avaliar (Ref "x") [("x", EClosure (Valor 3) env): env']
--              = pesquisar "x" [("x", EClosure (Valor 3) env): env']
--              = avaliacaoStrict (avaliar (EClosure (Valor 3) env) env)
--              = avaliacaoStrict (Valor 3)
--              = VInt 3
--           ve = 3
--    (VInt vd) = avaliar (Valor 1) [("x", EClosure (Valor 3) env): env']
--              = VInt 1
--           vd = 1
--VInt ((+) 3 1)
--VInt 4

--teste imagem relativa ao exemplo no livro(cap. 8)
let3 = Let "x" (Soma(Valor 4)(Valor 5)) (Let "y" (Soma(Ref "x")(Ref "x")) (Let "z" (Ref "y") (Let "x" (Valor 4) (Ref "z"))))

let4 = Let "x" (Valor 3) (Ref "x")

let5 = Let "x" (Valor 4) (Soma (Ref "x")(Ref "x"))
-- Outermost
--avaliar (Let "x" (Valor 4) (Soma (Ref "x")(Ref "x"))) []
--avaliar (Aplicacao (Lambda "x" (Soma(Ref "x")(Ref "x"))) (Valor 4)) []
--    v = avaliacaoStrict (avaliar (Lambda "x" (Soma(Ref "x")(Ref "x"))) [])
--    v = FClosure "x" (Soma(Ref "x")(Ref "x")) []
--    e = EClosure (Valor 4) []
--avaliar (Soma(Ref "x")(Ref "x")) [("x", EClosure (Valor 4)[]) [] ]
--avaliarExpBin (Ref "x") (Ref "x") (+) [("x", EClosure (Valor 4)[]) [] ]
--   (VInt ve) = avaliar (Ref "x") [("x", EClosure (Valor 4)[]) [] ]
--             = pesquisar "x" [("x", EClosure (Valor 4)[]) [] ]
--             = avaliacaoStrict (avaliar (EClosure (Valor 4) []) [] )
--             = avaliacaoStrict (Valor 4)
--             = VInt 4
--          ve = 4
--   (VInt vd) = avaliar (Ref "x") [("x", EClosure (Valor 4)[]) [] ]
--             = pesquisar "x" [("x", EClosure (Valor 4)[]) [] ]
--             = avaliacaoStrict (avaliar (EClosure (Valor 4) []) [] )
--             = avaliacaoStrict (Valor 4)
--             = VInt 4
--          vd = 4
--VInt ((+) 4 4)
--VInt 8

teste1 = TestCase (assertEqual "avaliar 5" (VInt 5) (avaliar v5 []))

teste2 = TestCase (assertEqual "avaliar let x = 5 in (\\y -> x + y) 3" (VInt 8) (avaliar aplicacao []))
--myTests
teste3 = TestCase (assertEqual "avaliar let x = 3 in x+1" (VInt 4) (avaliar let2 []))

teste4 = TestCase (assertEqual "avaliar let x = 4+5 in let y = x+x in let z = y in let x = 4 in z" (VInt 18) (avaliar let3 []))

teste5 = TestCase (assertEqual "avaliar let x= 3 in x" (VInt 3) (avaliar let4 []))

teste6 = TestCase (assertEqual "avaliar let x=4 in x+x" (VInt 8) (avaliar let5 []))

todosOsTestes = TestList [ teste1
                         , teste2
                         , teste3
                         , teste4
                         , teste5
                         , teste6
                         ]

executarTestes = runTestTT todosOsTestes
