--Aluno: André Gustavo da Rosa Ribeiro

--1 - Escreva uma função chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada
soma1 :: Int -> Int
soma1 x = x+1

--2 Escreva uma função chamada sempre que, não importando o valor de entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo

sempre :: a -> Int
sempre x = 0

--3 Escreva uma função chamada treco que receba três valores em ponto flutuantes com precisão dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro
treco :: Double -> Double -> Double -> Double
treco x y z = (x+y) * z

--4 Escreva uma função chamada resto que devolva o resto de uma divisão entre dois números inteiros.
resto:: Int -> Int -> Int
resto x y = rem x y

--5 Escreva uma função chamada precoMaior que devolva o maior valor entre quatro valores monetários.
precoMaior :: Double -> Double -> Double -> Double ->Double
precoMaior x y z k
 | x>=y  &&  x>=z  &&  x>=k = x
 | y>=x  &&  y>=z  &&  y>=k = y
 | z>=y  &&  z>=x  &&  z>=k = z
 | k>=y  &&  k>=z  &&  k>=x = k

--6 Escreva uma função chamada impar que devolva True, sempre que o resultado do produto de dois números inteiros for ímpar.
impar :: Int -> Int -> Bool
impar x y = if x*y `rem` 2==0 then False else True 

-- 7 Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: 𝑝𝑎𝑟 ∷ (𝐼𝑛𝑡, 𝐼𝑛𝑡). Escreva uma função em Haskell que devolva a soma dos componentes de um par de inteiros.
somaInteiros :: (Int,Int) -> Int
somaInteiros (x,y) = x+y

--8 Escreva uma função em Haskell que receba números reais (double) e devolva o resultado da equação 𝑥^2 + 𝑦/2 + 𝑧.

eq :: Double -> Double -> Double -> Double
eq x y z = (x**2) + y/2 + z

--9 Escreva uma função em Haskell chamada diagnostico que receba o peso do aluno e imprima um diagnóstico de obesidade, segundo a tabela que pode ser encontrada no link: Sobrepeso, obesidade e obesidade mórbida: entenda a diferença entre os três termos (cuidadospelavida.com.br). Observe que este diagnóstico é meramente estatístico e não tem nenhum valor real, está sendo usado nesta questão apenas para a definição das faixas. Todo e qualquer diagnóstico deve ser feito por um profissional médico
diagnostico:: Float -> Float -> String
diagnostico peso altura
 | (peso/(altura*altura))<17.0 = "Muito abaixo do peso"
 | (peso/(altura*altura))>=17.0 && (peso/(altura*altura))<18.5 = "Abaixo do peso"
 | (peso/(altura*altura))>=25.0 && (peso/(altura*altura))<30 = "Sobrepeso"
 | (peso/(altura*altura))>=30.0 && (peso/(altura*altura))<35.0 = "Obesidade leve"
 | (peso/(altura*altura))>=35.0 && (peso/(altura*altura))<40.0 = "Obesidade severa"
 | (peso/(altura*altura))>40.0 = "Obesidade Morbida"

--10 Escreva uma função em Haskell chamada bissexto que receba um ano e devolva True se o ano for bisexto sabendo que anos bissextos obedecem a seguinte regra: 𝑇𝑜𝑑𝑜𝑠 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠𝑒𝑗𝑎𝑚 𝑑𝑖𝑣𝑖𝑠í𝑣𝑒𝑖𝑠 𝑝𝑜𝑟 4 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 100 𝐸𝑥𝑐𝑒𝑡𝑜 𝑜𝑠 𝑎𝑛𝑜𝑠 𝑞𝑢𝑒 𝑠ã𝑜 𝑚ú𝑙𝑡𝑖𝑝𝑙𝑜𝑠 𝑑𝑒 400 1997 não é bissexto, 1900 não é bissexto e 2000 é bissexto
bissexto:: Int->Bool
bissexto x = if (x `rem` 4==0 && x `rem` 100/=0) || (x `rem` 4==0 && x `rem` 100==0 && x `rem` 400==0) then True else False

main = do
--Q1
  putStrLn $ "Func.1: entrada:2 resultado:" ++ show (soma1 2)
  putStrLn ""
--Q2
  putStrLn $ "Func.2: entrada:12 resultado:" ++ show (sempre 12)
  putStrLn $ "Func.2: entrada:aaa resultado:" ++ show (sempre "aaa")
  putStrLn ""

--Q3
  putStrLn $ "Func.3: entrada:1.0 ; 2.0 ; 3.6 resultado:" ++ show (treco 1.0 2.0 3.6)
  putStrLn ""

--Q4
  putStrLn $ "Func.4: entrada:14 ; 2 resultado:" ++ show (resto 14 2)
  putStrLn ""

--Q5
  putStrLn $ "Func.5: entrada:1.0 ; 2.0 ; 3.0 ; 4.0 resultado:" ++ show (precoMaior 1.0 2.0 3.0 4.0)
  putStrLn ""

--Q6
  putStrLn $ "Func.6: entrada:1 ; 2 resultado:" ++ show (impar 1 2)
  putStrLn $ "Func.6: entrada:1 ; 3 resultado:" ++ show (impar 1 3)
  putStrLn ""

--Q7
  putStrLn $ "Func.7: entrada:(2,2) resultado:" ++ show (somaInteiros (2,2))
  putStrLn ""

--Q8
  putStrLn $ "Func.8: entrada:2.0 ; 3.0 ; 4.0 resultado:" ++ show (eq 2.0 3.0 4.0)
  putStrLn ""
--Q9
  putStrLn $ "Func.9: entrada: 90.0 ; 1.80 resultado:" ++ show (diagnostico 90.0 1.80 )
  putStrLn $ "Func.9: entrada: 44.0 ; 1.55 resultado:" ++ show (diagnostico 44.0 1.55)
  putStrLn $ "Func.9: entrada: 40.0 ; 1.75 resultado:" ++ show (diagnostico 40.0 1.75)
  putStrLn ""
--Q10
  putStrLn $ "Func.10: entrada:1900 resultado:" ++ show (bissexto 1900)
  putStrLn $ "Func.10: entrada:2000 resultado:" ++ show (bissexto 2000)
