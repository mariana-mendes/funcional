{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = a /= b
impl a b = not a || b
equiv a b = impl a b && impl b a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y = x * pow x (y-1)


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
--- Solução do professor.
isPrime 1 = True
isPrime 2 = True
isPrime 3 = True
isPrime x = isPrime' x [2..(x-1)]

{- Nesse caso vai ser da forma mais custosa, vai sair tentando dividir por cada elemento
- da lista até chegar no número passado como parametro
-}
isPrime' x [] = True
isPrime' x (head:tail) = if (mod x head) == 0 then False else isPrime' x tail

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)


----Solução do professor usando lista.

fib2 x = last (fib2' x)

fib2' 1 = [1]
fib2' 2 = [1,1]
fib2' n = previous ++ [(last (init previous)) + (last previous)]
	where
	previous = fib2' (n-1)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}

mdc 0 y = y
mdc x 0 = x
mdc x y 
	| x > y = mdc (x-y) y
	| otherwise = mdc x (y-x)



------- Solução do professor.


mdc2 0 y = y
mdc2 x 0 = x
mdc2 = x y = mdc2 y r
	where
	r = mod x y

{-
- Calcula um MMC de dois numeros. 
-}

---- Solução do professor. // tendi nd
mmc x y = head ys
	where ys = filter (divisivel x y) [(min x y)..x*y]
divisivel x y n = (mod n x == 0) && (mod n y == 0)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}

----- Solução do professor.

coprimo x y = if (mdc x y) == 1 then True else False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = undefined
