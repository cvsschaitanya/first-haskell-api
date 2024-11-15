

fib :: Float -> Float -> Float
fib a b =  a + b

-- FUNCTION
hyp2 a b = a2 + b2
    where 
        a2 = a*a
        b2 = b*b

-- GUARD
fac n
    | n <= 1    = 1
    | otherwise = n * fac (n-1)

-- accumulator
sumN n = aux n 0
    where 
        aux n acc
            | n <= 0    = acc
            | otherwise = aux (n-1) (n + acc)

oneToN n = [1..n]

oneToN_ n 
    | n<1  =   []
    | n==1  =   [1]
    | otherwise =  n:oneToN_ (n-1)

isina :: Eq a => a -> [a] -> Bool
isina a al  
    | null al     = False 
    | head al == a       = True
    | otherwise         = isina a (tail al)

-- abc = isina (1 [1,2])


sett :: (Eq a) => [a] -> [a]
sett [] = []
sett (x:xs)
    | x `elem` xs = sett xs
    | otherwise  = x : sett xs
    

isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = x <= y && isAsc (y:xs)


hasPath :: [(Int ,Int)] -> Int-> Int -> Bool
hasPath g a b 
    | a == b    = True
    | otherwise = or [ hasPath g y b | (x,y)<-g, x==a ]

main = do
    print (hyp2 12 5)
    print (fac 5)
