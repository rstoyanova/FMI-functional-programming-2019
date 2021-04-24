
replicate' n times = if times == 0 then [] else n : (replicate' n (times - 1))

sumDer x = sum [1| i<-[1,2..x], mod x i == 0]

isPrime:: Integer -> Bool
isPrime 1 = False
isPrime x = null [i| i<-[2..x], mod x i == 0]

prime :: Integer -> Bool
prime 1 = False
prime n = null [i| i<-[2..sqn], n `mod` i == 0]
  -- заради строгата типова система използваме два помощни
  -- функции за кастване от/към число с плаваща запетая
  where sqn = floor . sqrt . fromIntegral $ n

descartes x y = [(i,j)| i<-x, j<-y]

primes = [x| x<-[1..], prime x]

primesA = filter prime [1..]

pairs = [(i,sum-i)| sum<-[0..], i<-[0..sum]]

{-
compress [] = []
compess list = (head list,cntx) : compress (drop cntx list)
            where cntx = countMyHead list

-}

countH [] = 0
countH [x] = 1
countH (x:xs) = if (x == (head xs)) then 1 + countH xs else 1

countMyHead :: Eq a => [a] -> Int
countMyHead lst = length (takeWhile (== head lst) lst)

{-
-- a.k.a. run-length encoding
compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compress lst = (head lst, count) : compress (drop count lst)
  where count = countH lst
-}

compress :: Eq a => [a] -> [(a,Int)]
compress [] = []
compess list = (head list,cntx) : compress (drop cntx list)
            where cntx = countMyHead list

{-
maxRepeatedH [] res = res
maxRepeatedH lst res = if curr > res 
                            then maxRepeatedH (drop curr lst) curr 
                            else maxRepeatedH (drop curr lst) res
                        where curr = countH lst

maxRepeated lst = maxRepeatedH lst 0
-}

maxRepeated lst = maximum . (map snd) . compress
