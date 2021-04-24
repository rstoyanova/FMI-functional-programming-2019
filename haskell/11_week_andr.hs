
prod [] = 1
prod (x:xs) = x * prod xs

sumProducts xs = sum [prod x| x<-xs]

--countEl list el 
countEl [] _ = 0
countEl (x:xs) y = if x == y then 1 + countEl xs x else countEl xs y

occurrences [] _ = []
occurrences elms lst = (countEl lst (head elms)) : occurrences (tail elms) lst

allEq [] = True
allEq [x] = True
allEq (x:y:xs) = if (x == y) then allEq (y:xs) else False

matchLengths lst = allEq [length x| x<-lst]

--setUnion [] lst = lst
--setUnion (x:xs) lst = x : setUnion xs lst

setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion [] lst2 = lst2
setUnion lst1 [] = lst1
setUnion lst1@(x:xs) lst2@(y:ys)
  | x == y    = x : (setUnion xs ys)
  | x < y     = x : (setUnion xs lst2)
  | otherwise = y : (setUnion lst1 ys)


--contains set x 
contains [] _ = False
contains (x:xs) el = if x == el then True else contains xs el

setIntersect set1 set2 = [x| x<-set1, contains set2 x]

setDiff set1 set2 = [x| x<-set1, not(contains set2 x)]


--cross-out m

withoutRH m@(x:xs) i n
    | n == i = withoutRH m i (n + 1)
    | n > length(m) = []
    | otherwise = x : (withoutRH xs i (n + 1))



    
    {-
withoutR [] _ = []
withoutR (x:xs) i = if i == 0 then withoutR xs -1 else x : (withoutR xs (i - 1))
-}


