


--Примерни задачи за писмен изпит 2016/2017

-- 1
--
contains [] _ = False
contains (x:xs) el = if x == el then True else contains xs el

subset :: Eq t => [t] -> [t] -> Bool
subset [] _ = True
subset (x:xs) set = if contains set x then subset xs set else False

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

anySub :: Eq t => [t] -> [[t]] -> Bool
anySub _ [] = False
anySub set (x:xs) = if subset set x then True else anySub set xs

findColumns :: (Num a, Eq t) => [[t]] -> a
findColumns m = sum [1| x<-(transpose m), anySub x m]



-- 2
--
lUns :: [Integer -> Integer]
lUns = [(\x->(x + 1)), (\x->(x * 2)), (\x->(x*x)), (\x->(x*10))]

lBins :: [Integer -> Integer -> Integer]
lBins = [(\x y -> x+y), (\x y -> x * y)]

fhg f g h x = h((f x), (g x))

genAllUns :: Enum a => [a -> b] -> a -> a -> [[b]]
genAllUns uns a b = [(map f [a..b])| f<-uns]

genAllfhg :: Enum t1 => [t1 -> t2] -> [t2 -> t2 -> b] -> t1 -> t1 -> [[b]]
genAllfhg uns bins a b = [(map (\x -> h (f x) (g x)) [a..b])| f<-uns, h<-bins, g<-uns]

setIntersect set1 set2 = [x| x<-set1, contains set2 x]

haveInter set1 set2 = if (setIntersect set1 set2) /= [] then True else False 

check uns bins a b = if [1| haveInter (genAllUns uns a b) (genAllfhg uns bins a b)] == [] then False else True

-- 3
--

get_st (_,a,b) = [a,b]

get_name (a,_,_) = a

unpack [] = []
unpack (x:xs) = (head x) : (head (tail x)) : unpack xs

allTemps plants = unpack (map get_st plants)

makeAllInt lst = [[x,y]| x<-lst, y<-lst]

--setIntersect set1 set2 = [x| x<-set1, contains set2 x]

toRange [a,b] = [a..b]

plantInInt plant int = if (setIntersect (toRange (get_st plant)) (toRange int)) /= [] then True else False

getPlantsInInt plants int = ((head int, head (tail int)),[get_name x| x<-plants, plantInInt x int])

--garden plans


-- 4
--

--contains [] _ = False
--contains (x:xs) el = if x == el then True else contains xs el

maxByLen [x] = x
maxByLen (x:xs) = if length x > length (maxByLen xs) then x else maxByLen xs

findSucc g u = if head (head g) == u then tail (head g) else findSucc (tail g) u

--allPaths g u visited curr

-- maxPath 




-- Изпит по Функционално програмиране 2018
--

-- 1
--


natPairs :: [(Integer,Integer)]
natPairs = [ (x,sum-x) | sum<-[2..],
                         x<-[1..sum-1] ]

generateExponents k l = [(x^k)*((sum-x)^l)| sum<-[2..], x<-[1..sum-1]]

-- 3
--

f = [(\x->(x + 1)), (\x->(x * 2)), (\x->(x*x)), id, (\x->(x*10))]

vals = [[2,3], [100,12,3], [1,2,3], [30,32,21]]

mapAll _ [] = []
mapAll (x:xs) (y:ys) = (map x y) : mapAll xs ys

--setIntersect set1 set2 = [x| x<-set1, contains set2 x]

--contains [] _ = False
--contains (x:xs) el = if x == el then True else contains xs el

{-
intersectAll [[a]] = [a]
intersectAll (x:xs) = setIntersect x (intersectAll xs)
-}


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]


makeSet [] = []
makeSet [x] = [x]
makeSet (h:t)
        | h == (head t) = makeSet t
        | otherwise = h : (makeSet t)

interAllH (x:[]) = x
interAllH (h:t) = [x| z<-t, x<-h, contains z x, contains (interAllH t) x]

interAll lst = makeSet (quicksort (interAllH lst))

findIndx var (x:xs) = if var == x then 1 else (1 + findIndx var xs)

getByIndx indx list = if indx == 1 then (head list) else getByIndx (indx - 1) (tail list)

getLIndx [] [] = []
getLIndx (x:xs) (y:ys) = (getByIndx x y) : getLIndx xs ys


allEqual vars f = if res == [] then [] else getLIndx indxs vars
        where fx = mapAll f vars
              res@(h:(t@[])) = interAll fx
              indxs = map (findIndx h) fx



-- 4
--

linCombH [] [] _ = True
linCombH (x:xs) (y:ys) coef = if (x / y) == coef then linCombH xs ys coef else False

len [] = 0
len (x:xs) = 1 + (len xs)

linComb a@(x:xs) b@(y:ys) = if (len a) == (len b) then linCombH xs ys coef else False
                            where coef = x / y


a = [("A",[("p",6),("q",9)]),("B",[("p",2),("q",3)])]
fl = [("p",6),("q",9),("i", 30)]
sl = [("p",12),("q",18), ("i",60)]

getActIngr (_,a) = a

findIngr _ [] = -1
findIngr (a,b) ((x,y):t) = if x == a then y else findIngr (a,b) t

checkH a@(x:xs) b ingrA ingrB 
        | a == [] || b == [] = linComb ingrA ingrB
        | ingrmg == -1 = False
        | otherwise = checkH xs b ((getActIngr x): ingrA) (ingrmg : ingrB)
           where ingrmg = findIngr x b
        
checkAB a b = checkH a b [] []



-- Поправителен изпит 2019г

-- 1
--

lst1 = [[1,2,1,1,1,4,3], [1,2,3,3,1,4], [1],[1,2], [1,1,1,1,1]]
lst2 = [[1,2,2,2,2,1,3], [1]]

similar a b = if (aElem + bElem) == (length a) + (length b) then True else False
                where aElem = sum [1| x<-a, contains b x]
                      bElem = sum [1| x<-b, contains a x]



remove el (x:xs) = if el == x then xs else x : (remove el xs)

dissimToAll lst [x] = not (similar lst x)
dissimToAll lst (x:xs) = if not (similar lst x) then dissimToAll lst xs else False

dissimilar ll = [x| x<-ll, dissimToAll x (remove x ll)]


-- 2
--

genK k = [k*x| x<-[1..]]

npairsH k = [(x*k,(x + 1)*k)| x<-[1..]]

-- 3
--



dt = [("mat", "izgrev", 2), ("geogr", "zvezda", 7), ("mat", "icko", 4), ("ist", "bobkata", 3), ("bel", "izgrev", 4)]

get_class (_,_,a) = a

getAllNth n [] = []
getAllNth n (x:xs) = if get_class x == n then x : getAllNth n xs else getAllNth n xs

sameClass (_,_,a) (_,_,b) = if a == b then True else False

sameSubj (a,_,_) (b,_,_) = if a == b then True else False

hasSameSubjN el [] = False
hasSameSubjN el (x:t) = if sameSubj el x then True else hasSameSubjN el t

--findSameSubjN el (x:t) = if sameSubj el x then x else hasSameSubjN el t

{-
exchange n dt@(h:t) st
        | forN == [] = dt
        | (get_class h /= n) && (hasSameSubN h forN) = exchange n (newB):(remove h dt) (remove newB st)
        | otherwise = 
         where forN = getAllNth n st
               newB = findSameSubjN h forN
-}


gabi k = [x*k| x<-[1..]]


gabiN = [(x, (sum - x))| sum<-[2..], x<-[1..(sum -1)]]

--list!!3



