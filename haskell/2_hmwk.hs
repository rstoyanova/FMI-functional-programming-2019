--
-- 00
--
group :: Eq a => [a] -> [[a]]
group [] = [[]]
group [x] = [[x]]
group (x:xs) 
    | x == h = (x:hh) : t
    | otherwise = [x] : rest
   where rest@((hh@(h:_)) : t) = group xs


--
-- 01
--
sortBy _ [] = []
sortBy comp (x:xs) = let
        lhs = [i| i<-xs, comp i x /= GT]
        rhs = [i| i<-xs, comp i x == GT]
        in (sortBy comp lhs) ++ [x] ++ (sortBy comp rhs)


-- 
-- 02
--
on f g x y = f (g x) (g y)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy _ [x] = [[x]]
groupBy pred (x:xs)
    | pred x h = (x:hh) :t
    | otherwise = [x] : rest
    where rest@((hh@(h:_)) : t) = groupBy pred xs


--
-- 04
--
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn func = map m . sortBy (\(a,b) (c, d) -> compare a c) . map (\x -> ((func x), x)) 
  

--
-- 05
--
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn func = map m . groupBy (\(a,b) (c, d) -> a == c) . map (\x -> ((func x), x)) 


--
-- 06
--
classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn func = groupOn func . sortOn func


