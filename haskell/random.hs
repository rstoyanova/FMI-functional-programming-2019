
fact 0 = 1
fact n = fact (n -1) * n 

fact2 x = if (x == 0) then 1 else x*(fact2 (x-1))

fact3 x
    |x==0 = 1
    |x>0 = x*(fact3(x-1))
    |otherwise = error "Invalid input"

num9 = 9 :: Int
sqrt9 = sqrt(fromIntegral num9)

pwr2 x y = x ** y

times3 x = x*3
 
appl2 f x = f (f x)

samearg f x = f x x

positive x = (x > 0)

lastdigt x = (mod x 10)

temp x
    | x==1 = "icko"
    | x==2 = "renka"


sumDig x = if (x == 0) then 0 else lastdig x + sumDig(stripLast x)
            where   lastdig x = mod x 10
                    stripLast x = div x 10

saySign x
    |x>0 = "Positive"
    |x==0 = "Zero"
    |x<0 = "Negative"      
    
fibonachi x
    |(x==1 || x==2) = 1
    |otherwise = (fibonachi (x-1) + fibonachi (x-2))

mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

frst (a,_,_) = a

ackermann m n
    |m==0 = n+1
    |(m>0 && n==0) = ackermann (m-1) 1
    |(m>0 && n>0) = ackermann (m-1) (ackermann m (n-1))

complAdd (a,b) (c,d) = ((a+c), (b+d))

--dist:: (Num a) => (a,a) -> (a,a) -> a
dist (a,b) (c,d) = sqrt((a-c)**2 + (b-d)**2)

myreplicate 0 _ = []
myreplicate times n = n: myreplicate (times-1) n

mytake 0 _ = []
mytake n (x:xs) = x : mytake (n -1) xs

icko f g lst = [f (g x) | x <- lst]

descartes ls1 ls2 = [(a,b) | a<-ls1, b<-ls2]

myany lst p = or (map p lst)

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a

getAllSec list = mymap second list

--bestMovie (String name, Floating rate, Int dur)
--bestMovie _ [] = ""

bestMovie mins movies = filter (\x -> (third x < mins)) movies













