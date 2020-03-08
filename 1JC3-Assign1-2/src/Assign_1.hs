{- Assignment 1
 - Name: KANWAR SHEHERAZ SINGH SANDHU
 - Date: 27 September 2019. 
 -}
 
module Assign_1 where

macid :: String
macid = "Sandhk8"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: this function takes a , b , c and gives value of q .
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = 
             let x = (3*a*c - b^2)/(9*(a^2))
              in x

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: this function  takes a , b , c , d  and computes value of r.
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = 
              let y = (9*a*b*c - 27*(a^2)*d - 2*(b^3))/(54*(a)^3)
               in y

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: q^cube + r^square is the discriminant of the equation .
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q^3 + r^2

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: cubicS will take  input from result of  cubicS and cubicR
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cRoot(r + (q^3 + r^2)**(1/2)) 

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description:  cubicT will take  input from result of  cubicS and cubicR
 defining function cRoot to find cube roots of both negative and positive integers
 -}
cubicT :: Double -> Double -> Double
cubicT q r =  cRoot(r - (q^3 + r^2)**(1/2))


cRoot :: Double -> Double 
cRoot p = (signum p )  *  (abs(p))**(1/3)
  
{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: this function is where we give input(a ,b,c,d) , it will compute x1 , x2 ,x3 using functions
 (cubicS ,cubicT,cubicR,cubicQ),  and give output according to conditional expression which applies condition
 on cubicDisc.
 here arguments q and r are further passed to cubicQ and cubicR to give outputs that are further used as 
 inputs in cubicS and cubicT.
 -}

cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d  = 
 let 
     x1 = (cubicS q r) + (cubicT q r) - b/(3*a)
     x2 = -((cubicS q r)+ (cubicT q r))/2 - b/(3*a) + sqrt(3)*((cubicS q r) - (cubicT q r ))/2
     x3 = -((cubicS q r)+ (cubicT q r))/2 - b/(3*a) - sqrt(3)*((cubicS q r) - (cubicT q r ))/2

     in(
          if (cubicDisc q r) < 0 then []
          else if (cubicDisc q r) > 0 then [x1]
          else [x1,x2,x3]
     )





     where q = (cubicQ a b c)
           r = (cubicR a b c d)

 



{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}
-- testing1 = cubicRealSolutions 1 0 (-3) 0 = []
-- testing2 = cubicRealSolutions 1 0 1 0    = [0.0]
-- testing3 = cubicRealSolutions 1 2 3 4    = [-1.650]
