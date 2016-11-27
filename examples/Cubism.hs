import Prelude hiding ((&&),(||),not,and,or)
import OBDD
import OBDD.Cube

data V = L Int | R Int | Out Int
       deriving (Eq, Ord, Show)

form n =
  let make f = variable . f <$> [1..n]
      (rs,c) = add (make L) (make R)
  in  not c && and ( zipWith equiv rs $ make Out )

add xs ys =
  let go c [] [] = ([], c)
      go c (x:xs) (y:ys) =
        let (r,d) = fulladd c x y
            (rs, e) = go d xs ys
        in  (r : rs, e)
  in  go false xs ys
      
halfadd x y = (xor x y , x && y)

fulladd x y z =
  let (r , c)  = halfadd x y
      (s , d) = halfadd r z
  in  (s, c || d)

