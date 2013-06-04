module Main where
    
import OBDD
import OBDD.Data
import OBDD.Operation

import Prelude hiding ( (&&) )

manual = make $ do
    fls <- checked_register $ Leaf False
    tru <- checked_register $ Leaf True
    b1  <- checked_register $ Branch 'b' fls tru
    checked_register $ Branch 'a' fls b1

constructed = (unit 'a' True) && (unit 'b' True)

ok = manual && manual
oops = constructed && manual

main = do
    writeFile "ok.dot" $ toDot ok
    writeFile "oops.dot" $ toDot oops

    