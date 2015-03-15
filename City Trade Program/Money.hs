module Money
( poundsToPence
, penceToPounds
, simplifyMoney
, LbP
) where
--I really shouldn't be doing so much work with Pennies...
data LbP = LbP  {lb :: Int
                ,pn :: Int} deriving (Show,Eq)

lbn :: LbP -> Int
lbn (LbP x _) = x

pnn :: LbP -> Int
pnn (LbP _ x) = x

poundsToPence :: Int -> Int
poundsToPence pounds = pounds * (240)

penceToPounds :: Int -> (Int, Int)
penceToPounds pence = (div pence 240, mod pence 240)

simplifyMoney :: LbP -> Int -> Int -> LbP
simplifyMoney z a b = z{lb = a + (div b 240), pn = mod b 240}

