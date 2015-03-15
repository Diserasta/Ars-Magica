module Money
( p2lb
, lb2p
, simplifyLbP
, Money
, simplifyNomisma
, nm2fl
, nm2mlr
, fl2nm
) where
--I really shouldn't be doing so much work with Pennies...
data Money  = LbP  {lb :: Int
                ,pn :: Int}
            | Nomisma {nm :: Int
                ,mlr :: Int
                ,fl :: Int}
            | Dinar {dn :: Int
                ,hadn :: Int
                ,thrdn :: Int} deriving (Show, Ord, Eq)

get1st :: (a, b, c) -> a
get1st (x,_, _) = x

get2nd :: (a, b, c) -> b
get2nd (_,x,_) = x

get3rd :: (a, b, c) -> c
get3rd (_,_,x) = x

lbn :: Money -> Int
lbn (LbP x _) = x

pnn :: Money -> Int
pnn (LbP _ x) = x

nmn :: Money -> Int
nmn (Nomisma x _ _) = x

mlrn :: Money -> Int
mlrn (Nomisma _ x _) = x

fln :: Money -> Int
fln (Nomisma _ _ x) = x

dnn :: Money -> Int
dnn (Dinar x _ _) = x

hdnn :: Money -> Int
hdnn (Dinar _ x _) = x

thdnn :: Money -> Int
thdnn (Dinar _ _ x) = x
--pounds to pennies--
lb2p :: Int -> Int
lb2p pounds = pounds * (240)
--pennies to pounds--
p2lb :: Int -> (Int, Int)
p2lb pence = (div pence 240, mod pence 240)

simplifyLbP :: Money -> Money
simplifyLbP (LbP a b) =
  (LbP {lb = a + (fst pn), pn = (snd pn)}) where pn = p2lb b

--nomisma to folles--
nm2fl :: Int -> Int
nm2fl nomismae = nomismae * 288
--nomisma to miliaresia--
nm2mlr :: Int -> Int
nm2mlr nomismae = nomismae * 12

fl2mlr :: Int -> Int
fl2mlr fl = (div fl 24)

mlr2fl :: Int -> Int
mlr2fl mlr = mlr * 24

mlr2nm :: Int -> (Int,Int)
mlr2nm mlr = (div mlr 12, mod mlr 12)

fl2nm :: (Int, Int) -> (Int, Int, Int)
fl2nm (mlr, fl)
    | fl <= 0 && mlr <=0 = (0,0,0)
    | (fl < 288 && mlr <12) = (0, mlr + (div fl 24), mod fl 24)
    | (fl >= 288 || mlr >=12) = ((fst ml) + div fl 288, (snd ml) + div (mod fl 288) 24, mod (mod fl 288) 24)
    | otherwise = (0,0,0)
    where ml = mlr2nm mlr

simplifyNomisma :: Money -> Money
simplifyNomisma (Nomisma a b c) = (Nomisma {nm = a + (get1st fmr),mlr = (get2nd fmr),fl = (get3rd fmr)}) where fmr = fl2nm (b,c)
