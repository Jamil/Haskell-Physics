-- STAGE 1

-- Define a data type for three-dimensional vectors

data Vec = Vec { xComp :: Double,
                 yComp :: Double,
                 zComp :: Double } deriving (Show)

-- Vector addition
(^+^) :: Vec -> Vec -> Vec
(^+^) a b = Vec ((xComp a) + (xComp b)) ((yComp a) + (yComp b)) ((zComp a) + (zComp b))

-- Vector addition
(^-^) :: Vec -> Vec -> Vec
(^-^) a b = Vec ((xComp a) - (xComp b)) ((yComp a) - (yComp b)) ((zComp a) - (zComp b))

-- Scalar multiplication
(*^) :: Double -> Vec -> Vec
(*^) a x = Vec (a * (xComp x)) (a * (yComp x)) (a * (zComp x))

-- Scalar multiplication (II)
(^*) :: Vec -> Double -> Vec
(^*) x a = Vec (a * (xComp x)) (a * (yComp x)) (a * (zComp x))

-- Scalar division
(^/) :: Vec -> Double -> Vec
(^/) x a = Vec ((xComp x) / a) ((yComp x) / a) ((zComp x) / a) 

-- Dot product
(<.>) :: Vec -> Vec -> Double
(<.>) x y = ((xComp x) * (xComp y)) + ((yComp x) * (yComp y)) + ((zComp x) * (zComp y))

-- Cross product
(><) :: Vec -> Vec -> Double
(><) a b = ((yComp a) * (zComp b) - (zComp a) * (yComp b)) - 
           ((xComp a) * (zComp b) - (zComp a) * (xComp b)) +
           ((xComp a) * (yComp b) - (yComp a) * (xComp b))

magnitude :: Vec -> Double
magnitude a = sqrt ((xComp a) * (xComp a) + 
                    (yComp a) * (yComp a) + 
                    (zComp a) * (zComp a))

zeroV :: Vec
zeroV = Vec 0.0 0.0 0.0

iHat :: Vec
iHat = Vec 1.0 0.0 0.0

jHat :: Vec
jHat = Vec 0.0 1.0 0.0

kHat :: Vec
kHat = Vec 0.0 0.0 1.0

negateV :: Vec -> Vec
negateV a = (-1.0) *^ a

sumV :: [Vec] -> Vec
sumV as = foldr (^+^) zeroV as


