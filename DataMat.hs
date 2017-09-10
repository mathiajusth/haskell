// in progress

data Matrices t = Scalar t
                | Matrix [Matrices t]
  deriving (Show, Eq)

----------------------------------------------------------
s :: t -> Matrices t
s t = Scalar t

m :: [Matrices t] -> Matrices t
m xs | isMatrix (Matrix xs) = Matrix xs                     -- | xs == []             = error "Empty matrix is not allowed"
     | otherwise            = error "This is not a matrix"
----------------------------------------------------------

-- juxtapose [x1,x2,x3,x4,..] ~> [(x1,x2),(x2,x3),(x3,x4),..]
juxtapose ::  [a] -> [(a,a)]
juxtapose xs = zip xs (tail xs)

-- Let op be binary Bool-valued operation
--   checkEq op [x1,x2,x3,..] ~> True  | iff for all i!=j: op (xi,xj) = True
-- Notice: If op represents equality relation ~ ( given by: a ~ b iff op (a,b) = True) )
--         then checkEq op xs = True 
--                iff 
--              all elements of xs are in the same equivalence class
checkEq :: (a -> a -> Bool) -> [a] -> Bool
checkEq op xs = (and.map (uncurry op).juxtapose) xs

-- equality relation on Matrices given by this operation is:
-- x ~ y iff x,y are both n-dimensional matrices
structureEq :: Matrices t -> Matrices t -> Bool
structureEq (Scalar x) (Scalar y) = True
structureEq (Matrix xs) (Matrix ys) = length xs == length ys && checkEq structureEq (xs ++ ys)
structureEq _ _ = False

isMatrix :: Matrices t -> Bool
isMatrix (Scalar t) = True
isMatrix (Matrix xs) = checkEq structureEq xs

dim :: Matrices t -> Int
dim (Scalar x)  = 0
dim (Matrix []) = 1
dim (Matrix (x:xs)) = 1 + dim x

signature :: Matrices t -> [Int]
signature matrix = 
  if (not.isMatrix) matrix then error "Incorrect matrix" 
  else matSignature matrix
    where matSignature (Scalar x) = []
          matSignature (Matrix xs) = length xs : signature (head xs)

ijkl :: Matrices t -> [Int] -> Matrices t
ijkl matrix xs =
  if ( dim matrix /= length xs )
    then error "Wrong number of indexes"
    else if not (areInRange matrix xs)
         then error "Indexes out of range"
         else  findIjk matrix (zipWith (-) xs (repeat 1))
           where findIjk (Scalar y) [] = (Scalar y)
                 findIjk (Matrix ys) (x:xs) = findIjk (ys !! x) xs
                 areInRange matrix xs = and (zipWith (<=) xs (signature matrix))
                                     && and (zipWith (>=) xs (repeat 1))

subMat :: Matrices t -> [Int] -> Matrices t
subMat matrix xs = 
  findIjk matrix (zipWith (-) xs (repeat 1))
    where findIjk (Matrix ys)  (x:xs) = findIjk (ys !! x) xs
          findIjk (Scalar y) _ = (Scalar y)
          findIjk x []     = x

addNumMats :: Num t => Matrices t -> Matrices t -> Matrices t
addNumMats x y = 
  if signature x /= signature y then error "Non-matching signatures"
  else addMatchingSignatureMats x y
    where addMatchingSignatureMats (Scalar a) (Scalar b) = Scalar (a + b)
          addMatchingSignatureMats (Matrix xs) (Matrix ys) = Matrix (zipWith addMatchingSignatureMats xs ys)

trace :: Num t => [t] -> t
trace [x] = x
trace (x:xs) = x + trace xs

--zipMat :: (Matrices t -> Matrices t -> Matrices t) -> Matrices t -> Matrices t ->

--matTrace :: Num t => Matrices t -> t
--matTrace (Matrix (x:xs)) =  

mulNumMats :: Num t => Matrices t -> Matrices t -> Matrices t
mulNumMats x y = 
  if signature x /= signature y then error "Non-matching signatures"
  else mulMatchingSignatureMats x y
    where mulMatchingSignatureMats (Scalar a) (Scalar b) = Scalar (a + b)
          --mulMatchingSignatureMats (Matrix xs) (Matrix ys) = Scalar (trace (zipWith mulMatchingSignatureMats xs ys))

a = m[ s 1, s 2 ]
b = m[ s 5, s 6 ]
c = Matrix[ Matrix [Scalar 1, Scalar 2] , Scalar 3 ]

e = m[
        m[
            s 1, s 2
        ],
        m[
            s 3, s 4
        ]
    ]

f = m[
        m[
            s 11, s 12
        ],
        m[
            s 13, s 14
        ]
    ]

mat4d =  m[ 
            m[
              m[
                m[ s (1,1,1,1) , s (1,1,1,2) ], 
                m[ s (1,1,2,1) , s (1,1,2,2) ]
              ], 
              m[
                m[ s (1,2,1,1) , s (1,2,1,2) ],
                m[ s (1,2,2,1) , s (1,2,2,2) ]
              ]
            ],
            m[
              m[
                m[ s (2,1,1,1) , s (2,1,1,2) ], 
                m[ s (2,1,2,1) , s (2,1,2,2) ]
              ], 
              m[
                m[ s (2,2,1,1) , s (2,2,1,2) ],
                m[ s (2,2,2,1) , s (2,2,2,2) ]
              ]
            ]
          ]
