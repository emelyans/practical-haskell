{-# LANGUAGE ViewPatterns #-}

module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

{--(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1
                then lst2
                else head lst1 : tail lst1 +++ lst2--}
{--(+++) :: [a] -> [a] -> [a]
list1 +++ list2 = case list1 of
                    []    -> list2
                    x:xs  -> x : (xs +++ list2)--}
(+++) :: [a] -> [a] -> [a]
[]     +++ list2  = list2
(x:xs) +++ list2  = x : (xs +++ list2)

reverse2 :: [a] -> [a]
reverse2 lst =  if null lst
                then []
                else reverse2 (tail lst) +++ [head lst]

{--maxmin :: Ord a => [a] -> (a,a)
maxmin list = if null (tail list)
              then (head list, head list)
              else ( if head list > fst (maxmin (tail list))
                     then head list
                     else fst (maxmin (tail list))
                   , if head list < snd (maxmin (tail list))
                     then head list
                     else snd (maxmin (tail list))
                   )--}

{--maxmin :: Ord a => [a] -> (a,a)
maxmin list = let h = head list
              in if null (tail list)
                 then (head list, head list)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min)
                      where t = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t--}

{--maxmin :: Ord a => [a] -> (a,a)
maxmin list = let h = head list
              in if null (tail list)
                 then (head list, head list)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min)
                      where t = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t--}

maxmin :: Ord a => [a] -> (a,a)
maxmin [x]    = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max,xs_min) = maxmin xs

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
              deriving Show

data Gender = Male | Female | Unknown
              deriving Show

data TimeMachine = TimeMachine String Integer String TimeTravelDirection Float

data TimeTravelDirection = Past | Future | Both

{--clientName :: Client -> String
clientName client = case client of
                      GovOrg name                 -> name
                      Company name id person resp -> name
                      Individual person ads       ->
                        case person of
                          Person fNm lNm gender   -> fNm ++ " " ++ lNm--}

{--clientName :: Client -> String
clientName client = case client of
                      GovOrg name                     -> name
                      Company name _ _ _              -> name
                      Individual (Person fNm lNm _) _ -> fNm ++ " " ++ lNm--}

clientName :: Client -> String
clientName (GovOrg name)                      = name
clientName (Company name _ _ _)               = name
clientName (Individual (Person fNm lNm _) _)  = fNm ++ " " ++ lNm

companyName :: Client -> Maybe String
companyName client = case client of
                      Company name _ _ _ -> Just name
                      _                  -> Nothing

responsibility :: Client -> String
responsibility (Company _ _ _ r)  = r
responsibility _                  = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName     -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")      = True
specialClient _                                   = False

{--fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n - 2) + fibonacci (n - 1)--}

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

{--sorted :: Ord a => [a] -> Bool
sorted []        = True
sorted [_]       = True
sorted (x:y:zs)  = x < y && sorted (y:zs)--}

sorted :: Ord a => [a] -> Bool
sorted []        = True
sorted [_]       = True
sorted (x:r@(y:_))  = x < y && sorted r

