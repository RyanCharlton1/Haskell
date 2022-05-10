chew :: String -> String
chew str = takeWhile ( == head str) str

nibble :: String -> String
nibble = take 9 . chew

runs :: String -> [String]
runs [] = []
runs str = ys : runs (drop (length ys) str)
            where ys = nibble str

encode :: String -> [(Char, Int)]
encode str = map (\x -> (head x, length x)) (runs str)

flatten :: [(Char, Int)] -> String
flatten = foldl (\z (x, y) -> z ++ [x] ++ show y) ""

compress :: String -> String 
compress = flatten . encode

decode :: [(Char, Int)] -> String 
decode = foldl (\z (x, y) -> z ++ replicate y x) ""

expand :: String -> [(Char, Int)]
expand [] = []
expand (x:y:xs) = (x, read [y]) : expand xs 

decompress :: String -> String
decompress = decode . expand
