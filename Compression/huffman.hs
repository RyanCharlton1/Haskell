import Data.Char (intToDigit, chr, ord)
import Control.Concurrent (rtsSupportsBoundThreads)

removeChar :: Char -> [Char] -> [Char]
removeChar c = filter (/= c)

getChars :: Char -> [Char] -> [Char]
getChars c = filter (== c)

splitStr :: [Char] -> [[Char]]
splitStr [] = []
splitStr str = getChars (head str ) str : splitStr (removeChar (head str) str)

data Tree = Branch String Int Tree Tree
          | Leaf String Int
          deriving (Show)

printTree :: Tree -> String
printTree (Leaf s _) = show s
printTree (Branch s l r _) = show l ++ "<-" ++ show s ++ "->" ++ show r

createLeaves :: [String] -> [Tree]
createLeaves xs = [Leaf [head x] (length x)| x <- xs]

treeWeight :: Tree -> Int
treeWeight (Leaf _ x) = x
treeWeight (Branch _ x _ _) = x

treeStr :: Tree -> String
treeStr (Leaf x _) = x
treeStr (Branch x _ _ _) = x

compareTrees :: (Int -> Int -> Bool) -> Tree -> Tree -> Bool
compareTrees f x y = treeWeight x `f` treeWeight y

treeOrder :: Tree -> Int 
treeOrder (Leaf _ _) = 1
treeOrder (Branch _ _ l r) = 1 + treeOrder l + treeOrder r

--treeToHeap :: Tree -> [String]
--treeToHeap tr = 
--            where arr = 

quicksort :: [a] -> ((Int -> Int -> Bool) -> a -> a -> Bool) -> [a]
quicksort [] _ = []
quicksort [x] _ = [x]
quicksort (x:xs) f = quicksort less f ++ [x] ++ quicksort more f
                   where less = [y | y <- xs, f (>) x y]
                         more = [y | y <- xs, f (<=) x y]

createExpressionTree :: [Tree] -> Tree
createExpressionTree [] = Leaf "" 0
createExpressionTree [x] = x
createExpressionTree (x:y:xs) = createExpressionTree (quicksort (Branch newStr newWeight x y : xs) compareTrees)
                                where newStr = treeStr x ++ treeStr y
                                      newWeight = treeWeight x + treeWeight y

createLookupTable :: Tree -> String -> [(String, String)]
createLookupTable (Leaf x _) str = [(x, str)]
createLookupTable (Branch _ _ l r) str = createLookupTable l (str ++ "0") ++ createLookupTable r (str ++ "1")

searchTable :: String -> [(String, String)] -> String
searchTable x ((y, z): ys) = if x == y then z else searchTable x ys

encodeString :: String -> String 
encodeString str =  concat [searchTable [x] table | x <- str]
                  where table = createLookupTable tree ""
                        tree = createExpressionTree (quicksort leaves compareTrees)
                        leaves = createLeaves (splitStr str)

padbString :: String -> String 
padbString str = str ++ concat (replicate num "0")
                where num = length str `mod` 8

bstringInt :: String -> Int 
bstringInt str = sum (zipWith (\x y -> (ord y - 48) * 2 ^ x) [0..] (reverse str))

binaryToChars :: String -> String
binaryToChars [] = []
binaryToChars str = chr (bstringInt (take 8 str)) : binaryToChars (drop 8 str)

huffman :: String -> String 
huffman  = binaryToChars . padbString . encodeString 

hman :: String -> IO()
hman path = do 
            str <- readFile path
            writeFile (path ++ "comp") (huffman str)