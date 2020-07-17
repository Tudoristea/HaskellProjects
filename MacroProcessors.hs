module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp s [] = []
lookUp s ((s', nr) : rest)
  |s' == s   = nr : lookUp s rest
  |otherwise = lookUp s rest


splitText :: [Char] -> String -> (String, [String])
splitText sep [] = ([], [""])
splitText sep (x : xs) =
  let (ts, (hws : tws)) = splitText sep xs 
  in if elem x sep then (x : ts, [] : (hws : tws))
     else (ts, (x : hws) : tws)
  

combine :: String -> [String] -> [String]
combine (sep : seps) (word : words)
              = [word] ++ [sep] : combine seps words -- ':' has a higher precenece than '++', this is why world, which is a list (list of chars) needs to be put in a list
combine xs [] = [xs]
combine [] xs = xs


getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs []
  = []
getKeywordDefs (ident : idents)
  = (word , concat (combine xs words)) : getKeywordDefs idents
    where
      (x : xs, word : words) = splitText " " ident



       

expand :: FileContents -> FileContents -> FileContents
expand textStr textKey
 = concat (combine' (c:cs) (str:strs))
    where
      (c:cs, str:strs) = splitText separators textStr
      combine' (c:cs) (str:strs) 
          = combine (c:cs) (expandHelp (str:strs))
      expandHelp :: [String] -> [String]
      expandHelp [] = []                
      expandHelp (str':strs')
        | n /= []   = head n : expandHelp strs'
        | otherwise = str' : (expandHelp strs')
        where 
          n = lookUp str' (getKeywordDefs (snd (splitText "\n" textKey) ) )

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
