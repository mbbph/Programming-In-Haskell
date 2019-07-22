--1
comment = do string "--"
             many (sat (/= '\n'))
             return ()

--4
--Each number would be parsed multiple times

--5 (Only modified parser code shown)
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr
            deriving Show

expr' :: Parser Expr
expr' = do
  t <- term
  do symbol "+"
     e <- expr'
     return (Add (Val t) e)
   <|> return (Val t)


--6 (Only modified parser code shown)
expr :: Parser Int
expr = do t <- term
          do symbol "+"
             e <- expr
             return (t + e)
           <|> do symbol "-"
                  e <- expr
                  return (t - e)
           <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> do symbol "/"
                  t <- term
                  return (f 'div' t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> return integer

--7 (Only new code shown)
expon :: Parser Int --expon parser comes after term and before factor
expon = do f <- factor
              do symbol "^"
                 e <- expon
                 return (f ^ e)
                <|> return f

--8
--a. expr ::= expr - expr | nat
--   nat := ... | 0 | 1 | 2 | ...
--b.
item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

natural :: Parser Int
natural = token nat

expr :: Parser Int
expr = do e1 <- factor
          do symbol "-"
             e2 <- expr
             return (e1 - e2)
           <|> return e1

factor :: Parser Int
factor = do e <- expr
            return e
          <|> natural

--c. Not sure
--d. Not sure because of c

--9.

calc :: String -> IO ()
calc xs = do display xs
            c <- getCh
            if elem c buttons then
                process c xs
            else
                do beep
                   calc xs

eval :: String -> IO ()
eval xs = case parse expr xs of
             [(n, [])] -> calc (show n)
             [(_, nx)]  -> do errorpos nx
                              calc xs

beep :: IO ()
beep = putStr "\BEL"

errorpos :: String -> IO ()
errorpos s = putStr $ "Error at " ++ s
