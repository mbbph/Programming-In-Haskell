--1
putStr xs = sequence_ [putChar x | x <- xs]

--2
putBoard = putBoard' 1

putBoard' r [] = return ()
putBoard' r (n:ns) = do putRow r n
                        putBoard' (r+1) ns

--3
putBoard b = sequence_ [putRow r n | (r,n) <- zip [1..] b]

--4
adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getLine                      --get number of ints
           let n' = (read n) :: Int          --not sure if this is correct
           total <- sumnum n' 0
           putStrLn ("The total is " ++ (show total))

--keeps track of ints left to read and outputs total
sumnum :: Int -> IO ()
sumnum 0 = return 0
sumnum n = do x <- getLine
              let x' = (read x) :: Int       --not sure if this is correct
              return $ x' + sumnum (n - 1)

--5
adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getLine                              --get num in string form
           let n' = (read n) :: Int                  --convert string to int
           nums <- sequence [getLine | _ <- [1..n']] --getLine n times
           let nums' = map read nums :: [Int]        --convert all string elements into ints
           putStrLn ("The total is " ++ (show (sum nums')))

--6
readLine :: IO String
readLine = do x <- getChar
              if x == '\n' then
                return []
              else
                if x == '\DEL' then
                  do xs <- readLine
                     return ('\b':xs)
                else
                  do xs <- readLine
                     return (x:xs)
  
