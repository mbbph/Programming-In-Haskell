--1
countNodes :: Tree Grid -> Int
countNodes (Node _ []) = 1
countNodes (Node _ [x]) = 1 + countNodes x
countNodes (Node _ x) = 1 + countNodes x + sum . map countNodes x
--I don't really understand the book's solution... In the case of (Node _ []),
--how does sum (map nodes []) return zero? How does the function even know to return zero?

maxDepth :: Tree Grid -> Int
maxDepth (Node _ []) = 0
maxDepth (Node _ x) = 1 + maximum . map maxDepth x

--countNodes (gametree empty X)
--maxDepth (gametree empty X)

--2 (only modified code shown)

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g', p') _ <- ts, p' == best]
               where
                 tree = prune depth (gametree g p)
                 Node (_, best) ts = minimax tree

play' :: Grid -> Player -> IO ()
play' g p
 | wins 0 g = putStrLn "Player 0 wins!\n"
 | wins X g = putStrLn "Player X wins!\n"
 | full g = putStrLn "It's a draw!\n"
 | p == 0 = do i <- getNat (prompt p)
               case move g i p of
                 [] -> do putStrLn "ERROR: invalid move"
                          play' g p
                 [g'] -> play g' (next p)
 | p == X = do putStr "Player X is thinking... "
               r <- randomRIO (0, length ((bestmove g p) - 1))
               (play $! ((bestmoves g p) !! r)) (next p)

--3
bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [g' | Node (g', p') _ <- ts, p' == best]
               where
                 tree = prune depth (gametree g p)
                 Node (_, best) ts = minimax tree

--New helper function
besttrees :: [Grid] -> Player -> [Tree Grid]
besttrees gs p = map (`gametree` p) gs
--Notes: when mapping a func with 2 arguments,
--map (`func` b) list) has the second input specified
-- map (a `func`) list) has the first  input specified

treedepths :: [Tree Grid] -> [Int]
treedepths xs = map maxDepth xs

--New Helper function
--returns the index of tree with minimum depth
depthindex :: [Int] -> Int -> Int
depthindex  [] _ = 0
depthindex  [x] _ = 0
depthindex  (x:xs) n = if x == n then 0 else 1 + mindepth xs n

play' :: Grid -> Player -> IO ()
play' g p
 | wins 0 g = putStrLn "Player 0 wins!\n"
 | wins X g = putStrLn "Player X wins!\n"
 | full g = putStrLn "It's a draw!\n"
 | p == 0 = do i <- getNat (prompt p)
               case move g i p of
                 [] -> do putStrLn "ERROR: invalid move"
                          play' g p
                 [g'] -> play g' (next p)
 | p == X = do putStr "Player X is thinking... "
               let bmvs = bestmoves g p --[Grid]
               let btrs = besttrees bmvs p --[Tree Grid]
               let depths = treedepths btrs --[Int]
               let smallestdepth = minimum depths --Int
               let i = depthindex depths smallestdepth --Int
               (play $! ((bestmove g p) !! i)) (next p)
--4
--To do later 
