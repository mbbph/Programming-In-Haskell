import Data.Maybe

--Source code
data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Add x y) = case eval x of
                  Just n -> case eval y of
                              Just m -> Just (n + m)
                              Nothing -> Nothing
                  Nothing -> Nothing
eval Throw = Nothing
eval (Catch x h) = case eval x of
                    Just n -> Just n
                    Nothing -> eval h
--------------------------------------------------------------------------------
--COMPILER VER B
--failure handling is built into comp'
--------------------------------------------------------------------------------
-- comp' :: Expr -> Code -> Code -> Code
-- type Stack = [Elem]
-- data Elem = VAL Int

-- exec (comp' x sc fc) s = case eval x of
--                            Just n -> exec sc (VAL n : s)
--                            Nothing -> exec fc s

-- Calculate the def for comp' using induction
--
-- Base case: Val n
-- exec (comp' (Val n) sc fc) s
-- = exec sc (VAL n : s)

--define exec (PUSH n c) s = ee c (VAL n : s)
-- = exec (PUSH n sc) s

-- Base case: Throw sc fc
-- exec (comp' Throw sc fc) s
-- = exec fc s

--Catch x h
-- exec (comp' (Catch x h) sc fc) s
-- = case eval x of
--     Just n -> exec sc (VAL n : s)
--     Nothing -> case h of
--                  Just m -> exec sc (VAL m : s)
--                  Nothing -> exec fc s
-- apply IH for h
--
-- = case eval x of
--     Just n -> exec sc (VAL n:s)
--     Nothing -> exec (comp' h sc fc) s

--apply IH for x
-- = exec (comp' x sc (comp' h sc fc)) s

-- Add x y
-- exec (comp' (Add x y) sc fc) s
-- = case eval x of
--     Just n -> case eval y of
--                 Just m -> exec sc (VAL (n + m) : s)
--                 Nothing -> eec fc s
--     Nothing -> exec fc s

-- define exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n + m) : s)

-- = case eval x of
--    Just n -> case eval y of
--               Just m -> exec (ADD sc) (VAL m : VAL n : s)
--               Nothing -> exec fc s
--    Nothing -> exec fc s

-- define exec (POP c) ( VAL _ : s) = exec c s

-- = case eval x of
--    Just n -> case eval y of
--                Just m -> exec (ADD sc) (VAL : VAL n : s)
--                Nothing -> exec (POP fc) (VAL n : s)
--    Nothing -> exec fc s

--apply IH for y

-- = case eval x of
--     Just n -> exec (cmp' y (ADD sc) (POP fc)) (VAL n : s)
--     Nothing -> exec fc s
--
-- apply IH for x
--
-- = exec (comp' x (comp' y (ADD sc) (POP fc)) fc) s
--
-- Calculate def of comp
-- exec (comp x) s
-- = case eval x of
--     Just n -> VAL n : s
--     Nothing -> s

-- define exec HALT s = s

-- = case eval x of
--     Just n -> exec HALT (VAL n : s)
--     Nothing -> exec HALT s
-- = exec (comp' x HALT HALT) s


type Stack = [Elem]
data Elem = VAL Int | HAN Code

--Target language:
data Code = HALT | PUSH Int Code | ADD Code |POP Code
--
-- --Compiler:
comp :: Expr -> Code
comp x = comp' x HALT HALT
comp' :: Expr -> Code -> Code -> Code
comp' (Val n) sc fc = PUSH n sc
comp' (Add x y) sc fc = comp' x (comp' y (ADD sc) (POP fc)) fc
comp' Throw sc fc = fc
comp' (Catch x h) sc fc = comp' x sc (comp' h sc fc)
--
-- --Virtual Machine
exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (VAL n : s)
exec (ADD c) ( VAL m : VAL n : s) = exec c (VAL (n + m) : s)
exec (POP c) (VAL _ : s) = exec c s
