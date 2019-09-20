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
--COMPILER VER A
--Constructed by the desire to apply IH and end up with form exec c' s
--------------------------------------------------------------------------------

--type Stack = [Elem]
--data Code = HALT | PUSH Int Code | ADD Code | FAIL
--data Elem = VAL Int | HAN Code


--TWO SPECIFICATIONS:
--1. Compiling e with a stack is the same as pushing the evaluated result of e
--onto the stack if it succeeds and throwing an exception if it fails
--exec (comp e) s = case eval e of
--                    Just n -> VAL n : s
--                    Nothing -> faill s
--
--2. Compiling e with a continuation and stack is the same as pushing the evaluated
--result of e onto the stack then compiling the continuation if it e succeeds
--OR throwing an exception if e fails
--exec (comp' e c) s = case eval e of
--                        Just n -> exec c (VAL n : s)
--                        Nothing -> faill s

--If we do Nothing -> faill e c s instead of faill s for the Nothing case, then
--when we try to prove the Add x y case for comp', we can't apply IH:
-- exec (comp' (Add x y) c) s
-- = case eval x of
--     Just n -> case eval y of
--                    Just m -> exec c (VAL (n + m):s)
--                    Nothing -> faill (Add x y) c s
--     Nothing -> faill (Add x y) c s

-- //define exec (ADD c) (VAL m : VAL n : s) = exec c (VAL (n+m):s)

-- = case eval x of
--     Just n -> case eval y of                                     --exec (comp' y (ADD c)) (VAL n : s)
--                Just m -> exec (ADD c) (VAL m : VAL n : s)
--                Nothing -> faill (Add x y) c s                    --cannot solve faill (Add x y) c s = faill y (ADD c) (VAL n : s) because it has unbound x value
--     Nothing -> faill (Add x y) c s           --faill (Add x y) c s = faill y (ADD c) s also unbound x

--Since e and c are causing problems, they should be omitted from faill.

--COMP' DEFINITION DERIVATION
--Calculate def of comp' by performing induction on x and
--rewriting exec (comp' x c) s into the form exec c' s for some code c'.
--The c' defines comp'.

--Base case #1: Val n
--exec (comp' (Val n) c) s
-- = exec c (VAL n : s)

--define exec (PUSH n c) s = exec c (VAL n : s)

-- = exec (PUSH n c) s
-- c' = (PUSH n c)

--Base case #2: Throw
--exec (comp' Throw c) s
-- = faill s
--define exec FAIL s = faill s
-- = exec FAIL s
-- c' = FAIL

--IH cases:
--Add x y
-- exec (comp' (Add x y) c) s
-- = case eval x of
--     Just n -> case eval y of
--                Just m -> exec c (VAL (n + m) : s)
--                Nothing -> faill s
--     Nothing -> faill s

--define exec (ADD c) (VAL m:VAL n:s) = exec c (VAL (n+m):s)

-- = case eval x of
--     Just n -> case eval y of
--                Just m -> exec (ADD c) (VAL m:VAL n:s)
--                Nothing -> faill s
--     Nothing -> faill s

--define faill (VAL n:s) = faill s  //Why is this is valid?

-- = case eval x of
--     Just n -> case eval y of
--                Just m -> exec (ADD c) (VAL m:VAL n:s)
--                Nothing -> faill (VAL n:s)
--     Nothing -> faill s

--apply IH for y:
-- exec (comp' y c) s = case eval y of
           --             Just m -> exec (ADD c) (VAL m:VAL n:s)
           --             Nothing -> faill (VAL n:s)
-- = case eval x of
--     Just n -> exec (comp' y (ADD c)) (VAL n:s)
--     Nothing -> faill s

--apply IH for x:

-- exec (comp' x c) s = case eval x of
           --             Just m -> exec (ADD c) (VAL n:s)
           --             Nothing -> faill s

-- = exec (comp' x (comp' y (ADD c))) s
--c' = (comp' x (comp' y (ADD c)))

--Catch x h
-- exec (comp' (Catch x h) c ) s
-- = case eval x of
--     Just n -> exec c (VAL n : s)
--     Nothing -> case eval h of
--                  Just m -> exec c (VAL m : s)
--                  Nothing -> faill s
--apply IH for h:
-- exec (comp' h c) s = case eval h of
           --             Just m -> exec c (VAL m : s)
           --             Nothing -> faill s
-- = case eval x of
--     Just n -> exec c (VAL n : s)
--     Nothing -> exec (comp' h c) s

--We can't apply IH to this because the Nothing case is in the wrong format, so we need to solve
--faill s = exec (comp' h c) s
--but h and c are unbound, so we need to find a way to move h and c to the left side
--Confused as to why this unboundedness issue isn't deterring us like prev unbound issues
--So it's bad practice for the compiler to push source language code onto the stack
--even if it's temporary? Is HAN h c temporary? Or is it going to remain on stack forever?
--Don't see how faill (HAN h c: s) is different from faill (HAN (comp' h c):s)?
--HAN h c is a constructor that takes Expr e and Code c. HAN c' just takes Code c.
--So wrapping Code with an Elem datatype is allowed onto the stack but
--an Expr + Code wrapped into an Elem datatype is not allowed?

--define faill (HAN (comp' h c):s) = exec (comp' h c) s
-- = case eval x of
--     Just n -> exec c (VAL n : s)
--     Nothing -> faill (HAN (comp' h c):s)

--Still wrong format, but close.
--Instead of changing the Nothing case, we can change the Just case.
--logical to erase handler code from stack if not needed.

--define exec (UNMARK c) (VAL n: HAN _ :s) = exec c (VAL n : s)

-- = case eval x of
--     Just n -> exec (UNMARK c) (VAL n: HAN _ :s)
--     Nothing -> faill (HAN (comp' h c):s)

-- apply IH for x:

-- =  exec (comp' x (UNMARK c)) (HAN (comp' h c):s)

-- We want to isolate s for the final form, so need to bring in a new constructor
--define exec (MARK c' c) s = exec c (HAN c' : s)

-- = exec (MARK (comp' h c) (comp' x (UNMARK c))) s
-- we can define c' = (MARK (comp' h c) (comp' x (UNMARK c)))


--COMP DEFINITION DERIVATION
--Rewrite exec (comp x) s into the form exec c' s
--This whole process is a mystery to me
--Why does the proof proceed in such a manner? Why don't we perform induction?
--What's the point of relating comp and comp'?
-- exec (comp x) s
-- = case eval x of
--     Just n -> VAL n : s
--     Nothing -> faill s

--Change format in order to apply def of comp'
-- define exec HALT s = s

-- = case eval x of
--    Just n -> exec HALT (VAL n : s)
--    Nothing -> faill s

-- --apply definition of exec (comp' x c) s

-- = exec (comp' x HALT) s


--Target Language
data Code = HALT | PUSH Int Code | ADD Code | FAIL | MARK Code Code | UNMARK Code

--compiles expr to code
comp :: Expr -> Code
comp x = comp' x HALT

--also takes a code continuation
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' Throw c = FAIL
comp' (Catch x h) c = MARK (comp' h c) (comp' x (UNMARK c))

-- Virtual Machine / Interpreter / Intermediate code
type Stack = [Elem]
data Elem = VAL Int | HAN Code

exec :: Code -> Stack -> Stack
exec HALT s = s
exec (PUSH n c) s = exec c (VAL n : s)
exec (ADD c) (VAL m : VAL n:s) = exec c (VAL (n+m):s)
exec FAIL s = faill s
exec (MARK c' c) s = exec c (HAN c' : s)
exec (UNMARK c) (VAL n: HAN _: s) = exec c (VAL n: s)

--elements are popped from stack until the handler code is found, at which
--point execution then transfers to the handler code
faill :: Stack -> Stack
faill [] = []
faill (VAL n: s) = faill s
faill (HAN c:s) = exec c s
