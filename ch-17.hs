--1
--When I first attemped this problem a couple of weeks ago, I was very confused and decided to take a peek at the solution
--http://www.cs.nott.ac.uk/~pszgmh/ccc.pdf
--But the solution was also very confusing so I just ended up trying to make sense of it.
--Below is my thought process:

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr

--Wondered for a long time why the stack type couldn't be [Maybe Int].
--But I guess the type being [Elem] or [Int] makes more sense in context of real life.
--Why does the solution say to use Elem = Val Int and not Elem = Int?
--I do understand that in order to permit pushing constructors onto the stack, which
--will come later, the type of the stack cannot be [Int] (or [Val Int])
type Stack = [Elem]


--Since the stack only takes int n and not Just n
--exec (comp' e c) s = exec c (eval e : s) needs to be changed
--to exec (comp' e c) s = exec c (n : s) for some n that results from eval e = Just n.

exec (comp' e c) s = exec c (n : s) --if eval e = Just n

--What if eval e = Nothing?
--What does it mean to keep the code aligned? How do you get from fail x c s to fail s?

exec (comp' e c) s = fail s

--Combined:
exec (comp' e c) s = case eval e of
                       Just n -> exec c (n : s)
                       Nothing -> fail s

--Proof using induction on x:
--write left side of exec (comp' e c) s = exec c (n : s) into exec c' s

--Base cases:
--Val
exec (comp' (Val n) c) s
= exec c (n : s)
--define exec (PUSH n c) s = exec c (n : s) --Constructor
= exec (PUSH n c) s

--Throw
exec (comp' Throw c) s
= fail s
--define exec FAIL s = fail s --Constructor
= exec FAIL s

--IH cases:
--Add
exec (comp' (Add x y) c) s
= case eval x of
    Just n -> case eval y of
               Just m -> exec c ((n + m) : s)
               Nothing -> fail s
    Nothing -> fail s
--A bit hard to follow here
--define exec (ADD c) (m:n:s) = exec c ((n+m):s)
--Why cant stack be in the format ((n + m) : s)?
= case eval x of
    Just n -> case eval y of
               Just m -> exec (ADD c) (m:n:s)
               Nothing -> fail s
    Nothing -> fail s
--define fail (n:s) = fail s
= case eval x of
    Just n -> case eval y of
               Just m -> exec (ADD c) (m:n:s)
               Nothing -> fail (n:s)
    Nothing -> fail s
--apply IH for y:
-- exec (comp' y c) s = case eval y of
           --             Just m -> exec (ADD c) (m:n:s)
           --             Nothing -> fail (n:s)

= case eval x of
    Just n -> exec (comp' y (ADD c)) (n:s)
    Nothing -> fail s
--apply IH for x:
-- exec (comp' x c) s = case eval x of
           --             Just m -> exec (ADD c) (n:s)
           --             Nothing -> fail (s)
= exec (comp' x (comp' y (ADD c))) s

--Catch
exec (comp' (Catch x h) c ) s
= case eval x of
    Just n -> exec c (n : s)
    Nothing -> case eval h of
                 Just m -> exec c (m : s)
                 Nothing -> fail s
--apply IH for h:
--How does applying the IH even work here? h and x are completely diff unlike x and y in prev case
-- exec (comp' h c) s = case eval h of
           --             Just m -> exec c (m : s)
           --             Nothing -> fail s
= case eval x of
    Just n -> exec c (n : s)
    Nothing -> exec (comp' h c) s
--We can't apply IH to this because the Nothing case is in the wrong format, so need to solve fail s = exec (comp' h c) s
--Don't quite understand the explanation for why fail (HAN h c: s) = exec (comp' h c) s doesn't work
--define fail (HAN (comp' h c):s) = exec (comp' h c) s
= case eval x of
    Just n -> exec c (n : s)
    Nothing -> fail (HAN (comp' h c):s)
--define exec (UNMARK c) (n: HAN _ :s) = exec c (n:s) --this is necessary to obtain the format neccesary for IH
= case eval x of
    Just n -> exec (UNMARK c) (n: HAN _ :s)
    Nothing -> fail (HAN (comp' h c):s)
-- IH for x:
-- exec (comp' x (UNMARK c)) (HAN (comp' h c):s) = case eval x of
--                                                   Just n -> exec (UNMARK c) (n: HAN _ :s)
--                                                   Nothing -> fail (HAN (comp' h c):s)
=  exec (comp' x (UNMARK c)) (HAN (comp' h c):s)
--Is MARK constructor essential?
--MARK Code Code
--define exec (MARK c' c) s = exec c (HAN c' : s)
= exec (MARK (comp' h c) (comp' x (UNMARK c))) s

--Prove comp aka bring in HALT constructor (same as original)
--We know that exec (comp e) s = eval e : s in the original version
--So with exceptions it should be
exec (comp e) s = case eval x of
                   Just n -> n : s
                   Nothing -> fail s

--Prove this by rewriting lefthand side exec (comp x) s into form exec c' s:
exec (comp x) s
=  case eval x of
      Just n -> n : s
      Nothing -> fail s
--We want to use the def of comp' so need to change format of Just case
--define exec HALT s = s
=  case eval x of
      Just n -> exec HALT (n : s)
      Nothing -> fail s
--apply comp' def
= exec (comp' e HALT) s

----------------------------------------

data Code = HALT | PUSH Int Code | ADD Code | FAIL
data Elem = Int | Han Code
