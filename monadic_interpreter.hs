module Parser where

import Control.Monad 
import Control.Applicative
import Data.Char

------------------------------ Back End -------------------------------
-- Step 1 : declare abstract syntax tree (AST)
data Exp = Constant Int
         | Variable String
         | Plus Exp Exp
         | Minus Exp Exp
         | Greater Exp Exp
         | Less Exp Exp
         | Equal Exp Exp
         | Times Exp Exp
         | Divide Exp Exp
         deriving Show

data Com = Assign String Exp 
         | Seq Com Com
         | Cond Exp Com Com
         | While Exp Com
         | Declare String Exp Com
         | Print Exp
         deriving Show

-- Step 2: prepare testing programs
-- s1String is used to test the parse function
s1String = "declare x = 150 in declare y = 200 in {while y > 0 do if x > 100 then x:=x/2 else y:=y-100; print x}"
-- s1 is used to test the intepret function
s1 = Declare "x" (Constant 150) 
                 (Declare "y" (Constant 200) 
                              (Seq (While (Greater (Variable "y") (Constant 0)) 
                                          (Cond (Greater (Variable "x") (Constant 100)) 
                                                (Assign "x" (Divide (Variable "x") (Constant 2))) 
                                                (Assign "y" (Minus (Variable "y") (Constant 100))))) 
                                    (Print (Variable "x"))))

-- Step 3: declare data structure for environment 
--         and functions to manipulate the enviroment
type Location = Int
type Index = [String]
type Stack = [Int]

-- get the location of a variable
position :: String -> Index -> Location
position name index = 
    let pos n (nm:nms)  = if name == nm then n
                         else pos (n+1) nms 
    in pos 1 index 

-- get nth value from stack
fetch :: Location -> Stack -> Int
fetch n (v:vs) = 
    if n == 1 then v
    else fetch (n-1) vs

-- replace value in stack
put :: Location -> Int -> Stack -> Stack
put n x (v:vs) = 
    if n == 1 then x:vs
    else v:(put (n-1) x vs)

-- Step 4: choose the "State and Output" monad
newtype M a = StOut (Stack -> (a, Stack, String))

instance Functor M where 
    fmap = liftM

instance Applicative M where
    pure = return 
    (<*>) = ap

instance Monad M where
    return x = StOut (\n -> (x, n, ""))
    e >>=  f = StOut (\n -> let (a, n1, s1) = (unStOut e) n
                                (b, n2, s2) = unStOut (f a) n1
                            in (b, n2, s1++s2))
-- unStOut is to extract the embeded function from a monadic capsule
unStOut (StOut f) = f

-- Step 5: define some operation functions to work on monad to fulfill the computations
-- the operation to get a value from the environment given the Location
getfrom :: Location -> M Int
getfrom i = StOut (\ns -> (fetch i ns, ns, ""))

-- the operation to replace a value in the environment
write :: Location -> Int -> M()
write i v = StOut (\ns -> ((), put i v ns, ""))

-- the operation to add a value to the environment at the top
push :: Int -> M ()
push x = StOut (\ns -> ((), x:ns, ""))

-- the operation to remove a value from the top of the environment
pop:: M ()
pop = StOut (\m -> let (n:ns) = m in ((), ns, ""))

-- Step 6: prepare the evaluator for expressions
eval1 :: Exp -> Index -> M Int
eval1 exp index = 
    case exp of 
        Constant n -> return n
        Variable x -> let loc = position x index 
                        in getfrom loc
        Plus x y -> do { a <- eval1 x index;
                         b <- eval1 y index;
                         return (a+b)}
        Minus x y -> do { a <- eval1 x index;
                          b <- eval1 y index;
                          return (a-b)}
        Greater x y -> do { a <- eval1 x index;
                            b <- eval1 y index;
                            return (if a > b then 1 else 0)}
        Less x y -> do { a <- eval1 x index;
                         b <- eval1 y index;
                         return (if a < b then 1 else 0)}
        Equal x y -> do { a <- eval1 x index;
                          b <- eval1 y index;
                          return (if a == b then 1 else 0)}
        Times x y -> do { a <- eval1 x index;
                          b <- eval1 y index;
                          return (a*b)}
        Divide x y -> do { a <- eval1 x index;
                           b <- eval1 y index;
                           return (if b /= 0 then a `div` b else  0)}

-- Step 7: prepare the evaluator for expressions
interpret1 :: Com -> Index -> M ()
interpret1 stmt index = 
    case stmt of 
        Assign name e -> 
            let loc = position name index
            in do { v <- eval1 e index;
                    write loc v}
        Seq s1 s2 ->
            do {x <- interpret1 s1 index;
                y <- interpret1 s2 index;
                return () }
        Cond e s1 s2 -> 
            do {x <- eval1 e index;
                if x == 1 then interpret1 s1 index
                else interpret1 s2 index}
        While e b -> 
            let loop () = do { v <- eval1 e index;
                               if v == 0 then return ()
                               else do {interpret1 b index; 
                               loop()} } 
             in loop ()
        Declare nm e s -> 
            do { v <- eval1 e index;
                 push v;
                 interpret1 s (nm:index);
                 pop}
        Print e -> 
            do { v <- eval1 e index;
                 output v}

output :: Show a => a -> M ()
output v = StOut (\n -> ((),n,show v))

------------------------------ Front End -------------------------------
-- Step 1: prepare the grammar, which should be in our mind not in the program
{-<rexp> ::= <rexp> <relop> <expr> | <expr>
<expr> ::= <expr> <addop> <term> | <term>
<term> ::= <term> <mulop> <factor> | <factor>
<var> ::= <var> | <digiti> | (<expr>)
<digiti> ::= <digit> | <digit><digit>
<digit> ::= 0 | 1 | ... | 9
<addop> ::= + | -
<mulop> ::= * | /
<relop> ::= = > | < | =
<com> ::= <assign> | <seqv> | <cond> | <while> | <declare> | <printe>
<assign> ::= <identif> ":=" <rexp>
<seqv> ::= "{" <com> ";" <com> "}"
<cond> ::= "if" <rexp> "then" <com> "else" <com>
<while> ::= "while" <rexp> "do" <com>
<declare> ::= "declare" <identif> "=" <rexp "in" <com>
<printe> ::= "print" <rexp>-}

-- Step 2: prepare the parser combinator
newtype Parser a = Parser (String -> [(a,String)])

-- the parse function is used to extract the value from the monadic capsule
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

instance Functor Parser where 
    fmap f (Parser cs) = Parser (\s->[(f a,b) | (a,b)<-cs s])

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser (\s->[(f a, s2)|(f,s1)<-cs1 s, (a, s2)<-cs2 s1])

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f = Parser (\cs -> concatMap (\(a,cs')-> parse (f a) cs') $ parse p cs)

instance MonadPlus Parser where
    mzero = Parser (\cs -> [])
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

-----------------------
-- Parser Combinators 
-----------------------
item :: Parser Char
item = Parser (\xs -> case xs of 
                        "" -> []
                        (c:cs) -> [(c, cs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                           [] -> []
                           (x:xs) -> [x])

sat :: (Char -> Bool) -> Parser Char
sat p = do { c <- item ; if p c then return c else mzero}
--sat p = item >>= (\c -> if p c then return c else mzero)

(?) :: Parser a -> ( a -> Bool) -> Parser a
p ? test = do { b <- p ; if test b then return b else mzero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String 
string "" = return ""
string (c:cs) = do {char c ; string cs; return (c:cs)}

many' :: Parser a -> Parser [a]
many' p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p ; as <- many' p ; return (a:as)}

space :: Parser String
space = many' (sat isSpace)

token :: Parser a -> Parser a
token p = do { a <- p ; space ; return a}

symbol :: String -> Parser String 
symbol cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space ; p})

ident :: Parser [Char]
ident = do { l <- sat isAlpha ;
             lsc <- many' (sat (\a -> isAlpha a || isDigit a));
             return (l:lsc)}

identif :: Parser [Char]
identif = token ident

var :: Parser Exp
var = do { v <- identif ; return (Variable v)}

chainl :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
p `chainl1` op =  do { a <- p;
                       rest a }
                  where rest a = (do f <- op
                                     b <- p
                                     rest ( f a b))
                                 +++ return a
-----------------
-----------------

-- Step 3: combine parsers until you get the parser of the whole language
-- parsers for one and more than one digits
digit :: Parser Exp
digit = do {x <- token (sat isDigit);
        return ( Constant (ord x - ord '0'))}

digiti :: Parser Exp
digiti = do { p <- digit;
              l <- many' digit;
              return (foldl (\a b -> let Constant nra = a 
                                         Constant nrb = b
                                      in Constant (10*nra + nrb))
                            (Constant 0)
                            (p:l))}

-- parsers for expressions 
rexp :: Parser Exp
rexp = expr `chainl1` relop

expr :: Parser Exp
expr = term `chainl1` addop

term :: Parser Exp
term = factor `chainl1` mulop

factor :: Parser Exp
factor = var +++
         digiti +++
         do { symbol "(";
              n <- rexp;
              symbol ")";
              return n}

-- parsers for operators
addop :: Parser (Exp->Exp->Exp)
addop = do {symbol "-";
            return (Minus)}
        +++
        do {symbol "+";
            return (Plus)}

instance Fractional Int where

mulop :: Parser (Exp->Exp->Exp)
mulop = do {symbol "*";
            return (Times)}
        +++
        do {symbol "/";
            return (Divide)}
relop :: Parser (Exp->Exp->Exp)
relop = do {symbol ">";
            return (Greater)}
        +++
        do {symbol "<";
            return (Less)}
        +++
        do {symbol "=";
            return (Equal)}

-- parsers for commands
printe :: Parser Com
printe = do { symbol "print";
              x <- rexp;
              return (Print x)}
assign :: Parser Com
assign = do { x <- identif;
              symbol ":=";
              e <- rexp;
              return (Assign x e)}
seqv :: Parser Com
seqv = do { symbol "{";
            c <- com;
            symbol ";";
            d <- com;
            symbol "}";
            return (Seq c d)}
cond :: Parser Com
cond = do { symbol "if";
            e <- rexp;
            symbol "then";
            c <- com;
            symbol "else";
            d <- com;
            return (Cond e c d)}
while :: Parser Com
while = do { symbol "while";
             e <- rexp;
             symbol "do";
             c <- com;
             return (While e c)}
declare :: Parser Com
declare = do { symbol "declare";
               x <- identif;
               symbol "=";
               e <- rexp;
               symbol "in";
               c <- com;
               return (Declare x e c)}
com :: Parser Com
com = assign +++ seqv +++ cond +++ while +++ declare +++ printe

run::String -> Com
run = runParser com

runParser :: Parser a -> String -> a
runParser m s = 
    case parse m s of 
        [(res,[])] -> res
        [(_,_)] -> error "Parse error."

main::IO()
main = forever $ do
    putStr ">"
    a <- getLine
    case unStOut(interpret1 (run a) [])[] of 
        (_, _, a) -> print a
        _ -> error "error."
        
