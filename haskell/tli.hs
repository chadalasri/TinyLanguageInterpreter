import Data.Char
import System.Environment
import System.IO

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

--constructors for operators, vars and constants
--in order
--Constant 
--Variable 
--Plusoperator Divoperator Mult Sub
--LessThan Greaterthan LessThanOrEqual GreaterThanOrEqual Equivalent("==") NotEquivalent("!=")

data Expr = Constant Float | Var String | Str String |
     LE_ Expr Expr |
     GE_ Expr Expr |
     LT_ Expr Expr |
     GT_ Expr Expr |
     EQ_ Expr Expr |
     NEQ_ Expr Expr |
     Minus Expr Expr |
     Times Expr Expr |
     Div Expr Expr |
     ExprError String |
     Plus Expr Expr deriving (Show)

data Stmt =
     Let String Expr |
     If Expr String |
     Input String |
     Error String |
     Print [Expr] deriving (Show)


-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel [] = False
isLabel [x] = False
isLabel [x, y] = False
isLabel (x:xs) = if last xs == ':' then True else False
--isLabel _ = False -- there are no labels in this nano version of the tiny language

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Times (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Div (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<=":e2:[]) = LE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">":e2:[]) = GT_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:">=":e2:[]) = GE_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"==":e2:[]) = EQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NEQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr [x] = if (isAlpha (head x)) then (Var x) else (Constant (read x))

--parse print takes the input of print and parses the expressions
parsePrint :: [String] -> [Expr]
parsePrint [] = []
parsePrint (",":rest) = parsePrint rest
parsePrint (e1:"+":e2:rest) = [Plus (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"-":e2:rest) = [Minus (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"*":e2:rest) = [Times (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"/":e2:rest) = [Div (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"<":e2:rest) = [LT_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"<=":e2:rest) = [LE_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:">":e2:rest) = [GT_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:">=":e2:rest) = [GE_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"==":e2:rest) = [EQ_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (e1:"!=":e2:rest) = [NEQ_ (parseExpr [e1]) (parseExpr [e2])] ++ (parsePrint rest)
parsePrint (x:rest) = if (isAlpha (head x)) then [(Var x)] ++ (parsePrint rest)  else [(Constant (read x))] ++ (parsePrint rest)



-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Stmt
parseStmt _ [] = Error "Missing Statement"
parseStmt "let" (v:"=":expr) = Let v (parseExpr expr)
parseStmt "print" expr = (Print (parsePrint expr))
parseStmt "input" (first:rest) = Input first 
parseStmt "if" expr = If (parseExpr (init (init expr))) (last expr)

--parseStmts :: [[String]] -> [Stmt] 
--parseStmts [] = []
--parseStmts (("print":rest):rester) = [(Print (parsePrint rest))]++(parseStmts rester)
--parseStmts ((first:rest):rester) = [(parseStmt first rest)] ++ (parseStmts rester)


--try:: [[String]] -> [[Stmt]]
--try [] = []
--try (("print":rest):rester) = [Print (parsePrint rest)] ++ (try rester)
--try (first:rester) = parseStmts(first):(try rester) 

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
--include linenum and symbol table
parseLine :: [String] -> Stmt
parseLine (first:rest) =
	  if (isLabel first) then parseLine rest
	  else (parseStmt first rest)

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

-- evaluates the given Expr with the variable values found in the given ST
eval :: Expr ->SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env = (eval e1 env) / (eval e2 env)
eval (LT_ e1 e2) env = if (eval e1 env) < (eval e2 env) then 1 else 0
eval (LE_ e1 e2) env = if (eval e1 env) <= (eval e2 env) then 1 else 0
eval (GT_ e1 e2) env = if (eval e1 env) > (eval e2 env) then 1 else 0
eval (GE_ e1 e2) env = if (eval e1 env) >= (eval e2 env) then 1 else 0
eval (EQ_ e1 e2) env = if (eval e1 env) == (eval e2 env) then 1 else 0
eval (NEQ_ e1 e2) env = if (eval e1 env) == (eval e2 env) then 0 else 1
--when dealing with comparators return 1 if true and 0 if false

--prompt :: (Read a) => IO a
--prompt =   do
 --          number <- getLine
  --          return number	 

-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
perform:: Stmt -> SymTable -> Float -> [String] ->String -> (SymTable, [String], String, Float)
perform (Print e) env lineNum input output = (env, input, output++(show (eval (head e) env) ++ "\n"), lineNum+1)
--perform (Input e) env lineNum input output = ((e, prompt):env, input, output, lineNum+1) 
perform (Let id (Str str)) env lineNum input output = ((id, (getFromTable env str)):env, input, output, lineNum+1)
perform (Let id e) env lineNum input output = ((id,(eval e env)):env, input, output, lineNum+1)
--perform (If expr str) env lineNum input output = if (eval expr env) == 1 then (env, input, output, lineNum+1) else 


--in the event of an if statement goes to sends into perform the proper stmt by checking if the line number matches the given label number
checkPerform :: [Stmt] -> SymTable -> Float -> Float -> [String] -> String -> String
--if linenum == label then perform the rest of the function using run to cycle through the rest of the stmts otherwise ignore the stmt, add 1 to lineNum and recur
checkPerform [] _ _ _ _ _ = []
checkPerform (curr:rest) env lineNum labelLine input output = if (lineNum == labelLine) then let (env1, input1, output1, lineNum1) = (perform curr env lineNum input output) in run rest env1 input1 output1 lineNum1 else (checkPerform rest env (lineNum+1) labelLine input output)

--exprFromTable:: SymTable -> Expr

--given a symbol table and a label find the line number
getFromTable :: SymTable -> String -> Float
getFromTable [] _ = -1
getFromTable ((first, second):rest) str = if (str == (init first)) then second else getFromTable rest str

-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
run :: [Stmt] -> SymTable -> [String] -> String -> Float -> String
run [] _ _ output _ = output
--if there is an if statement and it evaluates true transfer control using check perform, which will function like run otherwise ignore if and run the rest
run ((If expr str):rest) env input output lineNum = if (eval expr env) == 1.0 then checkPerform rest env lineNum (getFromTable env str) input output else 
																run rest env input output lineNum
run (curr:rest) env input output lineNum = 
    let (env1, input1, output1, lineNum1) = perform curr env lineNum input output in run rest env1 input1 output1 lineNum1

-- given list of list of tokens, a ST, and return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable ->  ([Stmt], SymTable)
parseTest []  st = ([], st)
parseTest (first:rester) env = ([(parseLine first)] ++ (fst (parseTest rester env)) , (holdSymNum (first:rester) 1 env))
-- needs completing for partial credit

--takes a string, a symbol table and a float  and finds the label in the string and returns the filled symbol table
startSym :: [String] -> SymTable -> Float-> SymTable
startSym (first:rest) env num =
	  if (isLabel first) then 
              env ++ [(first, num)]
	  else env

--recur with symstart, fills symbol tabel with labels given $ (map words (lines contents)) 1 []
holdSymNum :: [[String]] -> Float -> SymTable -> SymTable
holdSymNum [] _ [] = []
holdSymNum [] _ env = env
holdSymNum (first:rest) num env = holdSymNum rest (num+1) (startSym first env num)

main = do
     args <- getArgs
     progName <- getProgName
     pfile <- openFile (args!!0) ReadMode
     contents <- hGetContents pfile
     --putStrLn "Labels:  "
     --putStrLn coNtents
     let parsedContents = (map words (lines contents))
     let labelTable = holdSymNum parsedContents 1 []
     --let (listostmnts, symtablefilled) = (map parseLine (map words (lines contents)) [])
     putStr (run (map parseLine parsedContents) labelTable [] "" 1)
     hClose pfile
