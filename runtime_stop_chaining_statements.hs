-- What is to be done
-- Add boolean operations
-- Add for, while, switch
--
-- Rajouter argc et argv dans la première stack frame



import System.Exit (exitFailure)
import Control.Monad (when)
import Control.Monad.State
import qualified Data.Map as Map

safeTail :: [a] -> [a]
safeTail (_:xs) = xs    -- If the list is non-empty, return the rest of the list
safeTail []     = []    -- If the list is empty, return an empty list

intSafeDiv :: Int -> Int -> Int
intSafeDiv _ 0 = error "Division by 0 !!!\n"
intSafeDiv w v = div w v

floatSafeDiv :: Float -> Float -> Float
floatSafeDiv _ 0 = error "Division by 0 !!!\n"
floatSafeDiv w v = w / v
        

-- ==Lexer== -- 
data Token =  OPAR_TOK
            | CPAR_TOK
            | OCURLY_TOK
            | CCURLY_TOK
            | PLUS_TOK
            | MINUS_TOK
            | STAR_TOK
            | SLASH_TOK
            | GTH_TOK
            | LTH_TOK
            | EQU_TOK
            | BOOL_TOK Bool
            | IDEN_TOK String
            | FUNC_TOK
            | RETURN_TOK
            | INT_TOK Int
            | FLOAT_TOK Float
            | IF_TOK
            | ELSE_TOK
            | COMA_TOK
            | EOF_TOK

            deriving(Show, Eq)

isAlpha :: Char -> Bool
isAlpha c = (c >='a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isNum :: Char -> Bool
isNum c = (c >= '0' && c <= '9') || c == '.'

match :: (Char -> Bool) -> String -> String -> (String, String)
match ch s (c:cs)
    | ch c = match ch (s++[c]) cs
    | otherwise = (s, (c:cs))
match ch s [] = (s, [])

matchString :: String -> String -> (String, String)
matchString s (c:cs)
    | isAlpha c = matchString (s++[c]) cs
    | otherwise = (s, (c:cs))

delimiter :: [Char]
delimiter = [' ', '\t', '\n', '\r']

readIntOrFloat :: String -> Token
readIntOrFloat str = case (reads str :: [(Int, String)], reads str :: [(Float, String)]) of
    ([(i, "")], _) -> INT_TOK i    -- Successfully parsed as Int
    (_, [(f, "")]) -> FLOAT_TOK f   -- Successfully parsed as Float
    _ -> error "Could not parse as Int or Float"

tokenize :: [Token] -> String -> ([Token], String)
tokenize ts "" = (ts, "")
tokenize ts (c:cs) 
    | elem c delimiter = tokenize ts cs
    | c == '(' = tokenize (ts ++ [OPAR_TOK]) cs
    | c == ')' = tokenize (ts ++ [CPAR_TOK]) cs
    | c == '{' = tokenize (ts ++ [OCURLY_TOK]) cs
    | c == '}' = tokenize (ts ++ [CCURLY_TOK]) cs
    | c == '+' = tokenize (ts ++ [PLUS_TOK]) cs
    | c == '-' = tokenize (ts ++ [MINUS_TOK]) cs
    | c == '*' = tokenize (ts ++ [STAR_TOK]) cs
    | c == '/' = tokenize (ts ++ [SLASH_TOK]) cs
    | c == '=' = tokenize (ts ++ [EQU_TOK]) cs
    | c == '>' = tokenize (ts ++ [GTH_TOK]) cs
    | c == '<' = tokenize (ts ++ [LTH_TOK]) cs
    | c == ',' = tokenize (ts ++ [COMA_TOK]) cs
    | isNum c = let (s, r) = match isNum [c] cs
                in tokenize (ts++[readIntOrFloat s]) r
    | isAlpha c = 
        let (s, r) = match isAlpha [c] cs
        in case s of
            "func" -> tokenize (ts++[FUNC_TOK]) r
            "return" -> tokenize (ts++[RETURN_TOK]) r
            "true" -> tokenize (ts++[BOOL_TOK True]) r
            "false" -> tokenize (ts++[BOOL_TOK False]) r
            "if" -> tokenize (ts++[IF_TOK]) r
            "else" -> tokenize (ts++[ELSE_TOK]) r
            _ -> tokenize (ts++[IDEN_TOK s]) r
    | otherwise = (ts, (c:cs))

printTokens :: [Token] -> IO()
printTokens (t:ts) = do
    putStrLn (show t)
    printTokens ts
print [] = return


-- ==Parser== -- 

data ValueNode = INT_VAL Int -- 43
               | FLOAT_VAL Float -- 3.14
               | BOOL_VAL Bool -- true | false
               | CALLABLE [String] StatementNode -- my_func_name (NOT my_func_name() !!!)
               deriving Show

data ExprNode = N_ADD ExprNode ExprNode
          | N_SUB ExprNode ExprNode
          | N_NEG ExprNode
          | N_MUL ExprNode ExprNode
          | N_DIV ExprNode ExprNode
          | N_CONST_VAL ValueNode
          | N_VAR_VAL String
          | N_FUNC_CALL String [ExprNode]
          | N_BLOCK_WRAPPER StatementNode
          deriving Show

data StatementNode = N_IF { condition :: ExprNode, thenStatement :: StatementNode, elseStatement :: Maybe StatementNode}
                   | N_ASSIGN {varName :: String, val :: ExprNode}
                   | N_BLOCK [StatementNode]
                   | N_FUNC_CALL_WRAPPER { funcName :: String, args :: [ExprNode]}
                   | N_FUNC_DEF {funcName :: String, argsName :: [String], body :: StatementNode}
                   | N_RETURN ExprNode
                   | N_BUILTIN_PRINT 
                   | EOF -- Do i still need this ????? id think so
                   deriving Show

hoist :: Monad m => State s a -> StateT s m a
hoist = state . runState

type ContextFrames = [Map.Map String ValueNode]

addContextFrame :: State ContextFrames ()
addContextFrame = do
    currentTotal <- get
    let newTotal = currentTotal ++ [Map.empty]
    put newTotal

popContextFrame :: State ContextFrames ()
popContextFrame = 
    modify (init)

lookForAndAddVar :: String -> ValueNode -> [Map.Map String ValueNode] -> [Map.Map String ValueNode]
lookForAndAddVar s i [] = [Map.singleton s i]
lookForAndAddVar s i [t] = [Map.insert s i t]
lookForAndAddVar s i (t:ts) = if Map.notMember s t
    then (t:lookForAndAddVar s i ts)
    else (Map.insert s i t:ts)

assignValueToVar :: String -> ValueNode -> State ContextFrames ()
assignValueToVar s i = do
    modify (lookForAndAddVar s i)

getVarValue :: String -> ContextFrames -> (Maybe ValueNode)
getVarValue s [] = Nothing
getVarValue s c = case Map.lookup s (last c) of
    Nothing -> getVarValue s (init c)
    Just i -> Just i

getVar :: String -> State ContextFrames (Maybe ValueNode)
getVar s = do
    c <- get
    return (getVarValue s c)

getVarValueAndContext :: String -> ContextFrames -> Maybe (ValueNode, ContextFrames)
getVarValueAndContext s [] = Nothing
getVarValueAndContext s c = case Map.lookup s (last c) of
    Nothing -> getVarValueAndContext s (init c)
    Just i -> Just (i, c)

getVarAndContext :: String -> State ContextFrames (Maybe (ValueNode, ContextFrames))
getVarAndContext s = do
    c <- get
    return (getVarValueAndContext s c)

evalArgs :: [ExprNode] -> [ValueNode] -> StateT ContextFrames IO([ValueNode])
evalArgs (ex:exs) prev_v = do
    v <- evalExpr ex
    evalArgs exs (prev_v ++ [v])
evalArgs [] vs = return vs

passArgsToFunc :: [ValueNode] -> [String] -> State ContextFrames()
passArgsToFunc (v:vs) (s:ss) = do
    assignValueToVar s v
    passArgsToFunc vs ss
passArgsToFunc [] [] = return ()
passArgsToFunc [] s = error "Function needs more parameters"
passArgsToFunc v [] = error "Function doesn't take this much params"
    

callFunc :: String -> [ExprNode] -> StateT ContextFrames IO(Maybe ValueNode)
callFunc s args = do
    v <- hoist $ getVarAndContext s
    case v of
        Just (CALLABLE argsName body, c) -> do
            vs <- evalArgs args []
            let (_, ns) = runState (passArgsToFunc vs argsName) c
            -- here change evalStatement to evalFuc or block
            case body of
                N_BLOCK ss -> do
                    (ret_val, _) <- liftIO $ runStateT (evalBlock ss) ns
                    return ret_val
                N_RETURN e -> do
                    r <- evalExpr e
                    return (Just r)
                otherwise -> do
                    _ <- liftIO $ runStateT (evalStatement body) ns
                    return Nothing
        Nothing -> do 
            debug <- get
            error ("Function " ++ s ++ " not found" ++ show debug)
        otherwise -> error (s ++ " is a variable not a function")

evalBlock :: [StatementNode] -> StateT ContextFrames IO(Maybe ValueNode)
evalBlock (N_RETURN e:ss) = do
    v <- evalExpr e
    return $ Just v
evalBlock (N_BLOCK b:ss) = do
    hoist addContextFrame
    ret <- evalBlock b
    hoist popContextFrame
    case ret of
        Just e -> return (Just e)
        Nothing -> do
            evalBlock ss
evalBlock (s:ss) = do
    evalStatement s
    evalBlock ss
evalBlock [] = return Nothing

evalStatement :: (StatementNode) -> StateT ContextFrames IO()
evalStatement (N_BLOCK ss) = do
    hoist $ addContextFrame
    _ <- evalBlock ss
    hoist $ popContextFrame
    return ()
evalStatement (N_FUNC_DEF s args b) = do
    hoist $ assignValueToVar s (CALLABLE args b)
    return ()
evalStatement (N_FUNC_CALL_WRAPPER s args) = do
    _ <- callFunc s args
    return ()
evalStatement (N_ASSIGN s e) = do
    ee <- evalExpr e
    hoist (assignValueToVar s ee)
evalStatement (N_IF c t Nothing) = do
    ec <- evalExpr c
    when (toBool ec) ( do
        hoist addContextFrame
        evalStatement t
        hoist popContextFrame
        )
evalStatement (N_IF c t (Just e)) = do
    ec <- evalExpr c
    if (toBool ec) 
    then evalStatement t
    else evalStatement e
evalStatement (N_BUILTIN_PRINT) = do 
    p <- hoist $ getVar "toPrint"
    case p of
        Just v -> do
            liftIO $ putStr (show (toFloat v)) -- change this to toStr when string will be a thing for now floats are printable so ok
            return ()
        Nothing -> error "Print function didnt got it's argument, weird.."

evalStatement EOF = return ()


-- Type conversion
data Number = FLOAT_NUM Float | INT_NUM Int

toNumber :: ValueNode -> Number
toNumber (INT_VAL v) = INT_NUM v
toNumber (FLOAT_VAL v) = FLOAT_NUM v
toNumber (BOOL_VAL v) = if v then INT_NUM 1 else INT_NUM 0
toNumber (CALLABLE _ _) = error "Can convert function to number"

toInt :: ValueNode -> Int
toInt (INT_VAL v) = v
toInt (FLOAT_VAL v) = round v -- COERCE FLOAT TO INT BY ITSELF THROW AN ERROR INSTEAD IF YOU WHICH IT COULDN'T BE DONE
toInt (BOOL_VAL v) = if v then 1 else 0
toInt (CALLABLE _ _) = error "Can convert function to int"

toFloat :: ValueNode -> Float
toFloat (INT_VAL v) = fromIntegral v
toFloat (FLOAT_VAL v) = v
toFloat (BOOL_VAL v) = if v then 1.0 else 0.0
toFloat (CALLABLE _ _) = error "Can convert function to float"

toBool :: ValueNode -> Bool
toBool (INT_VAL v) =  if v /= 0 then True else False
toBool (FLOAT_VAL v) =  if v /= 0.0 then True else False
toBool (BOOL_VAL v) =  v
toBool (CALLABLE _ _) = error "Can convert function to bool"

inferRetNumType :: (Float -> Float -> Float) -> (Int -> Int -> Int) -> ExprNode -> ExprNode -> StateT ContextFrames IO(ValueNode)
inferRetNumType floatOpe intOpe l r = do
    ll <- evalExpr l
    rr <- evalExpr r
    case toNumber ll of
        FLOAT_NUM f -> return $ FLOAT_VAL (floatOpe f (toFloat rr))
        INT_NUM i -> case toNumber rr of
            FLOAT_NUM f -> return $ FLOAT_VAL (floatOpe (fromIntegral i) f)
            INT_NUM ii -> return $ INT_VAL (intOpe i ii)


evalExpr :: ExprNode -> StateT ContextFrames IO(ValueNode)
evalExpr (N_NEG n) = do
    e <- evalExpr n
    case toNumber e of
        INT_NUM i -> return $ INT_VAL (-i)
        FLOAT_NUM f -> return $ FLOAT_VAL (-f)
evalExpr (N_ADD l r) = inferRetNumType (+) (+) l r
evalExpr (N_SUB l r) = inferRetNumType (-) (-) l r
evalExpr (N_MUL l r) = inferRetNumType (*) (*) l r
evalExpr (N_DIV l r) = inferRetNumType floatSafeDiv intSafeDiv l r
evalExpr (N_CONST_VAL v) = return v
evalExpr (N_VAR_VAL s) = do
    v <- hoist $ getVar s
    case v of
        Nothing -> error ("Variable " ++ s ++ " not found")
        Just n -> return n
evalExpr (N_FUNC_CALL s args) = do
    r <- callFunc s args
    case r of
        Just e -> return e
        Nothing -> error "A function in an expression didn't return any value"
evalExpr (N_BLOCK_WRAPPER (N_BLOCK ss)) = do
    ret <- evalBlock ss
    case ret of
        Just e -> return e
        Nothing -> error "Block in an expression should return a value"
evalExpr (N_BLOCK_WRAPPER _) = error "Statement in N_BLOCK_WRAPPER isn't a block, weird..."

-- A better way to parse would have been to make a State [Token] applied to every parseX function
-- And a function like nextToken to modify the State
-- And every parseX function would have done nextToken firs tand than a switch case on it 
-- Would have removed all the token to return and evrything but np maybe one day

nextToken :: State [Token] Token
nextToken = do
    ts <- get
    if not (null ts) 
    then let (t:tr) = ts
        in do
        put tr
        return t
    else return EOF_TOK

putTokenBack :: Token -> State [Token]()
putTokenBack t = do
    ts <- get
    put ([t]++ts)
    return()

acceptToken :: Token -> State [Token] Bool
acceptToken t = do
    ts <- get
    if not (null ts) && (head ts == t)
        then do
            _ <- nextToken
            return True
        else return False

expectToken :: Token -> String -> State [Token]()
expectToken t s = do
    nt <- nextToken
    when (nt /= t) (error s)

parseArgsFuncCall :: [ExprNode] -> State [Token] [ExprNode]
parseArgsFuncCall prev_arg = do
    c <- acceptToken CPAR_TOK
    if c
        then return prev_arg
        else do
            when (not $ null prev_arg) 
                (expectToken COMA_TOK "Missing a coma between arguments")
            arg <- parseE
            parseArgsFuncCall (prev_arg++[arg])
    
parseArgsFuncDef :: [String] -> State [Token] [String]
parseArgsFuncDef prev_arg = do
    c <- acceptToken CPAR_TOK
    if c
        then return prev_arg
        else do
            when (not $ null prev_arg) 
                (expectToken COMA_TOK "Missing a coma between arguments")
            t <- nextToken
            case t of
                IDEN_TOK arg -> parseArgsFuncDef (prev_arg++[arg])
                otherwise -> error "Could not parse argument name"

-- parseB { S+ } | S
parseB :: [StatementNode] -> State [Token] StatementNode
parseB prev_s = do
    c <- acceptToken CCURLY_TOK
    if c
        then return (N_BLOCK prev_s)
    else do 
        s <- parseS
        parseB (prev_s++[s])

-- IF E {S} ELSE {S} | my_func(E) | IDEN = E
parseS :: State [Token] StatementNode
parseS = do
    t <- nextToken
    case t of
        OCURLY_TOK -> do
            b <- parseB []
            return b
        FUNC_TOK -> do
            name <- nextToken
            case name of
                IDEN_TOK s -> do
                    expectToken OPAR_TOK "Missing opening parenthese for the function definition"
                    arguments <- parseArgsFuncDef []
                    bdy <- parseS
                    return N_FUNC_DEF {funcName = s, argsName = arguments, body = bdy}
                otherwise -> error "Missing function name"
        RETURN_TOK -> do
           e <- parseE 
           return (N_RETURN e)
        IF_TOK -> do
            cond <- parseE
            thn <- parseS
            c <- acceptToken ELSE_TOK
            if not c
            then return N_IF {condition = cond, thenStatement = thn, elseStatement = Nothing}
            else do 
                else_block <- parseS
                return N_IF {condition = cond, thenStatement = thn, elseStatement = Just else_block}
        IDEN_TOK s -> do
            n <- nextToken
            case n of
                EQU_TOK -> do
                    value <- parseE
                    return N_ASSIGN { varName = s, val = value}
                OPAR_TOK -> do
                    arguments <- parseArgsFuncCall []
                    return N_FUNC_CALL_WRAPPER { funcName = s, args = arguments}
                otherwise -> error "Error parsing the statement"
        EOF_TOK -> return EOF
        otherwise -> do
            debug <- get
            error (show debug)
                    

-- E -> F E'
parseE :: State [Token] ExprNode
parseE = do
    e <- parseF
    parseE' e

-- E' -> + F E' | end
parseE' :: ExprNode -> State [Token] ExprNode
parseE' ln = do
    c <- acceptToken PLUS_TOK
    if c
        then do
            rn <- parseF 
            e <- parseE' (N_ADD ln rn)
            return e
        else do
            cc <- acceptToken MINUS_TOK
            if cc
                then do
                    rn <- parseF 
                    e <- parseE' (N_SUB ln rn)
                    return e
                else return ln

-- F -> T F'
parseF :: State [Token] ExprNode
parseF = do
    e <- parseT
    parseF' e

-- F' -> * T F' | end
parseF' :: ExprNode -> State [Token] ExprNode
parseF' ln = do
    c <- acceptToken STAR_TOK
    if c
        then do
            rn <- parseT
            e <- parseF' (N_MUL ln rn)
            return e
        else do
            cc <- acceptToken SLASH_TOK
            if cc
                then do
                    rn <- parseT
                    e <- parseF' (N_DIV ln rn)
                    return e
                else return ln

-- T -> 42 | 3.14 | true | my_var_name | (E) | -E
parseT :: State [Token] ExprNode
parseT = do
    t <- nextToken
    case t of
        IDEN_TOK s -> do
            c <- acceptToken OPAR_TOK
            if c
                then do
                    arguments <- parseArgsFuncCall []
                    return (N_FUNC_CALL s arguments)
                else return (N_VAR_VAL s)
        INT_TOK v -> return (N_CONST_VAL (INT_VAL v))
        FLOAT_TOK v -> return (N_CONST_VAL (FLOAT_VAL v))
        BOOL_TOK v -> return (N_CONST_VAL (BOOL_VAL v))
        MINUS_TOK -> do
            e <- parseE
            return (N_NEG e)
        OPAR_TOK -> do
            e <- parseE
            expectToken CPAR_TOK "Didn't match closing parenthese"
            return e
        OCURLY_TOK -> do
            b <- parseB []
            return (N_BLOCK_WRAPPER b)
        otherwise -> error "Failed to parse expression"
        
main :: IO()
main = do
    contents <- getContents
    let (tokens, _) = tokenize [] contents
    -- printTokens tokens
    let ast = evalState parseS ([OCURLY_TOK] ++ tokens ++ [CCURLY_TOK])
    -- putStr $ show ast
    _ <- runStateT (evalStatement ast) [Map.singleton "print" (CALLABLE ["toPrint"] (N_BUILTIN_PRINT))]
    return ()
