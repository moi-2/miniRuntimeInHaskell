-- What is to be done
-- add string support ??
-- add array support ???
--  for array :
--      missing the len function
--
--      FOR BUILTIN FUNCTION :
--          MAKE A N_BUILTIN_FUNC ExprNode
--          AND IN runFunction have a case more where you check for it and run it like a block
--
--
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
            | OSQUARY_TOK
            | CSQUARY_TOK
            | PLUS_TOK
            | MINUS_TOK
            | STAR_TOK
            | SLASH_TOK
            | GTH_TOK
            | LTH_TOK
            | GOREQU_TOK
            | LOREQU_TOK
            | EQU_TOK
            | OR_TOK
            | AND_TOK
            | CONCAT_TOK
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
tokenize ts (fst:snd:r)
    | [fst,snd] == "++" = tokenize (ts ++ [CONCAT_TOK]) r
    | [fst,snd] == "==" = tokenize (ts ++ [EQU_TOK]) r
    | [fst,snd] == "<=" = tokenize (ts ++ [LOREQU_TOK]) r
    | [fst,snd] == ">=" = tokenize (ts ++ [GOREQU_TOK]) r
tokenize ts (c:cs) 
    | elem c delimiter = tokenize ts cs
    | c == '(' = tokenize (ts ++ [OPAR_TOK]) cs
    | c == ')' = tokenize (ts ++ [CPAR_TOK]) cs
    | c == '{' = tokenize (ts ++ [OCURLY_TOK]) cs
    | c == '}' = tokenize (ts ++ [CCURLY_TOK]) cs
    | c == '[' = tokenize (ts ++ [OSQUARY_TOK]) cs
    | c == ']' = tokenize (ts ++ [CSQUARY_TOK]) cs
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
            "or" -> tokenize (ts++[OR_TOK]) r
            "and" -> tokenize (ts++[AND_TOK]) r
            _ -> tokenize (ts++[IDEN_TOK s]) r
    | otherwise = (ts, (c:cs))

printTokens :: [Token] -> IO()
printTokens (t:ts) = do
    putStrLn (show t)
    printTokens ts
print [] = return


-- ==Parser== -- 

data RValueNode = INT_VAL Int -- 43
               | FLOAT_VAL Float -- 3.14
               | BOOL_VAL Bool -- true | false
               | CALLABLE [String] StatementNode ContextFrames -- my_func_name (NOT my_func_name() !!!)
               | ARRAY_VAL [RValueNode]
               deriving (Show, Eq)

data LValueNode = LValueNode String [RValueNode]
                deriving (Show, Eq)

data ExprNode = N_ADD ExprNode ExprNode
          | N_SUB ExprNode ExprNode
          | N_NEG ExprNode
          | N_MUL ExprNode ExprNode
          | N_DIV ExprNode ExprNode
          | N_EQU ExprNode ExprNode
          | N_GTH ExprNode ExprNode
          | N_LTH ExprNode ExprNode
          | N_GOREQU ExprNode ExprNode
          | N_LOREQU ExprNode ExprNode
          | N_CONCAT ExprNode ExprNode
          | N_CONST_VAL RValueNode
          | N_ARRAY_LITERAL [ExprNode]
          | N_VAR_VAL String
          | N_FUNC_CALL ExprNode [ExprNode]
          | N_INDEXING ExprNode ExprNode
          | N_BLOCK_WRAPPER StatementNode
          deriving (Show, Eq)

data StatementNode = N_IF { condition :: ExprNode, thenStatement :: StatementNode, elseStatement :: Maybe StatementNode}
                   | N_ASSIGN {varExpr :: ExprNode, val :: ExprNode}
                   | N_BLOCK [StatementNode]
                   | N_FUNC_CALL_WRAPPER { exprToCall :: ExprNode} -- exprToCall will alway be a N_FUNC_CALL
                   | N_FUNC_DEF {funcName :: LValueNode, argsName :: [String], body :: StatementNode}
                   | N_RETURN ExprNode
                   | N_BUILTIN_FUNC BuiltinFunc
                   | EOF -- Do i still need this ????? id think so
                   deriving (Show, Eq)

data BuiltinFunc = PRINT
                 | LEN
                 deriving (Show, Eq)

hoist :: Monad m => State s a -> StateT s m a
hoist = state . runState

type ContextFrames = [Map.Map String RValueNode]

addContextFrame :: State ContextFrames ()
addContextFrame = do
    currentTotal <- get
    let newTotal = currentTotal ++ [Map.empty]
    put newTotal

popContextFrame :: State ContextFrames ()
popContextFrame =
    modify (init)

followList :: Int -> [RValueNode] -> RValueNode -> [RValueNode] -> [RValueNode]
followList _ _ _ [] = []
followList index i newVal (x:xs)
    | index == 0 = followDataStructur i newVal x:xs
    | otherwise = x : followList (index-1) i newVal xs

followDataStructur :: [RValueNode] -> RValueNode -> RValueNode -> RValueNode
followDataStructur (i:ii) newVal (ARRAY_VAL vs) = do
    let index = toInt i
    ARRAY_VAL $ followList index ii newVal vs -- if it is an array update the array
followDataStructur [] newVal _ = newVal
followDataStructur is _ _ = error "Dont support anything other then arrays for now"

fDSwrapper :: RValueNode -> Maybe RValueNode -- just wrap RValueNode in a Maybe
fDSwrapper v = Just v

lookForAndAddVar :: LValueNode -> RValueNode -> [Map.Map String RValueNode] -> [Map.Map String RValueNode]
lookForAndAddVar (LValueNode s []) v [] = [Map.singleton s v]
lookForAndAddVar (LValueNode s []) v [t] = [Map.insert s v t]
lookForAndAddVar (LValueNode s i) v [] = error $ "Can index in "++s++" because can't find it"
lookForAndAddVar (LValueNode s is) v (t:ts) = if Map.notMember s t
    then (t:lookForAndAddVar (LValueNode s is) v ts)
    else (Map.update (fDSwrapper . followDataStructur is v) s t:ts)

assignValueToVar :: LValueNode -> RValueNode -> State ContextFrames ()
assignValueToVar l i = do
    modify (lookForAndAddVar l i)

getVarValue :: String -> ContextFrames -> (Maybe RValueNode)
getVarValue s [] = Nothing
getVarValue s c = case Map.lookup s (last c) of
    Nothing -> getVarValue s (init c)
    -- if it is a CALLABLE then put it's context with it
    Just (CALLABLE argsName body _) -> Just $ CALLABLE argsName body c
    Just i -> Just i

getVar :: String -> State ContextFrames (Maybe RValueNode)
getVar s = do
    c <- get
    return (getVarValue s c)

evalArgs :: [ExprNode] -> [RValueNode] -> StateT ContextFrames IO([RValueNode])
evalArgs (ex:exs) prev_v = do
    v <- evalExprToRValue ex
    evalArgs exs (prev_v ++ [v])
evalArgs [] vs = return vs

passArgsToFunc :: [RValueNode] -> [String] -> State ContextFrames()
passArgsToFunc (v:vs) (s:ss) = do
    assignValueToVar (LValueNode s [])v
    passArgsToFunc vs ss
passArgsToFunc [] [] = return ()
passArgsToFunc [] s = error "Function needs more parameters"
passArgsToFunc v [] = error "Function doesn't take this much params"

runFunction :: Maybe RValueNode -> [ExprNode] -> StateT ContextFrames IO(Maybe RValueNode)
runFunction v args = do
    case v of
        Just (CALLABLE argsName body c) -> do
            vs <- evalArgs args []
            let (_, ns) = runState (passArgsToFunc vs argsName) c
            case body of
                N_BLOCK ss -> do
                    (ret_val, _) <- liftIO $ runStateT (evalBlock ss) ns
                    return ret_val
                N_RETURN e -> do
                    (r, _) <- liftIO $ runStateT (evalExprToRValue e) ns
                    return (Just r)
                N_BUILTIN_FUNC f -> do
                    (ret_val, _) <- liftIO $ runStateT (evalBuiltin f) ns -- myabe builtin fnuction dont even need ns
                    return ret_val
                otherwise -> do
                    _ <- liftIO $ runStateT (evalStatement body) ns
                    return Nothing
        Nothing -> error ("Function not found")
        otherwise -> error ("You tried to call a variable not a function")


callFunc :: ExprNode -> [ExprNode] -> StateT ContextFrames IO(Maybe RValueNode)
callFunc (N_VAR_VAL s) args = do
    v <- hoist $ getVar s
    ret <- runFunction v args
    return ret
callFunc (N_FUNC_CALL e args) aargs = do -- to handle case of a function (foo) returning a function directly called ( foo()()  )
    v <- callFunc e args
    ret <- runFunction v aargs
    return ret
callFunc _ _ = error "Your calling something that isn't callable"

evalBlock :: [StatementNode] -> StateT ContextFrames IO(Maybe RValueNode)
evalBlock (N_RETURN e:ss) = do
    v <- evalExprToRValue e
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


evalBuiltin :: BuiltinFunc -> StateT ContextFrames IO(Maybe RValueNode)
evalBuiltin PRINT = do 
    p <- hoist $ getVar "toPrint"
    case p of
        Just v -> do
            -- liftIO $ putStr $ "<"++(show v)++">" -- for debuging purpose
            liftIO $ putStr (show (toFloat v)) -- change this to toStr when string will be a thing for now floats are printable so ok
            return Nothing
        Nothing -> error "Print function didnt got it's argument, weird.."
evalBuiltin LEN = do
    p <- hoist $ getVar "array"
    case p of
        Just v -> do
            return $ Just $ INT_VAL $ length $ toArray v
        Nothing -> error "Len function didnt got it's argument, weird.."

evalStatement :: (StatementNode) -> StateT ContextFrames IO()
evalStatement (N_BLOCK ss) = do
    hoist $ addContextFrame
    _ <- evalBlock ss
    hoist $ popContextFrame
    return ()
evalStatement (N_FUNC_DEF s args b) = do
    -- context will by filled when retrived with getVar
    hoist $ assignValueToVar s (CALLABLE args b [])
    return ()
evalStatement (N_FUNC_CALL_WRAPPER (N_FUNC_CALL e args)) = do
    _ <- callFunc e args
    return ()
evalStatement (N_FUNC_CALL_WRAPPER _) = error "You writed a statement like a[0], didn't you ? what am i suppose to do with this value just disdard it ?? no i throw an error and you delete this statement if you want it to work"
evalStatement (N_ASSIGN l r) = do
    ll <- evalExprToLValue l
    rr <- evalExprToRValue r
    hoist $ assignValueToVar ll rr
evalStatement (N_IF c t Nothing) = do
    ec <- evalExprToRValue c
    when (toBool ec) ( do
        hoist addContextFrame
        evalStatement t
        hoist popContextFrame
        )
evalStatement (N_IF c t (Just e)) = do
    ec <- evalExprToRValue c
    if (toBool ec) 
    then evalStatement t
    else evalStatement e
evalStatement EOF = return ()


-- Type conversion
data Number = FLOAT_NUM Float | INT_NUM Int

toNumber :: RValueNode -> Number
toNumber (INT_VAL v) = INT_NUM v
toNumber (FLOAT_VAL v) = FLOAT_NUM v
toNumber (BOOL_VAL v) = if v then INT_NUM 1 else INT_NUM 0
toNumber (CALLABLE _ _ _) = error "Can convert function to number"

toInt :: RValueNode -> Int
toInt (INT_VAL v) = v
toInt (FLOAT_VAL v) = round v -- COERCE FLOAT TO INT BY ITSELF THROW AN ERROR INSTEAD IF YOU WHICH IT COULDN'T BE DONE
toInt (BOOL_VAL v) = if v then 1 else 0
toInt (CALLABLE _ _ _) = error "Can convert function to int"

toFloat :: RValueNode -> Float
toFloat (INT_VAL v) = fromIntegral v
toFloat (FLOAT_VAL v) = v
toFloat (BOOL_VAL v) = if v then 1.0 else 0.0
toFloat (CALLABLE _ _ _) = error "Can convert function to float"

toBool :: RValueNode -> Bool
toBool (INT_VAL v) =  if v /= 0 then True else False
toBool (FLOAT_VAL v) =  if v /= 0.0 then True else False
toBool (BOOL_VAL v) =  v
toBool (CALLABLE _ _ _) = error "Can convert function to bool"

toArray :: RValueNode -> [RValueNode]
toArray (ARRAY_VAL v) = v
toArray _ = error "Can't convert to array"

opeOnNum :: (Float -> Float -> Float) -> (Int -> Int -> Int) -> ExprNode -> ExprNode -> StateT ContextFrames IO(RValueNode)
opeOnNum floatOpe intOpe l r = do
    ll <- evalExprToRValue l
    rr <- evalExprToRValue r
    case toNumber ll of
        FLOAT_NUM f -> return $ FLOAT_VAL (floatOpe f (toFloat rr))
        INT_NUM i -> case toNumber rr of
            FLOAT_NUM f -> return $ FLOAT_VAL (floatOpe (fromIntegral i) f)
            INT_NUM ii -> return $ INT_VAL (intOpe i ii)

-- could have just toFloat and then do ope because ven when comparing interger if we convert to bool it is the same result
opeOnNumToBool :: (Float -> Float -> Bool) -> (Int -> Int -> Bool) -> ExprNode -> ExprNode -> StateT ContextFrames IO(RValueNode)
opeOnNumToBool fOpe iOpe l r = do
    ll <- evalExprToRValue l
    rr <- evalExprToRValue r
    case toNumber ll of
        FLOAT_NUM f -> return $ BOOL_VAL (fOpe f (toFloat rr))
        INT_NUM i -> case toNumber rr of
            FLOAT_NUM f -> return $ BOOL_VAL (fOpe (fromIntegral i) f)
            INT_NUM ii -> return $ BOOL_VAL (iOpe i ii)

evalExprToRValue :: ExprNode -> StateT ContextFrames IO(RValueNode)
evalExprToRValue (N_NEG n) = do
    e <- evalExprToRValue n
    case toNumber e of
        INT_NUM i -> return $ INT_VAL (-i)
        FLOAT_NUM f -> return $ FLOAT_VAL (-f)
evalExprToRValue (N_ADD l r) = opeOnNum (+) (+) l r
evalExprToRValue (N_SUB l r) = opeOnNum (-) (-) l r
evalExprToRValue (N_MUL l r) = opeOnNum (*) (*) l r
evalExprToRValue (N_DIV l r) = opeOnNum floatSafeDiv intSafeDiv l r
evalExprToRValue (N_EQU l r) = do
    ll <- evalExprToRValue l
    rr <- evalExprToRValue r
    return $ BOOL_VAL (ll == rr)
evalExprToRValue (N_GTH l r) = opeOnNumToBool (>) (>) l r
evalExprToRValue (N_LTH l r) = opeOnNumToBool (<) (<) l r
evalExprToRValue (N_GOREQU l r) = opeOnNumToBool (>=) (>=) l r
evalExprToRValue (N_LOREQU l r) = opeOnNumToBool (<=) (<=) l r
evalExprToRValue (N_CONST_VAL v) = return v
evalExprToRValue (N_ARRAY_LITERAL es) = do
    vs <- traverse evalExprToRValue es 
    return $ ARRAY_VAL vs
evalExprToRValue (N_INDEXING e i) = do
    ee <- evalExprToRValue e
    ii <- evalExprToRValue i
    case ee of
        ARRAY_VAL vs -> let index = toInt ii
            in if index < length vs
                then return $ vs !! index
                else error "Index out of range"
        otherwise -> error "error with the index"
evalExprToRValue (N_CONCAT l r) = do
    ll <- evalExprToRValue l
    rr <- evalExprToRValue r
    return $ ARRAY_VAL ((toArray ll) ++ (toArray rr))
evalExprToRValue (N_VAR_VAL s) = do
    v <- hoist $ getVar s
    case v of
        Nothing -> error ("Variable " ++ s ++ " not found")
        Just n -> return n
evalExprToRValue (N_FUNC_CALL e args) = do
    r <- callFunc e args
    case r of
        Just e -> return e
        Nothing -> error "A function in an expression didn't return any value"
evalExprToRValue (N_BLOCK_WRAPPER (N_BLOCK ss)) = do
    ret <- evalBlock ss
    case ret of
        Just e -> return e
        Nothing -> error "Block in an expression should return a value"
evalExprToRValue (N_BLOCK_WRAPPER _) = error "Statement in N_BLOCK_WRAPPER isn't a block, weird..."

appendIndex :: LValueNode -> RValueNode -> LValueNode
appendIndex (LValueNode s i) a = LValueNode s (i++[a])

evalExprToLValue :: ExprNode -> StateT ContextFrames IO(LValueNode)
evalExprToLValue (N_VAR_VAL s) = return $ LValueNode s []
evalExprToLValue (N_INDEXING e i) = do
    r <- evalExprToLValue e
    index <- evalExprToRValue i
    let new = appendIndex r index
    return new


nextToken :: State [Token] Token
nextToken = do
    ts <- get
    if not (null ts) 
    then let (t:tr) = ts
        in do
        put tr
        return t
    else return EOF_TOK

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
                (expectToken COMA_TOK $ "Missing a coma between arguments debug : ")
            arg <- parseE
            parseArgsFuncCall (prev_arg++[arg])

parseArrayLiteral :: [ExprNode] -> State [Token] [ExprNode]
parseArrayLiteral prev_arg = do
    c <- acceptToken CSQUARY_TOK
    if c
        then return prev_arg
        else do
            when (not $ null prev_arg) 
                (expectToken COMA_TOK $ "Missing a coma between arguments debug : ")
            arg <- parseE
            parseArrayLiteral (prev_arg++[arg])
    
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

-- { S+ } | S
parseB :: [StatementNode] -> State [Token] StatementNode
parseB prev_s = do
    c <- acceptToken CCURLY_TOK
    if c
        then return (N_BLOCK prev_s)
    else do 
        s <- parseS
        parseB (prev_s++[s])

-- IF E B (ELSE B)? | my_func(E (, E)*) | IDEN = E
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
                    return N_FUNC_DEF {funcName = LValueNode s [], argsName = arguments, body = bdy}
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
            suf <- parseC (N_VAR_VAL s)
            c <- acceptToken EQU_TOK
            if c
                then do
                    value <- parseE
                    return N_ASSIGN { varExpr = suf, val = value}
                else return $ N_FUNC_CALL_WRAPPER suf
        EOF_TOK -> return EOF
        otherwise -> do
            debug <- get
            error (show (debug ++ [t]))

parse :: State [Token] ExprNode -> [(Token, (ExprNode -> ExprNode -> ExprNode))] -> State [Token] ExprNode
parse sub_p tn = do
    e <- sub_p
    parse' sub_p tn e

                    
parse' :: State [Token] ExprNode -> [(Token, (ExprNode -> ExprNode -> ExprNode))] -> ExprNode -> State [Token] ExprNode
parse' _ [] ln = return ln
parse' sub_p ((t, n):r) ln = do
    c <- acceptToken t
    if c
        then do
            rn <- sub_p
            e <- parse' sub_p ((t,n):r) (n ln rn)
            return e
        else parse' sub_p r ln

-- E is for + or -
-- E -> F E'
parseE :: State [Token] ExprNode
parseE = parse parseZ [(CONCAT_TOK, N_CONCAT)]
parseZ = parse parseA [(EQU_TOK, N_EQU), (LTH_TOK, N_LTH), (GTH_TOK, N_GTH), (GOREQU_TOK, N_GOREQU), (LOREQU_TOK, N_LOREQU)]
parseA = parse parseF [(PLUS_TOK, N_ADD), (MINUS_TOK, N_SUB)]
parseF = parse parseT [(STAR_TOK, N_MUL), (SLASH_TOK, N_DIV)]

parseT :: State [Token] ExprNode
parseT = do
    e <- parseT'
    ret <- parseC e
    return ret

-- need C to be executed before every return  and c will parse func call and indexing
-- T -> 42 | 3.14 | true | [2+2, 3.14] | my_var_name C? | (E) | -E | { S+ RETURN E }
parseT' :: State [Token] ExprNode
parseT' = do
    t <- nextToken
    case t of
        OSQUARY_TOK -> do
            ret <- parseArrayLiteral []
            return $ N_ARRAY_LITERAL ret
        IDEN_TOK s -> return $ N_VAR_VAL s
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

-- C is for expression suffix like (args1 ,arg2) or [12] and accept multiple of them chained
-- C -> (args, ...) C?
parseC :: ExprNode -> State [Token] ExprNode
parseC e = do
    c <- acceptToken OPAR_TOK
    if c
        then do
            args <- parseArgsFuncCall []
            ret <- parseC (N_FUNC_CALL e args)
            return ret
        else do
                cc <- acceptToken OSQUARY_TOK
                if cc
                    then do
                        i <- parseE
                        expectToken CSQUARY_TOK "Missing closing squary bracket"
                        ret <- parseC (N_INDEXING e i)
                        return ret
                    else return e
        
main :: IO()
main = do
    contents <- getContents
    let (tokens, _) = tokenize [] contents
    -- printTokens tokens
    let ast = evalState parseS ([OCURLY_TOK] ++ tokens ++ [CCURLY_TOK])
    -- putStr $ show ast
    _ <- runStateT (evalStatement ast) [Map.fromList [("print", (CALLABLE ["toPrint"] (N_BUILTIN_FUNC PRINT) [])), ("len", (CALLABLE ["array"] (N_BUILTIN_FUNC LEN) []))]]
    return ()



