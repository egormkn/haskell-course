module Hw3 where

import Control.Exception (Exception, throw)
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Cont

import Text.Megaparsec.Char.Lexer as Lex
import Text.Megaparsec as Meg
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

import Data.Foldable as Fold
import Data.Void as Void
import Data.Map.Lazy as Map


-- Var "x" `Add` (Lit 3 `Mul` ("x" `Let` (Lit 2) $ Var "x"))
data Expr = Lit Int
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr
  deriving Show

data EvaluateException = DivideByZero
    | UndefinedVariable String
    | InvisibleVariable String
    | ExistingVariable String
    deriving Show

instance Exception EvaluateException

data RunState = Normal | BreakState

data Statement = Define String Expr
    | Update String Expr
    | Read String
    | Write Expr
    | Pure Expr
    | For String Expr Expr [Statement]
    | Break
    | If Expr String Expr [Statement]
    deriving Show

newtype Script = Script [Statement] deriving Show
type Parser = Meg.Parsec Void.Void String

reservedWords :: [String]
reservedWords = ["let", "in", "=", "mut", 
                 "for", "{", "}", ";", "break", "until", 
                 "<<", ">>", "if"]

spaces :: Parser ()
spaces = Lex.space space1 (Lex.skipLineComment "//") (Lex.skipBlockComment "/*" "*/")

readSymbol :: String -> Parser String
readSymbol a = Meg.try $ Lex.symbol spaces a

arithmeticOperators :: [[Operator Parser Expr]]
arithmeticOperators = [[InfixL (Mul <$ readSymbol "*"), InfixL (Div <$ readSymbol "/")],
                       [InfixL (Add <$ readSymbol "+"), InfixL (Sub <$ readSymbol "-")]]

parseScriptFromSrc :: Parser Script
parseScriptFromSrc = Meg.try $ 
    Script <$> Meg.between spaces Meg.eof (Meg.many _parseStatements)
    where
        {- 
            May be it was better to do as class bacause
            this id done for incapsulation of support methods
        -}

        _readWord :: String -> Parser ()
        _readWord str = (_readLexeme . Meg.try) 
                        (string str *> Meg.notFollowedBy alphaNumChar)

        _readLexeme :: Parser a -> Parser a
        _readLexeme = Lex.lexeme spaces

        _parseWord :: Parser String
        _parseWord = (:) <$> letterChar <*> Meg.many alphaNumChar

        _readIdentifier :: Parser String
        _readIdentifier = Meg.try $ (_readLexeme . Meg.try) (_parseWord >>= _isReserved)
            where
                _isReserved :: String -> Parser String
                _isReserved name = if name `elem` reservedWords 
                                   then fail $ "keyword " ++ show name ++ " cannot be an identifier"
                                   else return name

        _parseStatements :: Parser Statement
        _parseStatements = Meg.try _parseFOR Meg.<|> _parseIF Meg.<|> _parseOPERAITON

        _parseOPERAITON :: Parser Statement
        _parseOPERAITON = Meg.try $ Fold.foldr1 (Meg.<|>) (Prelude.map (Meg.try . _parseSEMI) 
            [_parseDEFINE, _parseUPDATE, _parseREAD, _parseWRITE, _parseBREAK, _parsePURE])
            where
                _parseSEMI a = a >>= \b -> Meg.try $ readSymbol ";" >> return b

        _parseFOR :: Parser Statement
        _parseFOR = do 
            _readWord "for"
            var <- _readIdentifier
            _readWord "="
            from <- _parseEXPR
            _readWord "to"
            to <- _parseEXPR
            _readWord "{"
            statements <- Meg.many _parseStatements
            _readWord "}"
            return (For var from to statements)

        _parseIF :: Parser Statement
        _parseIF = do 
            _readWord "if"
            left <- _parseEXPR
            cond <- _readIdentifier
            right <- _parseEXPR
            _readWord "{"
            statements <- Meg.many _parseStatements
            _readWord "}"
            return (If left cond right statements)

        _parseDEFINE :: Parser Statement
        _parseDEFINE = do 
            _readWord "mut"
            name <- _readIdentifier
            _readWord "="
            expr <- _parseEXPR
            return (Define name expr)

        _parseUPDATE :: Parser Statement
        _parseUPDATE = do 
            name <- _readIdentifier
            _readWord "="
            expr <- _parseEXPR
            return (Update name expr)

        _parseREAD :: Parser Statement
        _parseREAD = do 
            _readWord ">>"
            name <- _readIdentifier
            return (Read name)

        _parseWRITE :: Parser Statement
        _parseWRITE = do 
            _readWord "<<"
            expr <- _parseARITHMETICS
            return (Write expr)

        _parseBREAK :: Parser Statement
        _parseBREAK = Meg.try $ const Break <$> readSymbol "break"

        _parsePURE :: Parser Statement
        _parsePURE = Meg.try $ Pure <$> _parseEXPR

        _parseEXPR :: Parser Expr
        _parseEXPR = Fold.foldr1 (Meg.<|>)
            [_parseARITHMETICS, _parseVAR, _parseLIT, _parseLET, _parsePARENTHS _parseEXPR]

        _parseARITHMETICS :: Parser Expr
        _parseARITHMETICS = makeExprParser (Meg.try _parseAEXPR) arithmeticOperators
            where
                _parseAEXPR :: Parser Expr
                _parseAEXPR = Fold.foldr1 (Meg.<|>)
                    [_parseVAR, _parseLIT, _parseLET, _parsePARENTHS _parseEXPR]

        _parseVAR :: Parser Expr
        _parseVAR = Var <$> _readIdentifier

        _parseLIT :: Parser Expr
        _parseLIT = Lit <$> Meg.try (Lex.signed spaces (Meg.try $ _readLexeme Lex.decimal))

        _parseLET :: Parser Expr
        _parseLET = do 
            _readWord "let"
            name <- _readIdentifier
            _readWord "="
            sub <- _parseEXPR
            _readWord "in"
            expr <- _parseEXPR
            return (Let name sub expr)

        _parsePARENTHS :: Parser a -> Parser a
        _parsePARENTHS = Meg.between (readSymbol "(") (readSymbol ")")



        execute :: Expr -> Reader (Map String Int) Int
        execute (Lit lit) = return lit
        execute (Add f s) = liftM2 (+) (execute f) (execute s)
        execute (Sub f s) = liftM2 (-) (execute f) (execute s)
        execute (Mul f s) = liftM2 (*) (execute f) (execute s)
        execute (Div f s) = do
            f' <- execute f
            s' <- execute s
            case s' of
                0 -> throw DivideByZero
                _ -> return (div f' s')
        execute (Var name) = do
            look <- asks (Map.lookup name)
            case look of
                Just v' -> return v'
                Nothing -> throw $ UndefinedVariable name
        execute (Let name subst expression) = do
            v <- execute subst
            local (insert name v) (execute expression)
        
        executeStatements :: [Statement] -> ContT RunState (StateT (Map String Int) IO) ()
        executeStatements sts = let 
                setContext :: String -> Int -> (StateT (Map String Int) IO) ()
                setContext key value = do
                    contains <- gets (member key)
                    if contains then
                        throw $ ExistingVariable key
                    else modify (insert key value)
        
                updateContext :: String -> Int -> (StateT (Map String Int) IO) ()
                updateContext key value = do
                    contains <- gets (member key)
                    if contains then
                        modify (insert key value)
                    else throw $ InvisibleVariable key
                
                removeContext :: String -> (StateT (Map String Int) IO) ()
                removeContext key = do
                    contains <- gets (member key)
                    if contains then
                        modify (delete key)
                    else throw $ InvisibleVariable key
        
                eval :: Statement -> () -> ContT RunState (StateT (Map String Int) IO) ()
                eval st _ = ContT $ \f ->
                    case st of
                        Define name expr -> do
                            context <- get
                            setContext name $ runReader (execute expr) context
                            res <- f ()
                            removeContext name
                            return res
                        Update name expr -> do
                            context <- get
                            updateContext name $ runReader (execute expr) context
                            f ()
                        Read name -> do
                            readValue <- lift getLine
                            updateContext name (read readValue :: Int)
                            f ()
                        Write expr -> do
                            context <- get
                            (lift . putStrLn) $ show $ runReader (execute expr) context
                            f ()
                        Pure _ -> f ()
                        For name from to body -> do
                            context <- get
                            setContext name $ runReader (execute from) context
                            let loop = do loopContext <- get
                                          iterator <- gets (Map.! name)
                                          let loopTo = runReader (execute to) loopContext
                                          if iterator >= loopTo then
                                            return Normal
                                          else monad >>= \f' -> case f' of
                                            BreakState -> return Normal
                                            Normal -> gets (Map.! name) >>= \val ->
                                                updateContext name (val + 1) >> loop
                                          where monad = runContT (executeStatements body) $ const (return Normal)
                            _ <- loop
                            removeContext name
                            f ()
                        Break -> 
                            return BreakState
        
                        If left cond right body -> do
                            context <- get
                            let left' = runReader (execute left) context
                            let right' = runReader (execute right) context
                            let diff = left' - right'
                            if (cond == "eq" && diff == 0) || (cond == "gt" && diff > 0) 
                                || (cond == "lt" && diff < 0)
                            then runContT (executeStatements body) $ const (return Normal)
                            else f ()
            in Fold.foldl (>>=) (return ()) $ Prelude.map eval sts
        