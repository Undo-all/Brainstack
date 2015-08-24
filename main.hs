import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except
import Data.Char
import Text.Read (readMaybe)

data CompilerState = CompilerState
                   { cellPtr :: Int
                   , stackPtr :: Int
                   , tempCells :: [Int]
                   }

type Compiler = StateT CompilerState (WriterT String (Except String)) ()

runCompiler :: Compiler -> Either String String
runCompiler = runExcept . execWriterT . flip evalStateT (CompilerState 0 0 [])

moveTo :: Int -> Compiler
moveTo to | to < 0 = throwError "attempt to move to negative memory location"
moveTo to = 
    do cp <- gets cellPtr
       tell $ replicate (abs $ to - cp)
                        (if to > cp then '>' else '<')
       modify $ \x -> x { cellPtr = to }

guideTo :: Int -> Compiler
guideTo to = 
    do tcs <- gets tempCells
       if to `elem` tcs 
         then guideTo (to - 1)
         else moveTo to

guideToOffset :: Int -> Compiler
guideToOffset offset =
    do sp <- gets stackPtr
       guideTo (sp - (1 + offset))

zero :: Int -> Compiler
zero cell =
    do moveTo cell
       tell "[-]"

assign :: Int -> Int -> Compiler
assign cell val = 
    do moveTo cell
       tell $ replicate (abs val)
                        (if val > 0 then '+' else '-')

push :: Int -> Compiler 
push val = 
    do sp <- gets stackPtr
       zero sp 
       assign sp val
       modify $ \x -> x { stackPtr = sp + 1 } 

pushTemp :: Int -> Compiler
pushTemp val = 
    do push val
       modify $ \x -> x { tempCells = (stackPtr x - 1) : tempCells x }

pop' :: Compiler
pop' = 
    do sp <- gets stackPtr
       when (sp <= 0) $ throwError "attempt to pop from empty stack"
       modify $ \x -> x { stackPtr = sp - 1 }

pop :: Compiler
pop = 
    do pop'
       sp <- gets stackPtr
       tcs <- gets tempCells
       when ((sp - 1) `elem` tcs) pop

data Value = Int Int
           | Char Char deriving (Eq, Show)

data Ins = Drop
         | Dup
         | Swap
         | Over
         | Rot 
         | Inc
         | Dec
         | Add 
         | Sub 
         | Mul 
         | RawOutput deriving (Eq, Show)

data Expr = Literal Value
          | Instruction Ins
          | IfElse [Expr] [Expr]
          | When [Expr]
          | While [Expr] deriving (Eq, Show)

compileValue :: Value -> Compiler
compileValue (Int n) = push n
compileValue (Char c) = push (ord c)

translate :: String -> Compiler
translate = fmap last . mapM getIns . words
  where getIns "v"      = pop
        getIns ('^':xs) = case readMaybe xs :: Maybe Int of
                            Just n -> push n
                            Nothing -> tell ('^':xs)
        getIns xs = case readMaybe xs :: Maybe Int of
                      Just n -> guideToOffset n
                      Nothing -> tell xs

compileIns :: Ins -> Compiler
compileIns Drop = pop
compileIns Dup = 
    translate "^0 ^0 2 [ 1 + 0 + -] 0 [ 2 + 0 -] v"
compileIns Swap = 
    translate "^0 1 [ 0 + 1 -] 2 [ 1 + 2 -] 0 [ 2 + 0 - ] v"
compileIns RawOutput = translate "0 ."
compileIns Inc = 
    translate "0 +"
compileIns Dec = 
    translate "0 -"
compileIns Add = 
    translate "0 [ 1 + 0 -] v"
compileIns Sub = 
    translate "0 [ 1 - 0 -] v"
compileIns Mul = 
    translate "^0 ^0 3 [ 1 + 3 -] 1 [ 2 [ 3 + 0 + 2 -] 0 [ 2 + 0 - ] 1 -] v v v"

compileIfElse = undefined
compileWhen = undefined
compileWhile = undefined

compileExpr :: Expr -> Compiler
compileExpr (Literal val)     = compileValue val
compileExpr (Instruction ins) = compileIns ins
compileExpr (IfElse xs ys)    = compileIfElse xs ys
compileExpr (When xs)         = compileWhen xs
compileExpr (While xs)        = compileWhile xs

compileExprs :: [Expr] -> Either String String
compileExprs xs = runCompiler $ last <$> mapM compileExpr xs 

main = do
    either putStrLn putStrLn $ compileExprs [ Literal $ Int 10 
                                            , Literal $ Int 10
                                            , Instruction Mul
                                            , Instruction RawOutput
                                            ]

