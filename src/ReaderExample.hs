import Control.Monad.Reader
import Data.List

data Source = Keyword String
            | Block [Source]
            deriving (Show)

data Config = Config {
      open_paren :: String,
      close_paren :: String,
      current_indent :: Int,
      indent :: Int
    }

spaces :: Int -> String
spaces n = replicate n ' '

do_indent :: String -> Reader Config String
do_indent s = do Config {current_indent=k} <- ask
                 return $ spaces k ++ s

increase_indent :: Config -> Config
increase_indent cfg = cfg { current_indent = current_indent cfg + indent cfg }

pretty :: Source -> Reader Config String
pretty (Keyword k) = do_indent k
                        
pretty (Block b) = do Config {open_paren=op, close_paren=cl} <- ask
                      b_pretty <- local increase_indent (mapM pretty b)
                      op' <- do_indent op
                      cl' <- do_indent cl
                      return $ intercalate "\n" ([op'] ++ b_pretty ++ [cl'])

example = Block [
           Keyword "if", 
           Block [
            Keyword "then_happens_here", 
            Keyword "and_more_happened"
           ],
           Keyword "else",
           Block []
          ]

default_cfg = Config { open_paren = "{", close_paren = "}", current_indent = 2, indent = 4 }

example_pretty :: String
example_pretty = runReader (pretty example) default_cfg
