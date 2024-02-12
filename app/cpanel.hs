import MyXmonad.CPanel qualified as CP
import System.Environment (getArgs)

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    ["blup"] -> CP.doBacklight' '+'
    ["bldown"] -> CP.doBacklight' '-'
    _ -> return "bad args"
    >>= putStrLn
