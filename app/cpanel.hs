import MyXmonad.CPanel
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  arg <- getArgs
  case arg of
    ["blup"] -> blup def
    ["bldown"] -> bldown def
    -- ["volmute"] -> volmute def
    -- ["volup"] -> volup def
    -- ["voldown"] -> voldown def
    _ -> die "bad args"
