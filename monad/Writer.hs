-- Example of Writer monad

module Writer where

import Control.Monad.Writer (listen, runWriter, tell)

test :: (((), [String]), [String])
test = runWriter $ do
  tell ["start"]
  (a, w) <- listen $ tell ["foo"]
  tell ["end"]
  return (a, w)
