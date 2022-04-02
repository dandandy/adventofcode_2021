module MaybeTExample (example) where

import Control.Monad.Cont (lift, liftIO, guard)
import qualified Control.Monad.Identity as Monad
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Text.Read (readMaybe, readEither)
import Text.Parsec (Parsec, digit, many1, ParsecT, runParser, anyChar)
import Control.Monad.Identity (Identity)
import Data.Maybe (fromMaybe)
import Control.Monad.Error.Class (liftEither)
-- import Text.Read

-- aaaa :: IO (Maybe a)
aaaa :: MaybeT IO Int
aaaa = MaybeT $ readMaybe <$> liftIO getLine
-- aaaa = liftIO readMaybe <$> getLine 

b :: MaybeT IO (Int, Int)
b = do  a <- aaaa
        c <- aaaa
        return (a,c)

aaaaIO :: IO ()
aaaaIO = do x <- runMaybeT aaaa
            print x

eParser ::Parsec String u Int -- Uses Fail Monad typeclass
eParser = do d <- many1 anyChar 
             case readEither d :: Either String Int of
               Left a -> fail $ a <> ": " <> show d
               Right a -> return a

example :: IO ()
example = do let x = runParser eParser  () "" "a"
             print x