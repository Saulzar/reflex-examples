{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Control.Monad.Trans.State
import Control.Monad.Trans.Free
import qualified Data.Map as Map


  
listChoice :: MonadWidget t m => [String] -> m (Event t Int)
listChoice choices = do 
  clicks <- forM (zip [1..] choices) button'  
  return (leftmost clicks)
  
  where
    button' (i, str) = fmap (const i) <$> button str 
  
  
data InteractionF x = GetInput [String] (Int -> x)

instance Functor InteractionF where
  fmap f (GetInput s k) = GetInput s (f . k)

type Interaction m a = FreeT InteractionF m a

data MyState = MyState Int

type ProgM a = StateT MyState (FreeT InteractionF Identity) a

getInput :: [String] -> ProgM Int
getInput options = do
  st <- get
  ret <- liftF $ GetInput options id
  if ret < length options && ret >= 0
    then return ret
    else getInput options

-- a sample value of type ProgM ()
prog :: ProgM ()
prog = do
  x <- getInput ["a", "b"]
  MyState y <- get
  when (x == 1 || y == 2) $ (getInput ["c", "d", "e"]) >> return ()
  put (MyState x)
  return ()

initial_state :: MyState
initial_state = MyState 432435

prog' :: Interaction Identity ()
prog' = void $ execStateT prog initial_state


once :: MonadWidget t m => a -> m (Event t a)
once a = fmap (const a) <$> getPostBuild

-- Split Either into two event streams
splitEither :: (Reflex t) => Event t (Either a b) -> (Event t a, Event t b)
splitEither e = (fmapMaybe (firstOf _Left) e, fmapMaybe (firstOf _Right) e)

interpret :: MonadWidget t m => Interaction Identity r -> m (Event t (Either r (Interaction Identity r)))
interpret p = case runIdentity (runFreeT p) of
  
  (Pure r) -> once (Left r)
  
  Free (GetInput strs select) -> do
    choice <- listChoice strs
    return (Right . select <$> choice)
    

    
example :: MonadWidget t m => m ()
example = do
  rec 
    (done, continue) <- splitEither <$> (dyn cont >>= switchPromptly never)  
    cont <- holdDyn prog' continue >>= mapDyn interpret
    
    performEvent_ $ ffor done $ \() -> liftIO $ putStrLn "Done!" 
  return ()

main :: IO ()
main = mainWidget $ el "div" $ example
