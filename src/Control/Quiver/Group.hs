{-# LANGUAGE RankNTypes #-}
{- |
   Module      : Control.Quiver.Group
   Description : Group and chunk values within a Quiver
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Control.Quiver.Group where

import Control.Quiver.SP

--------------------------------------------------------------------------------

-- | Accumulate values within a Quiver.
spaccumM :: Monad m
           => (p -> a -> m (Maybe g, p))
              -- ^ Adds new value to a partial accumulation;
              -- returns an optional completed accumulation,
              -- and an updated partial accumulation.
           -> p
              -- ^ Initial partial accumulation @p@.
           -> SQ a g m (Maybe p)
spaccumM accum = loopc
  where
    loopc p = consume (loopd p) (deliver (Just p))
    loopd p x = do (mb, p') <- accum p x
                   maybe (loopc p') (\b -> produce b (const $ loopc p') (deliver Nothing)) mb
