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
spaccum :: (a -> p)
              -- ^ Create the initial partial accumulation @p@.
           -> (p -> a -> Either p (g, Maybe a))
              -- ^ Attempt to add a new value to a partial
              -- accumulation; returns either an updated partial
              -- accumulation or else a completed accumulation @g@ as
              -- well as optionally the initial value (if it was /not/
              -- added to the completed accumulation).
           -> (p -> Maybe g)
              -- ^ Attempt to convert the final partial accumulation
              -- to a complete accumulation.  If this function returns
              -- @'Nothing'@ then the final partial accumulation is
              -- returned using 'spfailed'.
           -> SP a g m p
spaccum mkInit addA finalise = createNewAccum
  where
    createNewAccum = spconsume newAccumFrom spcomplete

    newAccumFrom = accumPartial . mkInit

    accumPartial p = spconsume (withValue . addA p) (finalisePartial p)

    withValue epa = case epa of
                      Left p        -> accumPartial p
                      Right (g,ma') -> produce g
                                               (const (maybe createNewAccum newAccumFrom ma'))
                                               spincomplete

    finalisePartial p = maybe (spfailed p) spemit (finalise p)
