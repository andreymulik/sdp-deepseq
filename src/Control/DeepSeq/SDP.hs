{-# LANGUAGE Safe, MagicHash, FlexibleInstances #-}

{- |
    Module      :  Control.DeepSeq.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Control.DeepSeq.SDP@ contains 'NFData' instances for SDP types.
-}
module Control.DeepSeq.SDP
(
  -- * Exports.
  module Control.DeepSeq
)
where

import Prelude ()
import SDP.SafePrelude

import SDP.Templates.AnyChunks
import SDP.Templates.AnyBorder
import SDP.Prim.SArray
import SDP.Prim.SBytes

import Control.DeepSeq

default ()

--------------------------------------------------------------------------------

{- NFData instances. -}

instance (NFData e) => NFData (SArray# e) where rnf = o_foldr' deepseq ()
instance (NFData e) => NFData (SBytes# e) where rnf = rwhnf

instance (NFData i, NFData (rep e)) => NFData (AnyBorder rep i e)
  where
    rnf (AnyBorder l u rep) = l `deepseq` u `deepseq` rnf rep

instance (NFData (rep e)) => NFData (AnyChunks rep e)
  where
    rnf = foldr' deepseq () . toChunks

