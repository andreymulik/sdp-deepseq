{-# LANGUAGE Trustworthy, MagicHash, FlexibleInstances #-}

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

import Control.DeepSeq

import SDP.Array
import SDP.Bytes
import SDP.Unrolled
import SDP.ByteList

import SDP.Prim.SArray
import SDP.Prim.SBytes

default ()

--------------------------------------------------------------------------------

{- NFData instances. -}

instance (NFData e) => NFData (Unlist  e)
instance (NFData e) => NFData (Ublist  e)
instance (NFData e) => NFData (SArray# e) where rnf = rnf . listL
instance (NFData e) => NFData (SBytes# e) where rnf = rwhnf

instance (NFData i, NFData e) => NFData (Array i e)
instance (NFData i, NFData e) => NFData (Bytes i e)
instance (NFData i, NFData e) => NFData (Unrolled i e)
instance (NFData i, NFData e) => NFData (ByteList i e)




