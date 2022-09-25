{-# LANGUAGE Safe, CPP, MagicHash, FlexibleInstances #-}

{- |
    Module      :  Control.DeepSeq.SDP
    Copyright   :  (c) Andrey Mulik 2019-2022
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Control.DeepSeq.SDP@ contains 'NFData' instances for @sdp@ types.
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

instance NFData e => NFData (SArray# e)
  where
#if MIN_VERSION_sdp(0,3,0)
    rnf = sfoldr' deepseq ()
#else
    rnf = o_foldr' deepseq ()
#endif

instance NFData e => NFData (SBytes# e) where rnf = flip seq ()

instance (NFData i, NFData (rep e)) => NFData (AnyBorder rep i e)
  where
    rnf (AnyBorder l u rep) = l `deepseq` u `deepseq` rnf rep

instance NFData (rep e) => NFData (AnyChunks rep e)
  where
    rnf = foldr' deepseq () . toChunks

--------------------------------------------------------------------------------

{- NFData1 and NFData2 instances. -}

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 SArray#
  where
#if MIN_VERSION_sdp(0,3,0)
    liftRnf rnf' = sfoldr' (seq . rnf') ()
#else
    liftRnf rnf' = o_foldr' (seq . rnf') ()
#endif

instance (NFData i, NFData1 rep) => NFData1 (AnyBorder rep i)
  where
    liftRnf rnf' (AnyBorder l u rep) = l `deepseq` u `deepseq` liftRnf rnf' rep

instance NFData1 rep => NFData1 (AnyChunks rep)
  where
    liftRnf rnf' = foldr' (seq . liftRnf rnf') () . toChunks

instance NFData1 rep => NFData2 (AnyBorder rep)
  where
    liftRnf2 rnf' rnf'' (AnyBorder l u rep) = rnf' l `seq` rnf' u `seq` liftRnf rnf'' rep
#endif



