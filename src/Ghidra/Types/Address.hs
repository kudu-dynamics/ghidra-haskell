module Ghidra.Types.Address
  ( module Ghidra.Types.Address
  , module Exports
  ) where

import Ghidra.Prelude
import Data.BinaryAnalysis as Exports (AddressSpace(..), Address(..))

type AddressSpaceMap = HashMap AddressSpaceId AddressSpace

newtype AddressSpaceId = AddressSpaceId Int32
  deriving (Generic)
  deriving newtype (Eq, Ord, Read, Show, Num, Hashable)

