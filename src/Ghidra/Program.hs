{-# LANGUAGE DataKinds #-}
module Ghidra.Program where

import Ghidra.Prelude hiding (force)

import qualified Language.Java as Java
import Ghidra.Types
import qualified Foreign.JNI as JNI

getAddressFactory :: ProgramDB -> IO AddressFactory
getAddressFactory p = Java.call p "getAddressFactory" >>= JNI.newGlobalRef

-- | Returns an address from the constant space of the program
getConstantAddress :: AddressFactory -> Word64 -> IO Address
getConstantAddress gen off = Java.call gen "getConstantAddress" (fromIntegral off :: Int64) >>= JNI.newGlobalRef
