{-# LANGUAGE DataKinds #-}
module Ghidra.Instruction
  ( module Ghidra.Instruction
  , J.Instruction
  ) where

import Ghidra.Prelude hiding (toList)

import Language.Clojure
import System.IO.Memoize (once)
import Ghidra.State (GhidraState)
import qualified Ghidra.State as State
import qualified Language.Java as Java
import qualified Ghidra.Types as J
import Ghidra.Util (maybeNull)
import Ghidra.Types (toAddrSet)

requireModule :: IO ()
requireModule = unsafePerformIO . once $ do
  _ <- readEval "(require (quote [ghidra-clojure.function]))"
  return ()

fromAddr :: GhidraState -> J.Address -> IO (Maybe J.Instruction)
fromAddr gs addr = do
  prg <- State.getProgram gs
  listing :: J.Listing <- Java.call prg "getListing"
  maybeNull <$> Java.call listing "getInstructionContaining" addr
  
getInstructions :: J.Addressable a => GhidraState -> a -> IO [J.Instruction]
getInstructions gs x = do
  prg <- State.getProgram gs
  listing :: J.Listing <- Java.call prg "getListing"
  addrSet <- toAddrSet x
  instrsIterator :: J.InstructionIterator <- Java.call listing "getInstructions" addrSet True
  instructionIteratorToList instrsIterator
  
instructionIteratorToList :: J.InstructionIterator -> IO [J.Instruction]
instructionIteratorToList x = do
  hasNext :: Bool <- Java.call x "hasNext"
  if hasNext
    then do
      ref <- Java.call x "next"
      (ref:) <$> instructionIteratorToList x
    else return []
