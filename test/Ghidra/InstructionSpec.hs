module Ghidra.InstructionSpec where

import Ghidra.Prelude

import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Instruction (getInstructions)
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.Instruction" $ do
  gs <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ diveBin >>! State.analyze
    return gs

  context "getInstructions" $ do
    let cgc_printf_addr = 0x804c6e0
    refs <- runIO . runGhidraOrError $ do
      addr <- State.mkAddress gs cgc_printf_addr
      mfunc <- Function.fromAddr gs addr
      case mfunc of
        Nothing -> error "Couldn't find cgc_printf."
        Just func -> do
          -- addr <- Program.mkAddress prg cgc_printf_addr
          getInstructions gs func
    it "should get instructions for cgc_printf" $ do
      length refs `shouldBe` 18
