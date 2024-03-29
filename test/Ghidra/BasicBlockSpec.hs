module Ghidra.BasicBlockSpec where

import Ghidra.Prelude

import Ghidra.BasicBlock
import Ghidra.Types.BasicBlock (BasicBlockGraph(BasicBlockGraph))
import qualified Ghidra.Types.BasicBlock as BB
import qualified Ghidra.State as State
import qualified Ghidra.Function as Function
import Ghidra.Core
import Test.Hspec


diveBin :: FilePath
diveBin = "res/test_bins/Dive_Logger/Dive_Logger.gzf"

a1Bin :: FilePath
a1Bin = "res/test_bins/a1/a1.gzf"

spec :: Spec
spec = describe "Ghidra.BasicBlock" $ do
  (gs, jfunc, _highJfunc) <- runIO . runGhidraOrError $ do
    gs <- State.openDatabase_ a1Bin >>! State.analyze
    -- b <- isNil' $ gs ^. #unGhidraState
    -- when b $ error "Couldn't open a1"
    let jaddr = 0x1348
    jaddr' <- State.mkAddressBased gs jaddr
    (Just jfunc) <- Function.fromAddr gs jaddr'
    highJfunc <- Function.getHighFunction gs jfunc
    return (gs, jfunc, highJfunc)

  context "getCodeBlocks" $ do
    blocks <- runIO . runGhidraOrError $ do
      getCodeBlocks gs jfunc

    it "should get 4 blocks for j function" $ do
      length blocks `shouldBe` 4

  context "getBasicBlockGraph" $ do
    g <- runIO . runGhidraOrError $ do
      g <- getBasicBlockGraph gs jfunc
      -- g' :: BasicBlockGraph Address <- traverse (view #startAddress) g
      return $ view (#startAddress . #offset) <$> g
    let expected = BasicBlockGraph
          { BB.nodes = [1053512, 1053557, 1053592, 1053598]
          , BB.edges = [(1053512, 1053592), (1053557, 1053592), (1053592, 1053557), (1053592, 1053598)]
          }
    it "should generate basic block graph" $ do
      g `shouldBe` expected
