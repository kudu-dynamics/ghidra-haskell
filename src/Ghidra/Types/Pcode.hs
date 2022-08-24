{-# LANGUAGE DataKinds #-}
module Ghidra.Types.Pcode where

import Ghidra.Prelude hiding (toList)

import Ghidra.Types (VarNode)
import qualified Ghidra.Types as J


data BarePcodeInstruction = BarePcodeInstruction
  { op :: BarePcodeOp
  , output :: Maybe J.VarNode
  , inputs :: [J.VarNode]
  } deriving (Eq, Show, Generic)

data BarePcodeOp
  = BOOL_AND
  | BOOL_NEGATE
  | BOOL_OR
  | BOOL_XOR
  | BRANCH
  | BRANCHIND
  | CALL
  | CALLIND
  | CALLOTHER
  | CAST
  | CBRANCH
  | COPY
  | CPOOLREF
  | EXTRACT
  | FLOAT_ABS
  | FLOAT_ADD
  | FLOAT_CEIL
  | FLOAT_DIV
  | FLOAT_EQUAL
  | FLOAT_FLOAT2FLOAT
  | FLOAT_FLOOR
  | FLOAT_INT2FLOAT
  | FLOAT_LESS
  | FLOAT_LESSEQUAL
  | FLOAT_MULT
  | FLOAT_NAN
  | FLOAT_NEG
  | FLOAT_NOTEQUAL
  | FLOAT_ROUND
  | FLOAT_SQRT
  | FLOAT_SUB
  | FLOAT_TRUNC
  | INDIRECT
  | INSERT
  | INT_2COMP
  | INT_ADD
  | INT_AND
  | INT_CARRY
  | INT_DIV
  | INT_EQUAL
  | INT_LEFT
  | INT_LESS
  | INT_LESSEQUAL
  | INT_MULT
  | INT_NEGATE
  | INT_NOTEQUAL
  | INT_OR
  | INT_REM
  | INT_RIGHT
  | INT_SBORROW
  | INT_SCARRY
  | INT_SDIV
  | INT_SEXT
  | INT_SLESS
  | INT_SLESSEQUAL
  | INT_SREM
  | INT_SRIGHT
  | INT_SUB
  | INT_XOR
  | INT_ZEXT
  | LOAD
  | MULTIEQUAL
  | NEW
  | PCODE_MAX
  | PIECE
  | POPCOUNT
  | PTRADD
  | PTRSUB
  | RETURN
  | SEGMENTOP
  | STORE
  | SUBPIECE
  | UNIMPLEMENTED
  deriving (Eq, Ord, Read, Show, Generic)
