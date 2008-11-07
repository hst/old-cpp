{-# OPTIONS -fglasgow-exts -cpp #-}
module HST.CSPM.Parser
    (
     parseFile, parseExpr
    )
    where
import HST.CSPM.Types
import HST.CSPM.Lexer
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.18.1

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = GHC.Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn5 :: (CSPMScript) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (CSPMScript)
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (()) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (())
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (()) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (())
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: ([Definition]) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> ([Definition])
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Definition) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Definition)
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: (Identifier) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> (Identifier)
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: ([Identifier]) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> ([Identifier])
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([Expression]) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([Expression])
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: ([Expression]) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> ([Expression])
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([Expression]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([Expression])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: ([Expression]) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> ([Expression])
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Expression) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Expression)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Expression) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Expression)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Expression) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Expression)
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: (Expression) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (Expression)
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Expression) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Expression)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Expression) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Expression)
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Expression) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Expression)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Expression) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Expression)
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Binding]) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([Binding])
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Binding) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Binding)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x17\x00\x93\x00\x17\x00\x14\x00\x00\x00\x40\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x93\x00\x93\x00\x93\x00\x0d\x00\x93\x00\x93\x00\x00\x00\x93\x00\x0d\x00\x93\x00\x00\x00\x00\x00\x08\x00\x1b\x02\x3a\x00\xc8\xff\x00\x00\x9e\x00\x05\x00\x06\x00\x00\x00\x02\x00\x32\x00\x3f\x00\xa9\x01\x30\x00\x3d\x00\x3d\x01\x1f\x01\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\x93\x00\xf9\xff\x00\x00\xf4\xff\x00\x00\x97\x01\xe5\x01\x1b\x02\x03\x02\x03\x02\x03\x02\x03\x02\x2e\x00\x05\x00\x05\x00\x05\x00\x06\x00\x06\x00\x03\x02\x03\x02\x28\x00\x97\x01\x93\x00\x3c\x00\x00\x00\x93\x00\x25\x00\x00\x00\x93\x00\x93\x00\xe4\xff\x93\x00\xe4\xff\x93\x00\x93\x00\x97\x01\x97\x01\x00\x00\xf6\x00\x00\x00\x97\x01\xc7\x01\x79\x01\x00\x00\x5b\x01\xd8\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x00\x4b\x00\x5b\x01\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x46\x00\xfb\x03\x53\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xed\x03\x57\x02\xd9\x00\xff\xff\xdf\x03\xd1\x03\x00\x00\xc3\x03\xfc\xff\xb5\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x02\xa7\x03\x99\x03\x8b\x03\x7d\x03\x6f\x03\x61\x03\x53\x03\x45\x03\x37\x03\x29\x03\x1b\x03\x0d\x03\xff\x02\xf1\x02\xe3\x02\x07\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x02\xd5\x02\x00\x00\xc7\x02\xb9\x02\x00\x00\xab\x02\x9d\x02\x09\x00\x8f\x02\xfe\xff\x81\x02\x73\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x02\x26\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xfd\xff\xf9\xff\x00\x00\xf6\xff\xc8\xff\x00\x00\xeb\xff\xea\xff\xe9\xff\xe8\xff\xe7\xff\xe6\xff\xe5\xff\x00\x00\xf1\xff\xed\xff\x00\x00\x00\x00\x00\x00\xd4\xff\x00\x00\x00\x00\x00\x00\xd5\xff\xe4\xff\x00\x00\xd1\xff\x00\x00\x00\x00\xc4\xff\x00\x00\xdd\xff\xe3\xff\xf5\xff\x00\x00\xec\xff\x00\x00\xef\xff\xf0\xff\x00\x00\xf3\xff\x00\x00\xf1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xfc\xff\xf8\xff\xf7\xff\xd2\xff\xd3\xff\xcb\xff\xcc\xff\xcf\xff\xd0\xff\xd9\xff\xde\xff\xdf\xff\xe0\xff\xe1\xff\xe2\xff\xcd\xff\xce\xff\x00\x00\xf3\xff\xf1\xff\x00\x00\xd8\xff\x00\x00\x00\x00\xdc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\xff\xc7\xff\xc3\xff\x00\x00\xf4\xff\xc9\xff\xee\xff\x00\x00\xda\xff\xf2\xff\x00\x00\xd6\xff\x00\x00\xc6\xff\xfa\xff\xca\xff\xd7\xff\xdb\xff\x00\x00\xc5\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x05\x00\x01\x00\x05\x00\x05\x00\x06\x00\x01\x00\x01\x00\x07\x00\x08\x00\x42\x00\x04\x00\x05\x00\x45\x00\x05\x00\x13\x00\x14\x00\x02\x00\x14\x00\x11\x00\x12\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x01\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x1a\x00\x1a\x00\x01\x00\x21\x00\x22\x00\x23\x00\x24\x00\x01\x00\x43\x00\x03\x00\x02\x00\x02\x00\x05\x00\x07\x00\x08\x00\x09\x00\x01\x00\x2f\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x45\x00\x15\x00\x3a\x00\x43\x00\x01\x00\x19\x00\x03\x00\x04\x00\x04\x00\x12\x00\x07\x00\x12\x00\x09\x00\x00\x00\x08\x00\x47\x00\x03\x00\x04\x00\x05\x00\x01\x00\x13\x00\x03\x00\x47\x00\x43\x00\x15\x00\x07\x00\x13\x00\x09\x00\x19\x00\x03\x00\x04\x00\x05\x00\x45\x00\x43\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\x19\x00\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\x36\x00\x37\x00\x38\x00\x39\x00\x01\x00\xff\xff\x03\x00\xff\xff\xff\xff\xff\xff\x07\x00\x41\x00\x09\x00\x43\x00\x44\x00\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\xff\xff\x03\x00\xff\xff\xff\xff\x15\x00\x07\x00\xff\xff\x09\x00\x19\x00\xff\xff\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\xff\xff\x15\x00\xff\xff\xff\xff\xff\xff\x19\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\x36\x00\x37\x00\x38\x00\x39\x00\x2f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\xff\xff\x43\x00\x44\x00\x3a\x00\x01\x00\xff\xff\xff\xff\x04\x00\x3f\x00\x05\x00\x07\x00\x08\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\x07\x00\x08\x00\xff\xff\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3a\x00\x12\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\x01\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x07\x00\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\x2f\x00\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\xff\xff\xff\xff\x3a\x00\xff\xff\xff\xff\x2b\x00\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\x3a\x00\xff\xff\xff\xff\x01\x00\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2f\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x01\x00\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x07\x00\x08\x00\x21\x00\x22\x00\x23\x00\x24\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x1a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\x22\x00\x23\x00\x24\x00\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\x07\x00\x08\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x05\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1e\x00\x2e\x00\x1e\x00\x24\x00\x25\x00\x2e\x00\x2e\x00\x2f\x00\x30\x00\x5e\x00\x40\x00\x05\x00\x3f\x00\x63\x00\x1f\x00\x20\x00\x3f\x00\x61\x00\x5a\x00\x5b\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x5c\x00\x36\x00\x33\x00\x34\x00\x35\x00\x36\x00\x36\x00\x3d\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x11\x00\x07\x00\x12\x00\x6f\x00\x6d\x00\x07\x00\x13\x00\x68\x00\x14\x00\x2e\x00\x3b\x00\x72\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x6e\x00\x15\x00\x3c\x00\x07\x00\x11\x00\x16\x00\x12\x00\x6b\x00\x55\x00\x56\x00\x13\x00\x59\x00\x14\x00\x1c\x00\x58\x00\xff\xff\x03\x00\x04\x00\x05\x00\x11\x00\x5f\x00\x12\x00\xff\xff\x07\x00\x15\x00\x13\x00\x3d\x00\x14\x00\x16\x00\x03\x00\x04\x00\x05\x00\x3f\x00\x07\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x1c\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x11\x00\x00\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x14\x00\x07\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x00\x00\x15\x00\x13\x00\x00\x00\x14\x00\x16\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x00\x00\x15\x00\x00\x00\x00\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x1c\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x1c\x00\x3c\x00\x2e\x00\x00\x00\x00\x00\x70\x00\x5c\x00\x07\x00\x2f\x00\x30\x00\x00\x00\x26\x00\x27\x00\x28\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x72\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x00\x53\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x54\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x71\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x2e\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2f\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x00\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x57\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x3c\x00\x00\x00\x00\x00\x2e\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x2e\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x00\x00\x36\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x07\x00\x00\x00\x29\x00\x6b\x00\x00\x00\x00\x00\x51\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x29\x00\x50\x00\x00\x00\x00\x00\x51\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x29\x00\x2a\x00\x00\x00\x00\x00\x2b\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x64\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x68\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x42\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x43\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x45\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x49\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (2, 61) [
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61)
	]

happy_n_terms = 72 :: Int
happy_n_nonterms = 21 :: Int

happyReduce_2 = happySpecReduce_1  0# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (CSPMScript happy_var_1
	)}

happyReduce_3 = happySpecReduce_2  1# happyReduction_3
happyReduction_3 happy_x_2
	happy_x_1
	 =  happyIn6
		 (()
	)

happyReduce_4 = happySpecReduce_0  2# happyReduction_4
happyReduction_4  =  happyIn7
		 (()
	)

happyReduce_5 = happySpecReduce_2  2# happyReduction_5
happyReduction_5 happy_x_2
	happy_x_1
	 =  happyIn7
		 (()
	)

happyReduce_6 = happySpecReduce_1  3# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOut9 happy_x_1 of { happy_var_1 -> 
	happyIn8
		 ([happy_var_1]
	)}

happyReduce_7 = happySpecReduce_3  3# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut9 happy_x_3 of { happy_var_3 -> 
	happyIn8
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_8 = happySpecReduce_3  4# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn9
		 (DDefinition happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_1  5# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TIdentifier happy_var_1) -> 
	happyIn10
		 (Identifier happy_var_1
	)}

happyReduce_10 = happySpecReduce_1  6# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn11
		 ([happy_var_1]
	)}

happyReduce_11 = happySpecReduce_3  6# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut11 happy_x_1 of { happy_var_1 -> 
	case happyOut10 happy_x_3 of { happy_var_3 -> 
	happyIn11
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_12 = happySpecReduce_1  7# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ([happy_var_1]
	)}

happyReduce_13 = happySpecReduce_3  7# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_14 = happySpecReduce_0  8# happyReduction_14
happyReduction_14  =  happyIn13
		 ([]
	)

happyReduce_15 = happySpecReduce_1  8# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (happy_var_1
	)}

happyReduce_16 = happySpecReduce_1  9# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_17 = happySpecReduce_3  9# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_18 = happySpecReduce_0  10# happyReduction_18
happyReduction_18  =  happyIn15
		 ([]
	)

happyReduce_19 = happySpecReduce_1  10# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  11# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut17 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  11# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut18 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_22 = happySpecReduce_1  11# happyReduction_22
happyReduction_22 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_23 = happySpecReduce_1  11# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  11# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  11# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  11# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  12# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { (TNumber happy_var_1) -> 
	happyIn17
		 (ENLit happy_var_1
	)}

happyReduce_28 = happySpecReduce_2  12# happyReduction_28
happyReduction_28 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (ENNeg happy_var_2
	)}

happyReduce_29 = happySpecReduce_3  12# happyReduction_29
happyReduction_29 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (ENSum happy_var_1 happy_var_3
	)}}

happyReduce_30 = happySpecReduce_3  12# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (ENDiff happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_3  12# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (ENProd happy_var_1 happy_var_3
	)}}

happyReduce_32 = happySpecReduce_3  12# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (ENQuot happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_3  12# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn17
		 (ENRem happy_var_1 happy_var_3
	)}}

happyReduce_34 = happySpecReduce_2  12# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (EQLength happy_var_2
	)}

happyReduce_35 = happySpecReduce_3  13# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (EQLit happy_var_2
	)}

happyReduce_36 = happyReduce 5# 13# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn18
		 (EQClosedRange happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_37 = happyReduce 4# 13# happyReduction_37
happyReduction_37 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn18
		 (EQOpenRange happy_var_2
	) `HappyStk` happyRest}

happyReduce_38 = happySpecReduce_3  13# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn18
		 (EQConcat happy_var_1 happy_var_3
	)}}

happyReduce_39 = happySpecReduce_3  14# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut13 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (ESLit happy_var_2
	)}

happyReduce_40 = happyReduce 5# 14# happyReduction_40
happyReduction_40 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn19
		 (ESClosedRange happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_41 = happyReduce 4# 14# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn19
		 (ESOpenRange happy_var_2
	) `HappyStk` happyRest}

happyReduce_42 = happySpecReduce_1  15# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn20
		 (EBTrue
	)

happyReduce_43 = happySpecReduce_1  15# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn20
		 (EBFalse
	)

happyReduce_44 = happySpecReduce_3  15# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EBAnd happy_var_1 happy_var_3
	)}}

happyReduce_45 = happySpecReduce_3  15# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EBOr happy_var_1 happy_var_3
	)}}

happyReduce_46 = happySpecReduce_2  15# happyReduction_46
happyReduction_46 happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_2 of { happy_var_2 -> 
	happyIn20
		 (EBNot happy_var_2
	)}

happyReduce_47 = happySpecReduce_3  15# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EEqual happy_var_1 happy_var_3
	)}}

happyReduce_48 = happySpecReduce_3  15# happyReduction_48
happyReduction_48 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (ENotEqual happy_var_1 happy_var_3
	)}}

happyReduce_49 = happySpecReduce_3  15# happyReduction_49
happyReduction_49 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (ELT happy_var_1 happy_var_3
	)}}

happyReduce_50 = happySpecReduce_3  15# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EGT happy_var_1 happy_var_3
	)}}

happyReduce_51 = happySpecReduce_3  15# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (ELTE happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_3  15# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn20
		 (EGTE happy_var_1 happy_var_3
	)}}

happyReduce_53 = happyReduce 5# 16# happyReduction_53
happyReduction_53 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut13 happy_x_4 of { happy_var_4 -> 
	happyIn21
		 (ETLit (happy_var_2:happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_54 = happyReduce 4# 17# happyReduction_54
happyReduction_54 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut11 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn22
		 (ELambda happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_55 = happySpecReduce_1  18# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (EVar happy_var_1
	)}

happyReduce_56 = happyReduce 4# 18# happyReduction_56
happyReduction_56 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	happyIn23
		 (ELet happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_57 = happyReduce 4# 18# happyReduction_57
happyReduction_57 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (EApply happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_58 = happyReduce 6# 18# happyReduction_58
happyReduction_58 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut16 happy_x_2 of { happy_var_2 -> 
	case happyOut16 happy_x_4 of { happy_var_4 -> 
	case happyOut16 happy_x_6 of { happy_var_6 -> 
	happyIn23
		 (EIfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_59 = happySpecReduce_1  19# happyReduction_59
happyReduction_59 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 ([happy_var_1]
	)}

happyReduce_60 = happySpecReduce_3  19# happyReduction_60
happyReduction_60 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_3 of { happy_var_3 -> 
	happyIn24
		 (happy_var_1 ++ [happy_var_3]
	)}}

happyReduce_61 = happySpecReduce_3  20# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	happyIn25
		 (Binding happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 71# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	TLParen -> cont 1#;
	TRParen -> cont 2#;
	TLBrace -> cont 3#;
	TRBrace -> cont 4#;
	TLBracket -> cont 5#;
	TRBracket -> cont 6#;
	TLAngle -> cont 7#;
	TRAngle -> cont 8#;
	TBackslash -> cont 9#;
	TPipe -> cont 10#;
	TColon -> cont 11#;
	TAmpersand -> cont 12#;
	TSemi -> cont 13#;
	TDot -> cont 14#;
	TQMark -> cont 15#;
	TBang -> cont 16#;
	TAt -> cont 17#;
	TComma -> cont 18#;
	TEquals -> cont 19#;
	TPlus -> cont 20#;
	TMinus -> cont 21#;
	TAsterisk -> cont 22#;
	TSlash -> cont 23#;
	TPercent -> cont 24#;
	THash -> cont 25#;
	TCaret -> cont 26#;
	TLMap -> cont 27#;
	TRMap -> cont 28#;
	TLPBrace -> cont 29#;
	TRPBrace -> cont 30#;
	TLPar -> cont 31#;
	TRPar -> cont 32#;
	TEq -> cont 33#;
	TNE -> cont 34#;
	TLE -> cont 35#;
	TGE -> cont 36#;
	TRTriangle -> cont 37#;
	TBox -> cont 38#;
	TTwoPipe -> cont 39#;
	TLArrow -> cont 40#;
	TRArrow -> cont 41#;
	TTriangle -> cont 42#;
	TTwoDot -> cont 43#;
	TCap -> cont 44#;
	TThreePipe -> cont 45#;
	TRefinedBy happy_dollar_dollar -> cont 46#;
	TAnd -> cont 47#;
	TAssert -> cont 48#;
	TChannel -> cont 49#;
	TChaos -> cont 50#;
	TDatatype -> cont 51#;
	TElse -> cont 52#;
	TExternal -> cont 53#;
	TFalse -> cont 54#;
	TIf -> cont 55#;
	TLet -> cont 56#;
	TNot -> cont 57#;
	TOr -> cont 58#;
	TPragma -> cont 59#;
	TPrint -> cont 60#;
	TSkip -> cont 61#;
	TStop -> cont 62#;
	TThen -> cont 63#;
	TTransparent -> cont 64#;
	TTrue -> cont 65#;
	TWithin -> cont 66#;
	TIdentifier happy_dollar_dollar -> cont 67#;
	TNumber happy_dollar_dollar -> cont 68#;
	TNewline -> cont 69#;
	TBadChar -> cont 70#;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseFile_ tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut5 x))

parseExpr_ tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut16 x))

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error ("Parse error\n")

parseFile = parseFile_ . lexer
parseExpr = parseExpr_ . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Int# Happy_IntList





{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st

{-# LINE 127 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             off    = indexShortOffAddr happyGotoOffsets st1
             off_i  = (off +# nt)
             new_state = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
