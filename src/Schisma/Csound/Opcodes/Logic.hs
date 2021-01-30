module Schisma.Csound.Opcodes.Logic
  ( Comparator(..)
  , CompoundLogicExpression(..)
  , LogicExpression(..)
  , LogicStatement(..)
  , ifAndS
  , ifOrS
  ) where

import           Data.Text                      ( Text )

import           Schisma.Csound.Types.Signals   ( ARateSignal(..)
                                                , Conditional(..)
                                                , ContainsSignals(..)
                                                , IRateSignal(..)
                                                , IsSignal(..)
                                                , KRateSignal(..)
                                                , Opcode(ConditionalExpression)
                                                , OrdinaryStatement
                                                  ( ConditionalStatement
                                                  )
                                                , Signal(Signal)
                                                )

class (IsSignal a) => Comparator a where
  -- | Compares two signals for equality.
  compareEqual
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareEqual (checkSignal, againstSignal) =
    Predicate "==" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one signal is not equal to another.
  compareNotEqual
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareNotEqual (checkSignal, againstSignal) =
    Predicate "!=" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one signal is less than another.
  compareLessThan
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareLessThan (checkSignal, againstSignal) =
    Predicate "<" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one signal is less than or equal to another.
  compareLessThanOrEqualTo
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareLessThanOrEqualTo (checkSignal, againstSignal) =
    Predicate "<=" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one signal is greater than another.
  compareGreaterThan
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareGreaterThan (checkSignal, againstSignal) =
    Predicate ">" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one signal is greater than or equal to another.
  compareGreaterThanOrEqualTo
    :: (a, a)      -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> Conditional -- ^ The returned conditional.
  compareGreaterThanOrEqualTo (checkSignal, againstSignal) =
    Predicate ">=" (getSignal checkSignal, getSignal againstSignal)

instance Comparator ARateSignal
instance Comparator IRateSignal
instance Comparator KRateSignal


class (IsSignal a, ContainsSignals b) => LogicExpression a b where
  -- | Compares two values for equality.
  --
  --   <https://csound.com/docs/manual/equals.html Csound documentation>
  ifEqualE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

  -- | Determines if one value is not equal to another.
  --
  --   <https://csound.com/docs/manual/notequal.html Csound documentation>
  ifNotEqualE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

  -- | Determines if one value is less than another.
  --
  --   <https://csound.com/docs/manual/lessthan.html Csound documentation>
  ifLessThanE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

  -- | Determines if one value is less than or equal to another.
  --
  --   <https://csound.com/docs/manual/lessequal.html Csound documentation>
  ifLessThanOrEqualToE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

  -- | Determines if one value is greater than another.
  --
  --   <https://csound.com/docs/manual/greaterthan.html Csound documentation>
  ifGreaterThanE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

  -- | Determines if one value is greater than or equal to another.
  --
  --   <https://csound.com/docs/manual/greaterequal.html Csound documentation>
  ifGreaterThanOrEqualToE
    :: (a, a) -- ^ @(checkSignal, againstSignal)@ - The signals to compare.
    -> (b, b) -- ^ @(ifSignal, elseSignal)@ - The signal(s) to return within
              --   each logical branch.
    -> b      -- ^ The returned signal(s).

instance LogicExpression IRateSignal ARateSignal where
  ifEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifNotEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

instance LogicExpression IRateSignal [ARateSignal] where
  ifEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifNotEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals)
    = map ARateSignal $ predicateSignals
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

instance LogicExpression KRateSignal ARateSignal where
  ifEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifNotEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    ARateSignal $ predicateSignal
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

instance LogicExpression KRateSignal [ARateSignal] where
  ifEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifNotEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map ARateSignal $ predicateSignals
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals)
    = map ARateSignal $ predicateSignals
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

instance LogicExpression IRateSignal KRateSignal where
  ifEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifNotEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

instance LogicExpression IRateSignal [KRateSignal] where
  ifEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifNotEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals)
    = map KRateSignal $ predicateSignals
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

instance LogicExpression KRateSignal KRateSignal where
  ifEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifNotEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    KRateSignal $ predicateSignal
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

instance LogicExpression KRateSignal [KRateSignal] where
  ifEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifNotEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map KRateSignal $ predicateSignals
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals)
    = map KRateSignal $ predicateSignals
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

instance LogicExpression IRateSignal IRateSignal where
  ifEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifNotEqualE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignal, elseSignal) =
    IRateSignal $ predicateSignal
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignal ifSignal   , getSignal elseSignal)

instance LogicExpression IRateSignal [IRateSignal] where
  ifEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map IRateSignal $ predicateSignals
      "=="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifNotEqualE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map IRateSignal $ predicateSignals
      "!="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map IRateSignal $ predicateSignals
      "<"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifLessThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map IRateSignal $ predicateSignals
      "<="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanE (checkSignal, againstSignal) (ifSignals, elseSignals) =
    map IRateSignal $ predicateSignals
      ">"
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)

  ifGreaterThanOrEqualToE (checkSignal, againstSignal) (ifSignals, elseSignals)
    = map IRateSignal $ predicateSignals
      ">="
      (getSignal checkSignal, getSignal againstSignal)
      (getSignals ifSignals , getSignals elseSignals)


class (IsSignal a) => LogicStatement a where
  -- | Compares two values for equality.
  --
  --   <https://csound.com/docs/manual/equals.html Csound documentation>
  ifEqualS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifEqualS (checkSignal, againstSignal) =
    predicateStatement "==" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one value is not equal to another.
  --
  --   <https://csound.com/docs/manual/notequal.html Csound documentation>
  ifNotEqualS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifNotEqualS (checkSignal, againstSignal) =
    predicateStatement "!=" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one value is less than another.
  --
  --   <https://csound.com/docs/manual/lessthan.html Csound documentation>
  ifLessThanS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifLessThanS (checkSignal, againstSignal) =
    predicateStatement "<" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one value is less than or equal to another.
  --
  --   <https://csound.com/docs/manual/lessequal.html Csound documentation>
  ifLessThanOrEqualToS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifLessThanOrEqualToS (checkSignal, againstSignal) =
    predicateStatement "<=" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one value is greater than another.
  --
  --   <https://csound.com/docs/manual/greaterthan.html Csound documentation>
  ifGreaterThanS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifGreaterThanS (checkSignal, againstSignal) =
    predicateStatement ">" (getSignal checkSignal, getSignal againstSignal)

  -- | Determines if one value is greater than or equal to another.
  --
  --   <https://csound.com/docs/manual/greaterequal.html Csound documentation>
  ifGreaterThanOrEqualToS
    :: (a, a)                -- ^ @(checkSignal, againstSignal)@ - The signals
                             --   to compare.
    -> ( OrdinaryStatement
       , OrdinaryStatement ) -- ^ @(ifStatement, elseStatement)@ - The
                             --   ordinary statement to return within each
                             --   logical branch.
    -> OrdinaryStatement     -- ^ The returned ordinary statement.
  ifGreaterThanOrEqualToS (checkSignal, againstSignal) =
    predicateStatement ">=" (getSignal checkSignal, getSignal againstSignal)

instance LogicStatement ARateSignal
instance LogicStatement KRateSignal
instance LogicStatement IRateSignal


class CompoundLogicExpression a where
  -- | Logical AND operator.
  --
  --   <https://csound.com/docs/manual/opand.html Csound documentation>
  ifAndE
    :: (Conditional, Conditional) -- ^ @conditionals@ - The conditional
                                  --   clauses.
    -> (a, a)                     -- ^ @(ifSignal, elseSignal)@ - The
                                  --   signal(s) to return within each logical
                                  --   branch.
    -> a                          -- ^ The returned signal(s).

  -- | Logical OR operator.
  --
  --   <https://csound.com/docs/manual/opor.html Csound documentation>
  ifOrE
    :: (Conditional, Conditional) -- ^ @conditionals@ - The conditional
                                  --   clauses.
    -> (a, a)                     -- ^ @(ifSignal, elseSignal)@ - The
                                  --   signal(s) to return within each logical
                                  --   branch.
    -> a                          -- ^ The returned signal(s).

instance CompoundLogicExpression ARateSignal where
  ifAndE conditionals (ifSignal, elseSignal) =
    ARateSignal $ compoundPredicateSignal
      "&&"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

  ifOrE conditionals (ifSignal, elseSignal) =
    ARateSignal $ compoundPredicateSignal
      "||"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

instance CompoundLogicExpression [ARateSignal]  where
  ifAndE conditionals (ifSignals, elseSignals) =
    map ARateSignal $ compoundPredicateSignals
      "&&"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)

  ifOrE conditionals (ifSignals, elseSignals) =
    map ARateSignal $ compoundPredicateSignals
      "||"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)

instance CompoundLogicExpression KRateSignal  where
  ifAndE conditionals (ifSignal, elseSignal) =
    KRateSignal $ compoundPredicateSignal
      "&&"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

  ifOrE conditionals (ifSignal, elseSignal) =
    KRateSignal $ compoundPredicateSignal
      "||"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

instance CompoundLogicExpression [KRateSignal]  where
  ifAndE conditionals (ifSignals, elseSignals) =
    map KRateSignal $ compoundPredicateSignals
      "&&"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)

  ifOrE conditionals (ifSignals, elseSignals) =
    map KRateSignal $ compoundPredicateSignals
      "||"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)

instance CompoundLogicExpression IRateSignal  where
  ifAndE conditionals (ifSignal, elseSignal) =
    IRateSignal $ compoundPredicateSignal
      "&&"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

  ifOrE conditionals (ifSignal, elseSignal) =
    IRateSignal $ compoundPredicateSignal
      "||"
      conditionals
      (getSignal ifSignal, getSignal elseSignal)

instance CompoundLogicExpression [IRateSignal]  where
  ifAndE conditionals (ifSignals, elseSignals) =
    map IRateSignal $ compoundPredicateSignals
      "&&"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)

  ifOrE conditionals (ifSignals, elseSignals) =
    map IRateSignal $ compoundPredicateSignals
      "||"
      conditionals
      (getSignals ifSignals, getSignals elseSignals)


ifAndS
  :: (Conditional, Conditional)       -- ^ @conditionals@ - The conditional clauses.
  -> (OrdinaryStatement, OrdinaryStatement) -- ^ @(ifStatement, elseStatement)@ - The
                           --   ordinary statement to return within each
                           --   logical branch.
  -> OrdinaryStatement     -- ^ The returned ordinary statement.
ifAndS = compoundPredicateStatement "&&"

ifOrS
  :: (Conditional, Conditional)       -- ^ @conditionals@ - The conditional clauses.
  -> (OrdinaryStatement, OrdinaryStatement) -- ^ @(ifStatement, elseStatement)@ - The
                           --   ordinary statement to return within each
                           --   logical branch.
  -> OrdinaryStatement     -- ^ The returned ordinary statement.
ifOrS = compoundPredicateStatement "||"

predicateSignal :: Text -> (Signal, Signal) -> (Signal, Signal) -> Signal
predicateSignal operator operands bodies@(ifSignal, elseSignal) = result where
  conditional = Predicate operator operands
  opcode      = ConditionalExpression conditional ([ifSignal], [elseSignal])
  result      = Signal opcode 1

predicateSignals
  :: Text -> (Signal, Signal) -> ([Signal], [Signal]) -> [Signal]
predicateSignals operator operands bodies@(ifSignals, elseSignals) = result where
  numberOfIfSignals   = fromIntegral $ length ifSignals
  numberOfElseSignals = fromIntegral $ length elseSignals
  conditional         = Predicate operator operands
  opcode              = ConditionalExpression conditional bodies

  result              = if numberOfIfSignals == numberOfElseSignals
    then map (Signal opcode) [1 .. numberOfIfSignals]
    else error "number of resulting signals differ between branches"

predicateStatement
  :: Text
  -> (Signal, Signal)
  -> (OrdinaryStatement, OrdinaryStatement)
  -> OrdinaryStatement
predicateStatement operator operands ordinaryStatements = statement where
  conditional = Predicate operator operands
  statement   = ConditionalStatement conditional ordinaryStatements

compoundPredicateSignal
  :: Text -> (Conditional, Conditional) -> (Signal, Signal) -> Signal
compoundPredicateSignal operator conditionals bodies@(ifSignal, elseSignal) =
  result where
  conditional = CompoundPredicate operator conditionals
  opcode      = ConditionalExpression conditional ([ifSignal], [elseSignal])
  result      = Signal opcode 1

compoundPredicateSignals
  :: Text -> (Conditional, Conditional) -> ([Signal], [Signal]) -> [Signal]
compoundPredicateSignals operator conditionals bodies@(ifSignals, elseSignals)
  = result where
  numberOfIfSignals   = fromIntegral $ length ifSignals
  numberOfElseSignals = fromIntegral $ length elseSignals
  conditional         = CompoundPredicate operator conditionals
  opcode              = ConditionalExpression conditional bodies

  result              = if numberOfIfSignals == numberOfElseSignals
    then map (Signal opcode) [1 .. numberOfIfSignals]
    else error "number of resulting signals differ between branches"

compoundPredicateStatement
  :: Text
  -> (Conditional, Conditional)
  -> (OrdinaryStatement, OrdinaryStatement)
  -> OrdinaryStatement
compoundPredicateStatement operator conditionals statementOpcodes = statement
 where
  conditional = CompoundPredicate operator conditionals
  statement   = ConditionalStatement conditional statementOpcodes
