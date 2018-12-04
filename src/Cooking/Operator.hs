module Cooking.Operator where

import Cooking.Goods
import Cooking.State
import Cooking.Tools

data Operator = Blend Goods Tools Attributes
              | Load Goods Attributes
              | Pick (Either Tools Goods) Attributes
              | Pour Tools Tools Attributes
              | Put Goods Tools Attributes
              deriving (Eq, Show)

data Attributes = Attributes
                { postcondition :: PostconditionKind
                , precondition :: PreconditionKind
                , runable :: Bool
                }
                deriving (Eq, Show)

type PostconditionKind = [Action]
type PreconditionKind = [State]

data Action = Add State | Remove State deriving (Eq, Show)

{- OPERATOR MANIPULATOR -}
-- | Tell the program that the operator is ready so when the stack is
-- processing the operator it will runs.
readyToRun :: Operator -> Operator
readyToRun (Load g a)    = Load g (a { runable = True })
readyToRun (Blend g t a) = Blend g t (a { runable = True })
readyToRun opr           = opr

-- | Retrieve precondition of the operator so the program would push
-- them all into the stack.
getPrecondition :: Operator -> PreconditionKind
getPrecondition (Load _ a)    = fetchPrecondition a
getPrecondition (Blend _ _ a) = fetchPrecondition a
getPrecondition _             = []

-- | Retrieve postconditon of the operator so the program would runs
-- all the command after successfully ran the operator.
getPostcondition :: Operator -> PostconditionKind
getPostcondition (Load _ a)    = fetchPostcondition a
getPostcondition (Blend _ _ a) = fetchPostcondition a
getPostcondition _             = []

-- | Check whether the operator is ready.
isRunable :: Operator -> Bool
isRunable (Load _ a)    = fetchRunable a
isRunable (Blend _ _ a) = fetchRunable a
isRunable _             = False
{- END -}

{- ATTRIBUTES MANIPULATOR -}
-- | Fetch the precondition of an operator by its attributes.
fetchPrecondition :: Attributes -> PreconditionKind
fetchPrecondition (Attributes { precondition = p }) = p

-- | Fetch the postcondition of an operator by its attributes.
fetchPostcondition :: Attributes -> PostconditionKind
fetchPostcondition (Attributes { postcondition = p }) = p

-- | Fetch the runable status of an operator by its attributes.
fetchRunable :: Attributes -> Bool
fetchRunable (Attributes { runable = r }) = r
{- END -}

data OperatorFn = OprE ((Either Tools Goods) -> Operator)
                | OprG (Goods -> Operator)
                | OprT (Tools -> Operator)
                | OprGG (Goods -> Goods -> Operator)
                | OprGT (Goods -> Tools -> Operator)
                | OprTT (Tools -> Tools -> Operator)

operators :: [OperatorFn]
operators =
  [ OprG load
  , OprGT blend
  , OprE pick
  , OprTT pour
  ]

blend :: Goods -> Tools -> Operator
blend g t = Blend g t (blendAttributes g t)

blendAttributes :: Goods -> Tools -> Attributes
blendAttributes g t =
  Attributes
  { precondition = [In g t, EmptyArm]
  , postcondition = [Remove $ In g t, Add $ Blended g]
  , runable = False
  }

load :: Goods -> Operator
load g = Load g (loadAttributes g)

loadAttributes :: Goods -> Attributes
loadAttributes g =
  Attributes
  { runable = False
  , precondition = [Fridge g, EmptyArm]
  , postcondition = [Add $ OnTable (goods g)]
  }

pick :: Either Tools Goods -> Operator
pick x = Pick x (pickAttributes x)

pickAttributes :: Either Tools Goods -> Attributes
pickAttributes x =
  Attributes
  { runable = False
  , precondition = [OnTable x, EmptyArm]
  , postcondition = [Add $ OnArm x, Remove $ OnTable x, Remove EmptyArm]
  }

pour :: Tools -> Tools -> Operator
pour a b = Pour a b (pourAttributes a b)

pourAttributes :: Tools -> Tools -> Attributes
pourAttributes a b =
  Attributes
  { runable = False
  , precondition =
      [ OnArm (tools a)
      , In PassableGoods a
      , EmptyArm
      , OnTable (tools a)
      , OnTable (tools b)
      ]
  , postcondition =
      [ Remove $ In PassableGoods a
      , Remove $ OnArm (tools a)
      , Add EmptyArm
      , Add $ In PassableGoods b
      ]
  }

