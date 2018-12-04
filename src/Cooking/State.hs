module Cooking.State where

import Cooking.Goods
import Cooking.Tools

data State = Blended Goods
           | Clean Goods
           | EmptyArm
           | Fridge Goods
           | FoodReady Goods
           | In Goods Tools
           | JuiceReady Goods
           | On (Either Tools Goods) (Either Tools Goods)
           | OnArm (Either Tools Goods)
           | OnTable (Either Tools Goods)
           | PassableState
           | Sliced Goods
           | SnackReady Goods
           deriving (Eq, Show)

found :: [State] -> State -> Bool
[]           `found` x = False
(state:rest) `found` x
  | state == x = True
  | otherwise  = rest `found` x

initialState :: [State]
initialState =
  [ OnTable $ tools blender
  , OnTable $ tools tumblr
  , In water tumblr
  , Fridge $ Fruit strawberry
  , OnTable $ goods sugar
  ]

goalState :: [State]
goalState = [JuiceReady $ Juice strawberry]

