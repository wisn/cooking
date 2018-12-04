module Cooking.Goods where

data Goods = Chili String
           | CookedRice RiceKind
           | FriedPotato PotatoKind
           | FriedTuna TunaKind
           | Fruit FruitKind
           | Juice FruitKind
           | PassableGoods
           | Potato PotatoKind
           | Rice RiceKind
           | Sugar String
           | Tuna TunaKind
           | Water String
           deriving (Eq, Show)

data FruitKind = Strawberry String deriving (Eq, Show)

type PotatoKind = String
type RiceKind = String
type TunaKind = String

goods :: Goods -> Either a Goods
goods b = Right b

sugar :: Goods
sugar = Sugar "S1"

strawberry :: FruitKind
strawberry = Strawberry "S1"

water :: Goods
water = Water "W1"

