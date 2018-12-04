module Cooking.Stack where

data Stack a = Nil | Step a (Stack a) deriving (Eq, Show)

push :: Stack a -> a -> Stack a
Nil        `push` y = Step y Nil
(Step x s) `push` y = Step y (Step x s)

isEmpty :: Stack a -> Bool
isEmpty Nil = True
isEmpty _   = False

peek :: Stack a -> a
peek Nil        = error "Stack is empty"
peek (Step x _) = x

pop :: Stack a -> Stack a
pop Nil        = error "Stack is empty"
pop (Step x s) = s

