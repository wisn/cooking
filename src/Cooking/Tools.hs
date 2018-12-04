module Cooking.Tools where

data Tools = Blender BlenderKind
           | Bowl String
           | Knife String
           | PassableTools
           | Plate String
           | RiceCooker String
           | Stove StoveKind
           | Tumblr String
           deriving (Eq, Show)

type BlenderKind = String
type StoveKind = String

tools :: Tools -> Either Tools b
tools a = Left a

blender :: Tools
blender = Blender "B1"

tumblr :: Tools
tumblr = Tumblr "T1"

