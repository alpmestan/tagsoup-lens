{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Text.HTML.TagSoup.Lens where

import Control.Lens hiding (children, element)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap, fromListWith, toList)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.StringLike

data Node str = Elt (Element str) -- ^ an HTML tag (@<foo>...</foo>@ or @<foo />@)
              | T str             -- ^ "raw" text
              | Comment str       -- ^ an HTML comment (@<!-- some comment -->@)
  deriving (Eq, Show)

data Element str = Element
  { _name :: str
  , _children :: [Node str]
  , _attributes :: HashMap str [str]
  } deriving (Eq, Show)

makeLenses ''Element

class AsElement a where
  _Element :: Prism' (a str) (Element str)

instance AsElement Node where
  _Element = prism' Elt $ \case
    Elt e -> Just e
    T   _ -> Nothing

instance AsElement Element where
  _Element = prism' id (Just . id)

element :: Prism' (Node str) (Element str)
element = prism' Elt $ \case
  Elt e -> Just e
  _     -> Nothing

attr :: (Hashable str, Eq str) => str -> Lens' (Element str) (Maybe [str])
attr name = attributes . at name

attrOne :: (Hashable str, Eq str) => str -> Traversal' (Element str) str
attrOne name = attr name . _Just . _head

text :: Prism' (Node str) str
text = prism' T $ \case
  Elt _ -> Nothing
  T txt -> Just txt

comment :: Prism' (Node str) str
comment = prism' Comment $ \case
  Comment c -> Just c
  _         -> Nothing

trees :: StringLike str => Prism' str [TagTree str]
trees = prism' renderTree (Just . parseTree)

node :: (Hashable str, Eq str, Show str, StringLike str) => Iso' (TagTree str) (Node str)
node = iso tree2node node2tree

hashmapify :: (Hashable str, Eq str) => [Attribute str] -> HashMap str [str]
hashmapify as = fromListWith (++) $ [ (k, [v]) | (k, v) <- as ]

assoclist :: HashMap str [str] -> [Attribute str]
assoclist = concatMap f . toList

  where f (key, vals) = map (key,) vals

tree2node :: (Hashable str, Eq str, Show str, StringLike str) => TagTree str -> Node str
tree2node (TagBranch name attrs cs) =
  Elt (Element name (map tree2node cs) $ hashmapify attrs)
tree2node (TagLeaf (TagText txt)) = T txt
tree2node (TagLeaf (TagComment c)) = Comment c
tree2node (TagLeaf (TagOpen name attrs)) = Elt (Element name [] $ hashmapify attrs)
tree2node (TagLeaf (TagClose name)) = Comment empty
tree2node t = error $ "tree2node: non textual TagLeaf <" ++ show t ++ ">"

node2tree :: Node str -> TagTree str
node2tree (Elt e) =
  TagBranch (e ^. name) (assoclist $ e ^. attributes) $
  map node2tree (e ^. children)
node2tree (T txt) = TagLeaf (TagText txt)

nodes :: (Hashable str, Eq str, Show str, StringLike str) => Iso' [TagTree str] [Node str]
nodes = iso (map tree2node) (map node2tree)

_DOM :: (StringLike str, Hashable str, Show str, Eq str) => Prism' str [Node str]
_DOM = trees . nodes

attributed :: Fold (HashMap str [str]) a -> Traversal' (Element str) (Element str)
attributed fld = filtered . has $ attributes . fld

named :: Fold str a -> Traversal' (Element str) (Element str)
named fld = filtered . has $ name . fld

instance Plated (Node str) where
  plate = element . children . traverse

instance Plated (Element str) where
  plate = children . traverse . element

allNamed :: Fold str b -> Fold (Node str) (Element str)
allNamed fld = element . to universe . traverse . named fld

allAttributed :: Fold (HashMap str [str]) b -> Fold (Node str) (Element str)
allAttributed fld = element . to universe . traverse . attributed fld

allElts f = element . to universe . traverse . filtered . has f

class HasContents e str | e -> str where
  -- | A @'Traversal'@ targetting all the immediate textual children of a
  --   node.
  --
  -- @
  -- > "<body>hello<br />this is my homepage</body>" ^.. _DOM . traverse . contents
  -- ["hello","this is my homepage"]
  -- @
  contents :: Traversal' e str

instance HasContents (Node str) str where
  contents = _Element . contents

instance HasContents (Element str) str where
  contents = children . traverse . text

allContents :: (HasContents e str, Plated e) => Fold e str
allContents = to universe . traverse . contents

elements :: Fold (Node str) (Element str)
elements = element . to universe . folded
