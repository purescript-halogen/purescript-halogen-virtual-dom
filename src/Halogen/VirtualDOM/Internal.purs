{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.
module Halogen.VirtualDOM.Internal
  ( VTree
  , Patch
  , Props
  , prop
  , attr
  , handlerProp
  , refProp
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  , widget
  ) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Monoid (class Monoid)
import Data.Either (Either(..))
import Data.Nullable (Nullable)
import Data.Function.Uncurried (Fn2, runFn2)

import DOM (DOM)
import DOM.Event.Types (EventType)
import DOM.HTML.Types (HTMLElement)
import DOM.Node.Types (Element)

-- | Virtual DOM nodes
data VTree

-- | Patch sets, used to update the DOM
data Patch

-- | Property collections
data Props

-- | Create a property from a key/value pair.
foreign import prop :: forall value. Fn2 String value Props

foreign import attr :: Fn2 String String Props

-- | Create a property from an event handler.
foreign import handlerProp :: forall eff event. Fn2 EventType (event -> Eff eff Unit) Props

-- | Create a property from a ref.
refProp :: forall eff. (Either Element Element -> Eff eff Unit) -> Props
refProp = refPropImpl Left Right

foreign import refPropImpl
  :: forall eff
   . (forall a b. a -> Either a b)
  -> (forall a b. b -> Either a b)
  -> (Either Element Element -> Eff eff Unit)
  -> Props

foreign import concatProps :: Fn2 Props Props Props

foreign import emptyProps :: Props

instance semigroupProps :: Semigroup Props where
  append = runFn2 concatProps

instance monoidProps :: Monoid Props where
  mempty = emptyProps

-- | Create a DOM node from a virtual DOM tree
foreign import createElement :: forall eff. VTree -> Eff (dom :: DOM | eff) HTMLElement

-- | Calculate the differences between two virtual DOM trees
foreign import diff :: VTree -> VTree -> Patch

-- | Apply a set of patches to the DOM
foreign import patch :: forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement

-- | Create a virtual DOM tree which represents a single text node
foreign import vtext :: String -> VTree

-- | Create a virtual DOM tree which represents an element with properties
-- | (namespace, tag name, key, properties, children).
foreign import vnode :: Nullable String -> String -> Nullable String -> Props -> Array VTree -> VTree

foreign import widget :: Int -> HTMLElement -> VTree
