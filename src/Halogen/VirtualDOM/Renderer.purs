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

module Halogen.VirtualDOM.Renderer (renderHTML) where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Aff.Effects (HalogenEffects)
import Halogen.HTML.Core (HTML(..))
import Halogen.VDom (ElemSpec(..), VDom(..), runGraft)
import Halogen.VDom.DOM.Prop (ElemRef(..), Prop(..))
import Halogen.Query.InputF (InputF)
import Halogen.VirtualDOM.Internal as V

-- | Render a `HTML` document to a virtual DOM node
-- |
-- | The first argument is an event handler.
renderHTML
  :: forall p i m eff
   . Monad m
  => (InputF Unit i -> Eff (HalogenEffects eff) Unit)
  -> (p -> m V.VTree)
  -> HTML p i
  -> m V.VTree
renderHTML driver handleSlot (HTML vdom) = go null vdom
  where
  go :: Nullable String -> VDom (Array (Prop (InputF Unit i))) p -> m V.VTree
  go nk = case _ of
    Text s ->
      pure $ V.vtext s
    Elem (ElemSpec ns name props) els -> do
      els' <- traverse (go null) els
      pure $ V.vnode
        (toNullable $ unwrap <$> ns)
        (unwrap name)
        nk
        (foldMap (renderProp driver) props) els'
    Keyed (ElemSpec ns name props) els -> do
      els' <- traverse (\(Tuple k el) -> go (toNullable (Just k)) el) els
      pure $ V.vnode
        (toNullable $ unwrap <$> ns)
        (unwrap name)
        nk
        (foldMap (renderProp driver) props) els'
    Widget p ->
      handleSlot p
    Grafted gd ->
      go nk (runGraft gd)

null :: forall a. Nullable a
null = toNullable Nothing

renderProp
  :: forall i eff
   . (InputF Unit i -> Eff (HalogenEffects eff) Unit)
  -> Prop (InputF Unit i)
  -> V.Props
renderProp driver = case _ of
  Property name value ->
    runFn2 V.prop name value
  Attribute ns name value ->
    let attrName = maybe "" (\ns' -> unwrap ns' <> ":") ns <> name
    in runFn2 V.attr attrName value
  Handler ev k ->
    runFn2 V.handlerProp ev (maybe (pure unit) driver <<< k)
  Ref f ->
    V.refProp (maybe (pure unit) driver <<< f <<< either Removed Created)
