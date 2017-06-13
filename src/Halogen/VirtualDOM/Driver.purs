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

module Halogen.VirtualDOM.Driver
  ( runUI
  , module Halogen.Aff.Driver
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, modifyRef, newRef, readRef)

import Data.Maybe (Maybe(..))

import DOM.HTML.Types (HTMLElement, htmlElementToNode)
import DOM.Node.Node (appendChild)

import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX, unRenderStateX)
import Halogen.Aff.Effects (HalogenEffects)
import Halogen.Component (ComponentSlot, Component)
import Halogen.HTML.Core (HTML)
import Halogen.Query.InputF (InputF)
import Halogen.VirtualDOM.Internal as V
import Halogen.VirtualDOM.Renderer (renderHTML)

import Halogen.Aff.Driver (HalogenIO)

newtype RenderState s (f :: Type -> Type) (g :: Type -> Type) p o (eff :: # Effect) =
  RenderState
    { keyId :: Int
    , node :: HTMLElement
    , vtree :: V.VTree
    }

-- | This function is the main entry point for a Halogen based UI, taking a root
-- | component, initial state, and HTML element to attach the rendered component
-- | to.
-- |
-- | The returned "driver" function can be used to send actions and requests
-- | into the component hierarchy, allowing the outside world to communicate
-- | with the UI.
runUI
  :: forall f eff i o
   . Component HTML f i o (Aff (HalogenEffects eff))
  -> i
  -> HTMLElement
  -> Aff (HalogenEffects eff) (HalogenIO f o (Aff (HalogenEffects eff)))
runUI component i element = do
  fresh <- liftEff (newRef 0)
  AD.runUI (mkRenderSpec element fresh) component i

mkRenderSpec
  :: forall eff
   . HTMLElement
  -> Ref Int
  -> AD.RenderSpec HTML RenderState eff
mkRenderSpec element fresh =
  { render
  , renderChild
  , removeChild: const (pure unit)
  }
  where

  render
    :: forall s f g p o
     . (forall x. InputF x (f x) -> Eff (HalogenEffects eff) Unit)
    -> (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit) -> Eff (HalogenEffects eff) (RenderStateX RenderState eff))
    -> HTML (ComponentSlot HTML g (Aff (HalogenEffects eff)) p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o eff)
    -> Eff (HalogenEffects eff) (RenderState s f g p o eff)
  render handler child html lastRender = do
    vtree <- renderHTML handler (map getVTree <<< child) html
    case lastRender of
      Nothing -> do
        modifyRef fresh (_ + 1)
        keyId <- readRef fresh
        node <- V.createElement vtree
        _ <- appendChild (htmlElementToNode node) (htmlElementToNode element)
        pure $ RenderState { keyId, vtree, node }
      Just (RenderState r) -> do
        node <- V.patch (V.diff r.vtree vtree) r.node
        pure $ RenderState { keyId: r.keyId, vtree, node }

  getVTree :: RenderStateX RenderState eff -> V.VTree
  getVTree = unRenderStateX \(RenderState { vtree }) -> vtree

  renderChild
    :: forall s f g p o
     . RenderState s f g p o eff
    -> RenderState s f g p o eff
  renderChild (RenderState r) =
    RenderState { keyId: r.keyId, vtree: V.widget r.keyId r.node, node: r.node }
