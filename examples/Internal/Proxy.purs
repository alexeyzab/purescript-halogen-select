-- | A proxy that hides both the Query and Message of wrapped component.
-- | Adapted from `Halogen.Component.Proxy` and `Halogen.Storybook.Proxy`.

module Docs.Internal.Proxy
  ( ProxyS
  , proxy
  ) where

import Prelude

import Data.Const (Const(..))
import Data.Coyoneda (Coyoneda, unCoyoneda)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Prim.Row as Row

data ProxyS f i a
  = Query (Coyoneda f a)

-- | A proxy that hides both the Query and Message of wrapped component.
proxy
  :: ∀ f i o m
   . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS (Const Void) i) i Void m
proxy = proxyEval (const (absurd <<< un Const)) (SProxy :: SProxy "slot")

proxyEval
  :: ∀ f g i o cs m t0 sym
   . IsSymbol sym
  => Row.Cons sym (H.Slot f o Unit) t0 cs
  => (∀ a b. (b -> a) -> g b -> H.HalogenM i (ProxyS g i) cs Void m a)
  -> SProxy sym
  -> H.Component HH.HTML f i o m
  -> H.Component HH.HTML (ProxyS g i) i Void m
proxyEval evalQuery sym component =
  H.component
    { initialState: identity
    , render: render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    render :: i -> H.ComponentHTML (ProxyS g i) cs m
    render i = HH.slot sym unit component i (const Nothing)

    eval :: ProxyS g i ~> H.HalogenM i (ProxyS g i) cs Void m
    eval (Query iq) = unCoyoneda evalQuery iq
