-- | A centralized module ready for use to mount components into documentation pages.

module Docs.Internal.Component where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Docs.Components.Dropdown as Dropdown
import Docs.Components.Typeahead as Typeahead
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

----------
-- Component Types

type State = Unit
type Input = Unit
type Message = Void

data Query a = NoOp a

type Component m = H.Component HH.HTML Query Input Void m
type DSL m = H.HalogenM State Query Slots Void m
type HTML m = H.ComponentHTML Query Slots m

type Slots =
  ( typeahead :: Typeahead.Slot Unit
  , dropdown :: Dropdown.Slot Unit
  )

_typeahead = SProxy :: SProxy "typeahead"
_dropdown = SProxy :: SProxy "dropdown"

----------
-- Built components

typeahead
  :: ∀ m
   . MonadAff m
  => Component m
typeahead =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    eval :: Query ~> DSL m
    eval (NoOp a) = pure a

    render :: Unit -> HTML m
    render _ = HH.slot _typeahead unit Typeahead.component { items: users, keepOpen: false } (const Nothing)

    users :: Array String
    users =
      [ "Lyndsey Duffield"
      , "Chris Pine"
      , "Kevin Hart"
      , "Dave Chappelle"
      , "Hannibal Buress"
      , "Rico Suave"
      ]

dropdown :: ∀ m. MonadAff m => Component m
dropdown =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    eval :: Query ~> DSL m
    eval (NoOp a) = pure a

    render :: Unit -> HTML m
    render _ = HH.slot _dropdown unit Dropdown.component { items: users } (const Nothing)

    users :: Array String
    users =
      [ "Lyndsey Duffield"
      , "Chris Pine"
      , "Kevin Hart"
      , "Dave Chappelle"
      , "Hannibal Buress"
      , "Rico Suave"
      ]
