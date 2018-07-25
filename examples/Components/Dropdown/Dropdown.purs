module Docs.Components.Dropdown where

import Prelude

import Data.Array (difference, mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Docs.CSS as CSS
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Select as Select
import Select.Setters as Setters

type State =
  { items :: Array String
  , text :: String }

type Input =
  { items :: Array String }

data Query a
  = HandleSelect (Select.Message Query String) a

type Message = Void

type Slots m =
  ( select :: Select.Slot Query () String m Unit )
_select = SProxy :: SProxy "select"

type Slot = H.Slot Query Void

component :: ∀ m. MonadAff m => H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
    initialState :: Input -> State
    initialState i = { items: i.items, text: "Select an option" }

    eval :: Query ~> H.HalogenM State Query (Slots m) Message m
    eval = case _ of
      HandleSelect (Select.Selected item) a -> do
        st <- H.get
        _ <- H.query _select unit $ Select.setVisibility Select.Off
        _ <- H.query _select unit $ Select.replaceItems $ difference st.items [ item ]
        H.modify_ _ { text = item }
        pure a

      HandleSelect other a -> pure a

    render :: State -> H.ComponentHTML Query (Slots m) m
    render st =
      HH.div
        [ HP.class_ $ HH.ClassName "w-full" ]
        [ HH.slot _select unit Select.component input (HE.input HandleSelect) ]
      where
        input =
          { initialSearch: Nothing
          , debounceTime: Nothing
          , inputType: Select.Toggle
          , items: difference st.items [ st.text ]
          , render: renderDropdown
          }

        renderDropdown :: Select.State String -> Select.HTML Query () String m
        renderDropdown state = HH.div_ [ renderToggle, renderMenu ]
          where
            renderToggle =
              HH.button
                ( Setters.setToggleProps [ HP.classes CSS.button ] )
                [ HH.text st.text ]

            renderMenu =
              HH.div [ HP.classes CSS.menu ]
              $ if state.visibility == Select.Off
                then []
                else [ renderContainer $ renderItem `mapWithIndex` state.items ]
              where
                renderContainer html =
                  HH.div
                    ( Setters.setContainerProps [ HP.classes CSS.itemContainer ] )
                    [ HH.ul [ HP.classes CSS.ul ] html ]

                renderItem index item =
                  HH.li
                    ( Setters.setItemProps index props )
                    [ HH.text item ]
                  where
                    props =
                      [ HP.classes
                        ( CSS.li <>
                          if state.highlightedIndex == Just index
                            then [ HH.ClassName "bg-grey-lighter" ]
                            else []
                        )
                      ]

