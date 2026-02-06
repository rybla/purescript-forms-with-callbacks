module Main where

import Prelude

import Control.Monad.State (modify_)
import Data.Maybe (Maybe(..), fromMaybe')
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent {} =<< HA.awaitBody)

appComponent :: forall query input output m. MonadAff m => H.Component query input output m
appComponent = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { initTerm:
        Input "name" \name -> Input "age" \age -> Input "message" \message ->
          Output ("Hello, " <> name <> ", age " <> age <> ". Here's your message: " <> show message)
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div [ HP.classes [ HH.ClassName "app" ] ]
      [ HH.div [] [ HH.text $ "initTerm = " <> show state.initTerm ]
      , HH.slot_ (Proxy @"term") 0 termComponent { initTerm: state.initTerm }
      ]

--------------------------------------------------------------------------------

data Term
  = Output String
  | Input String (String -> Term)

instance Show Term where
  show (Output output) = "(Output " <> show output <> ")"
  show (Input label k) = "(Input " <> show label <> " " <> show (k ("{{" <> label <> "}}")) <> ")"

data TermAction
  = SubmitInput (String -> Term)
  | Pass

termComponent :: forall query input output m. MonadAff m => H.Component query (Record (initTerm :: Term | input)) output m
termComponent = H.mkComponent { initialState, eval, render }
  where
  initialState input =
    { oldTerm: input.initTerm
    , mb_newTerm: Nothing @Term
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case _ of
        SubmitInput k -> do
          input <-
            H.getRef refLabel.input
              >>= (fromMaybe' (\_ -> unsafeCrashWith "Impossible: no element at input RefLabel") >>> pure)
              >>= (HTMLInputElement.fromElement >>> pure)
              >>= (fromMaybe' (\_ -> unsafeCrashWith "Impossible: KeyboardEvent target is not an HTMLInputElement.") >>> pure)
              >>= (HTMLInputElement.value >>> liftEffect)
          modify_ _ { mb_newTerm = Just (k input) }
        Pass -> pure unit
    }

  refLabel =
    { input: H.RefLabel "input"
    }

  render state = case state.oldTerm of
    Output output ->
      HH.div [ HP.classes [ HH.ClassName "term", HH.ClassName "Output" ] ]
        [ HH.text output ]
    Input label k ->
      HH.div [ HP.classes [ HH.ClassName "term", HH.ClassName "Input" ] ]
        [ HH.text label
        , HH.input
            [ HP.classes [ HH.ClassName "input" ]
            , HP.ref refLabel.input
            , HE.onKeyDown
                ( \e ->
                    if KeyboardEvent.key e == "Enter" then
                      SubmitInput k
                    else
                      Pass
                )
            ]
        , HH.button
            [ HP.classes [ HH.ClassName "submit" ]
            , HE.onClick (const (SubmitInput k))
            ]
            [ HH.text "Submit" ]
        ]

