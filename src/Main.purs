module Main where

import Prelude

import Control.Monad.State (modify_)
import Data.Foldable (fold)
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
    { term:
        makeInputTerm "name" \name -> makeInputTerm "age" \age -> makeInputTerm "message" \message ->
          makeOutputTerm ("Hello, " <> name <> ", age " <> age <> ". Here's your message: " <> show message)
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div [ HP.classes [ HH.ClassName "app" ] ]
      [ HH.div [ HP.classes [ HH.ClassName "row" ] ] $ fold
          [ [ HH.span_ [ HH.text "term =" ] ]
          , displayTerm state.term
          ]
      , HH.slot_ (Proxy @"term") 0 termComponent { term: state.term }
      ]

--------------------------------------------------------------------------------

data Term
  = OutputTerm OutputTerm
  | InputTerm InputTerm

type OutputTerm =
  { content :: String
  }

type InputTerm =
  { label :: String
  , k :: String -> Term
  }

makeOutputTerm :: String -> Term
makeOutputTerm content = OutputTerm { content }

makeInputTerm :: String -> (String -> Term) -> Term
makeInputTerm label k = InputTerm { label, k }

instance Show Term where
  show (OutputTerm { content }) = "(Output " <> show content <> ")"
  show (InputTerm { label, k }) = "(Input " <> show label <> " " <> show (k ("{{" <> label <> "}}")) <> ")"

displayTerm :: forall w i. Term -> Array (HH.HTML w i)
displayTerm (OutputTerm { content }) =
  [ HH.span [ HP.classes [ HH.ClassName "punctuation" ] ] [ HH.text "(" ]
  , HH.span [ HP.classes [ HH.ClassName "constructor" ] ] [ HH.text "Output" ]
  , HH.span [ HP.classes [ HH.ClassName "literal" ] ] [ HH.text content ]
  , HH.span [ HP.classes [ HH.ClassName "punctuation" ] ] [ HH.text ")" ]
  ]
displayTerm (InputTerm { label, k }) = fold
  [ [ HH.span [ HP.classes [ HH.ClassName "punctuation" ] ] [ HH.text "(" ]
    , HH.span [ HP.classes [ HH.ClassName "constructor" ] ] [ HH.text "Input" ]
    , HH.span [ HP.classes [ HH.ClassName "literal" ] ] [ HH.text label ]
    ]
  , displayTerm (k ("{{" <> label <> "}}"))
  , [ HH.span [ HP.classes [ HH.ClassName "punctuation" ] ] [ HH.text ")" ] ]
  ]

data TermState
  = OutputTermState
      { term :: OutputTerm
      }
  | InputTermState
      { term :: InputTerm
      , result :: Maybe { content :: String, term :: Term }
      }

initTermState :: Term -> TermState
initTermState (OutputTerm term) = OutputTermState { term }
initTermState (InputTerm term) = InputTermState { term, result: Nothing }

data TermAction
  = SubmitInput (String -> TermState)
  | Pass

termComponent :: forall query input output m. MonadAff m => H.Component query (Record (term :: Term | input)) output m
termComponent = H.mkComponent { initialState, eval, render }
  where
  initialState input =
    { termState: initTermState input.term
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
          modify_ _ { termState = k input }
        Pass -> pure unit
    }

  refLabel =
    { input: H.RefLabel "input"
    }

  render state = case state.termState of
    OutputTermState termState ->
      HH.div [ HP.classes [ HH.ClassName "term", HH.ClassName "Output" ] ]
        [ HH.div [ HP.classes [ HH.ClassName "row" ] ]
            [ HH.span [ HP.classes [ HH.ClassName "item" ] ]
                [ HH.text termState.term.content ]
            ]
        ]
    InputTermState termState ->
      HH.div [ HP.classes [ HH.ClassName "term", HH.ClassName "Input" ] ]
        case termState.result of
          Nothing ->
            [ HH.div [ HP.classes [ HH.ClassName "row" ] ]
                [ HH.span [ HP.classes [ HH.ClassName "item" ] ]
                    [ HH.text termState.term.label ]
                , HH.input
                    [ HP.classes [ HH.ClassName "input" ]
                    , HP.ref refLabel.input
                    , HE.onKeyDown
                        ( \e ->
                            if KeyboardEvent.key e == "Enter" then
                              SubmitInput \s -> InputTermState termState { result = Just { term: termState.term.k s, content: s } }
                            else
                              Pass
                        )
                    ]
                , HH.button
                    [ HP.classes [ HH.ClassName "submit" ]
                    , HE.onClick (const (SubmitInput \s -> InputTermState termState { result = Just { term: termState.term.k s, content: s } }))
                    ]
                    [ HH.text "Submit" ]
                ]
            ]
          Just result ->
            [ HH.div [ HP.classes [ HH.ClassName "row" ] ]
                [ HH.span [ HP.classes [ HH.ClassName "item" ] ]
                    [ HH.text termState.term.label ]
                , HH.span [ HP.classes [ HH.ClassName "item" ] ]
                    [ HH.text result.content ]
                ]
            , HH.slot_ (Proxy @"term") 0 termComponent { term: result.term }
            ]

