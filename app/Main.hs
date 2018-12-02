{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs
           , NoImplicitPrelude, RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Control.Applicative (pure)
import Control.Lens ((.~))
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.Function (($), (&))
import GHCJS.DOM.Types (JSM)
import Reflex (getPostBuild, leftmost)
import Reflex.Dom (
    Widget
  , divClass, el, elAttr, elAttr'
  , blank, button, initialAttributes, inputElement, text
  , (=:)
  )
import Reflex.Dom.Core (mainWidgetWithCss)
import Language.Javascript.JSaddle.Warp (run)
import System.IO (IO)

import Reflex.Dom.DragAndDrop
import Reflex.Network.Portal


portalExample :: Widget x ()
portalExample = do
  el "h2" $ text "Portal"
  divClass "example-portal__portalarea" $ mdo
    porter <- enterPortal nxtPortal $ do
      elAttr "label" ("for" =: "name") $ text "Porter"
      inputElement $ def & initialAttributes .~ ("placeholder" =: "Sam")
    fstPortal <- do
      e_pb <- getPostBuild
      rec
        p1   <- divClass "example-portal__portal" $ openPortal (leftmost [e_pb, e_b1]) porter
        e_b1 <- button "TELEPORT"
      pure p1
    nxtPortal <- do
      rec
        p2   <- divClass "example-portal__portal" $ linkPortal e_b2 fstPortal
        e_b2 <- button "TELEPORT"
      pure p2
    blank

dndExample :: Widget x ()
dndExample = mdo
  el "h2" $ text "Drag and Drop"
  (el_dragzone, _) <- elAttr' "div" ("class" =: "example-dnd__dragzone") $ do
  -- (el_dragzone, _) <- elAttr' "div" ("class" =: "example-dnd__dragzone") $ \el_dragzone -> do
    rec
      fstDropzone <- joinDropzone def dr
      nxtDropzone <- joinDropzone def fstDropzone
      dr <-
        divClass "example-dnd__draggable-container" $ do
          makeDraggable el_dragzone nxtDropzone $
            elAttr' "div" ("class" =: "example-dnd__draggable-box") blank
      -- TODO Close draggable/dropzone circuit
      -- dr_oth <- makeDraggable el_dragzone nxtDropzone $
      --   elAttr' "div" ("class" =: "example-dnd__draggable-box other-box") blank
      -- dr_doc <- makeDocumentDraggable nxtDropzone $
      --   elAttr' "div" ("class" =: "example-dnd__draggable-box document-box") blank
    blank
  blank

widget :: JSM ()
widget = mainWidgetWithCss $(embedFile "app/app.css") $ do
  el "header" $ el "h1" $ text "Reflex DnD Examples"
  el "main" $ do
    -- el "aside" $ el "nav" $ do
    --   elAttr "a" ("href" =: "#exampleDnd")    $ text "Basic Drag and Drop"
    --   elAttr "a" ("href" =: "#examplePortal") $ text "Basic Portal"
    el "article" $ do
      elAttr "section" ("id" =: "exampleDnd")    dndExample
      elAttr "section" ("id" =: "examplePortal") portalExample

main :: IO ()
main =
  let port = 7357
  in  run port widget
