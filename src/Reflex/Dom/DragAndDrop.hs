{-# LANGUAGE GADTs, NoImplicitPrelude, RecursiveDo, RankNTypes, ViewPatterns, DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module Reflex.Dom.DragAndDrop where

import Prelude (Num)
import Control.Applicative (pure, (<*>))
import Control.Monad ((=<<), (>>=))
import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (munzip)
import Data.Bool (Bool(True, False), (&&))
import Data.Default (Default, def)
import Data.Int (Int)
import Data.Function (const, flip, id, ($), (.))
import Data.Functor (Functor, (<$>), (<$), ($>))
import Data.Map (Map, empty)
import Data.Maybe (Maybe(Just, Nothing), maybe, isJust)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Tuple (fst, uncurry)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex
import Reflex.Dom
import Reflex.Host.Class
import Reflex.Network
import Text.Show (Show, show)

import qualified GHCJS.DOM.DataTransfer        as DOM
import qualified GHCJS.DOM.Element             as DOM
import qualified GHCJS.DOM.EventM              as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLElement         as DOM
import qualified GHCJS.DOM.MouseEvent          as DOM
import qualified GHCJS.DOM.Types               as DOM (uncheckedCastTo)

import Reflex.Network.Portal (Porter, Portal, enterPortal, openPortal, linkPortal, _porter_value)
import Util ((<&>))

-- TODO Simplify constraints

-- DRAGGABLE

-- | Widget that can be dragged around a designated Dragzone
-- CHECK Not an actual widget
data Draggable t m a where
  Draggable ::
    ( Adjustable t m
    , MonadFix m
    , MonadJSM m
    ) =>
      { _draggable_porter :: Porter t m (Element EventResult (DomBuilderSpace m) t, a)
      , _draggable_drop :: Event t (Int, Int)
      } -> Draggable t m a

-- DRAGZONE

-- | Area around which an element can be dragged.
-- * Mouse drops it whenever it loses control
-- * Returns to the original position when mouse loses control
data Dragzone t m where
  Dragzone ::
    ( Adjustable t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    , MonadFix m
    , PostBuild t m
    ) => 
      { _dragzone_widget :: m () -- CHECK
      -- _dragzone_dropOnLeave
      } -> Dragzone t m

-- DROPZONE

data DropzoneConfig t m where
  DropzoneConfig :: (DomBuilder t m, Reflex t) =>
    { _dropzoneConfig_enabled   :: Behavior t Bool
    , _dropzoneConfig_container :: forall a. m a -> m (Element EventResult (DomBuilderSpace m) t, a)
    } -> DropzoneConfig t m

instance (DomBuilder t m, Reflex t) => Default (DropzoneConfig t m) where
  def = DropzoneConfig 
    { _dropzoneConfig_enabled   = constant True
    , _dropzoneConfig_container = elAttr' "div" ("class" =: "reflex-dnd-dropzone")
    }

-- | Area element in which a draggable can be dropped.
data Dropzone t m a where
  Dropzone ::
    ( Adjustable t m
    , DomBuilderSpace m ~ GhcjsDomSpace
    , MonadFix m    
    , PostBuild t m
    ) =>
      { _dropzone_portal :: Portal t m (Element EventResult (DomBuilderSpace m) t, a)
      -- , _dropzone_gate :: Behavior t Bool
      , _dropzone_draggable :: Draggable t m a
      } -> Dropzone t m a

data Dragging = Dragging
  { _dragging_start :: (Int, Int)
  , _dragging_position :: (Int, Int)
  -- , dragEnd :: Maybe (Int, Int)
  }

-- isDragging :: Maybe Dragging -> Bool
-- isDragging = maybe False (isJust . dragEnd)

-- EVENTS

-- dragstart
-- drag
-- dragenter
-- dragleave
-- dragover
-- drop
-- dragend

makeDraggable :: forall t m a el.
  ( Adjustable t m
  , DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace

  , DomEventType el 'MouseleaveTag ~ ()
  , DomEventType el 'MousemoveTag ~ (Int, Int)
  , DomEventType el 'MouseupTag ~ (Int, Int)
  , HasDomEvent t el 'MouseleaveTag
  , HasDomEvent t el 'MousemoveTag
  , HasDomEvent t el 'MouseupTag

  , MonadFix m
  , MonadHold t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , MonadReflexCreateTrigger t m
  , NotReady t m
  , PerformEvent t m
  , TriggerEvent t m
  -- DEBUG
  , PostBuild t m
  ) => el
    -> Dropzone t m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
    -> m (Dropzone t m a)
makeDraggable el_dragzone dz w = mdo

  let d_mv = _porter_value pr
      b_mv = current d_mv
      e_mousedown  = switch (maybe never (domEvent Mousedown . fst) <$> b_mv)
      e_mouseup    = domEvent Mouseup    el_dragzone
      e_mouseleave = domEvent Mouseleave el_dragzone
      e_mousemove  = domEvent Mousemove  el_dragzone
      e_drop    = gate (current (isJust <$> d_drag)) e_mouseup
      e_dragend = leftmost [e_drop $> (), e_mouseleave $> ()]

  d_drag <- foldDyn id Nothing $ leftmost
    [ e_mousedown <&> \p _  -> pure (Dragging p p)
    , e_mousemove <&> \p md -> md <&> \d -> d { _dragging_position = p }
    , e_dragend    $> const Nothing
    ]

  pr <- enterPortal (_dropzone_portal dz) w
  -- DEBUG
  e_pb <- getPostBuild

  (el, pl) <- -- networkView d_isDragging
    -- TODO Scrap `reflex-dnd-drag` class?
    elDynAttr' "div" (d_drag <&> \d -> "class" =: "reflex-dnd-drag" <> "style" =: ("user-select: none;" <> styleDraggable (_dragging_position <$> d))) $
      openPortal (leftmost [e_mousedown $> (), e_pb]) pr
      -- openPortal (e_mousedown $> ()) pr

  pure $ Dropzone pl $ Draggable pr e_drop

styleDraggable :: (Show a, Num a) => Maybe (a, a) -> Text
styleDraggable
  =  maybe "" $ \(x, y) -> pack $ "position: fixed;"
  <> "pointer-events: none;"
  <> "left: " <> show x <> "px;"
  <> "top: "  <> show y <> "px;"

makeDragzone ::
  ( Adjustable t m
  , DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , MonadFix m
  , MonadHold t m
  , MonadJSM m
  , PostBuild t m
  , TriggerEvent t m
  ) => m (Dragzone t m)
makeDragzone = pure (Dragzone blank)

joinDropzone :: forall t m a.
  ( Adjustable t m
  , DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , MonadJSM m
  , MonadJSM (Performable m)
  , MonadHold t m
  -- DEBUG
  -- , PostBuild t m
  ) => DropzoneConfig t m
    -> Dropzone t m a
    -> m (Dropzone t m a)
joinDropzone dzConfig (Dropzone pl dr) = do
  let e_drDrop = _draggable_drop dr
  rec
    -- TODO Make Dragzone a higher-order component for DropzoneConfig or customize class
    (el_dz, pl_dz) <- _dropzoneConfig_container dzConfig $ do
      d_mouseover <- holdDyn False $ leftmost
        [ domEvent Mouseenter el_dz $> True
        , domEvent Mouseleave el_dz $> False
        ]
      let g = (&&) <$> _dropzoneConfig_enabled dzConfig <*> current d_mouseover
      linkPortal (gate g e_drDrop $> ()) pl
  pure $ Dropzone pl_dz dr

makeDocumentDragzone ::
  ( DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadJSM m
  , TriggerEvent t m
  ) => m (DocumentDragzone t)
makeDocumentDragzone = askDocument >>= \doc -> do
  e_mouseup    <- wrapDomEvent doc (`DOM.on` DOM.mouseUp)    mouseEvent
  e_mousemove  <- wrapDomEvent doc (`DOM.on` DOM.mouseMove)  mouseEvent
  e_mouseleave <- wrapDomEvent doc (`DOM.on` DOM.mouseLeave) (pure ())
  pure $ DocumentDragzone e_mouseup e_mousemove e_mouseleave
  where
    mouseEvent = DOM.event >>= \e -> (,) <$> DOM.getClientX e <*> DOM.getClientY e

data DocumentDragzone t = DocumentDragzone
  { documentDragzone_mouseup    :: Event t (Int, Int)
  , documentDragzone_mousemove  :: Event t (Int, Int)
  , documentDragzone_mouseleave :: Event t ()
  }

instance Reflex t => HasDomEvent t (DocumentDragzone t) 'MouseupTag where
  type DomEventType (DocumentDragzone t) 'MouseupTag = EventResultType 'MouseupTag
  domEvent _ = documentDragzone_mouseup

instance Reflex t => HasDomEvent t (DocumentDragzone t) 'MousemoveTag where
  type DomEventType (DocumentDragzone t) 'MousemoveTag = EventResultType 'MousemoveTag
  domEvent _ = documentDragzone_mousemove

instance Reflex t => HasDomEvent t (DocumentDragzone t) 'MouseleaveTag where
  type DomEventType (DocumentDragzone t) 'MouseleaveTag = EventResultType 'MouseleaveTag
  domEvent _ = documentDragzone_mouseleave

makeDocumentDraggable  :: forall t m a el.
  ( Adjustable t m
  , DomBuilder t m
  , DomBuilderSpace m ~ GhcjsDomSpace
  , HasDocument m
  , MonadFix m
  , MonadHold t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , MonadReflexCreateTrigger t m
  , PerformEvent t m
  , TriggerEvent t m
  , PostBuild t m
  ) => Dropzone t m a
    -> m (Element EventResult (DomBuilderSpace m) t, a)
    -> m (Dropzone t m a)
makeDocumentDraggable dz w = makeDocumentDragzone >>= \el -> makeDraggable el dz w
