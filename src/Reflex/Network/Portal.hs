{-# LANGUAGE ConstraintKinds, GADTs, NoImplicitPrelude, RecursiveDo, RankNTypes #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Reflex.Network.Portal where
--   PortalConstraints, Porter, Portal
-- , enterPortal, openPortal, linkPortal
-- )

import Prelude (succ)
import Control.Applicative (pure)
import Control.Monad.Fix (MonadFix)
import Data.Bool (Bool(True, False), bool)
import Data.Int (Int)
import Data.Function (const, id, ($), (.))
import Data.Functor (fmap, (<$>), (<$))
import Data.Map (Map, elemAt, insert, singleton, toList, updateAt)
import Data.Maybe (Maybe(Just, Nothing))
import qualified Data.Map as M
import Data.Tuple (snd, uncurry)
import Reflex (
    Dynamic, Event, Reflex
  , Adjustable, MonadHold, PostBuild
  , fmapMaybe, holdDyn, foldDyn, leftmost
  )
import Reflex.NotReady.Class (NotReady)
import Reflex.Network (networkView)

import Util ((<&>))

-- TODO Elaborate this into a full blown data structure
singleCaseDyn :: forall t m a. (MonadFix m, MonadHold t m, Reflex t)
  => Map Int (Event t a)
  -> m (Dynamic t (Map Int Bool))
singleCaseDyn mevs =
  let e_k = leftmost $ fmap (uncurry (<$)) $ toList mevs
      falseMap = const False <$> mevs
  in  foldDyn (\i _ -> updateAt (const $ const $ Just True) i falseMap) falseMap e_k

type PortalConstraints t m = (Adjustable t m, MonadFix m, NotReady t m, PostBuild t m)

-- | A container for a widget that will be placed in any one of the different `Portal`s.
data Porter t m a where
  Porter :: PortalConstraints t m =>
    { _porter_map :: Dynamic t (Map Int Bool)
    , _porter_widget :: m a
    , _porter_value :: Dynamic t (Maybe a)
    -- , _porter_portalEntered :: Event t Int
    -- , _porter_portalExited :: Event t Int
    } -> Porter t m a

-- | An incrementaly built structure used to place the `Porter` widget in any one of its composing nodes.
data Portal t m a where
  Portal :: PortalConstraints t m =>
    { _portal_key :: Int
    , _portal_map :: Map Int (Event t ()) -- Dynamic t (Map Int (Event t ()))
    , _portal_updates :: [Event t a]
    , _portal_porter :: Porter t m a
    -- , _portal_entered :: Event t ()
    -- , _portal_exited :: Event t ()
    } -> Portal t m a

-- | View the widget whenever the flag is set for the particular node.
portalView :: forall t m a. Reflex t => Int -> Porter t m a -> m (Event t a)
portalView i pr@(Porter _ _ _) =
  let d = snd . elemAt i <$> _porter_map pr
  in  fmapMaybe id <$> networkView (d <&> bool (pure Nothing) (Just <$> _porter_widget pr))
  -- render placeholder? (pure Nothing)

-- data PortalEventTag
--   = PortalEnteredTag
--   | PortalExitedTag
--   | PortalClosedTag

-- data PortalEventName :: PortalEventTag -> * where
--   PortalEntered :: PortalEventTag 'PortalEnteredTag
--   PortalExited  :: PortalEventTag 'PortalExitedTag
--   PortalClosed  :: PortalEventTag 'PortalClosedTag

-- type family PortalEventResultType (en :: PortalEventTag) :: * where
--   EventResultType 'PortalEnteredTag = Int
--   EventResultType 'PortalExitedTag = Int
--   EventResultType 'PortalClosedTag = Int

-- | `enterPortal` wraps a widget to be rendered by the Portal context.
enterPortal :: (MonadHold t m, PortalConstraints t m)
  => Portal t m a
  -> m a
  -> m (Porter t m a)
enterPortal pl w = do
  d_mp <- singleCaseDyn $ _portal_map pl
  d_v  <- holdDyn Nothing $ fmap Just $ leftmost $ _portal_updates pl
  pure $ Porter d_mp w d_v

-- | `openPortal` creates a context on which to `Porter` wrapped widget can be
-- rendered whenever the fed `Event` occurs.
openPortal :: forall t m a. PortalConstraints t m
  => Event t ()
  -> Porter t m a
  -> m (Portal t m a)
openPortal e pr = do
  let i = 0
  e_x <- portalView i pr
  pure $ Portal i (singleton i e) [e_x] pr

-- openFunctionalPortal :: Event t b -> (b -> m a) -> m (Portal t m a)
-- linkFunctionalPortal :: Event t b -> (b -> m a) -> m (Portal t m a)

-- | `linkPortal` chains a new render context the given Portal for rendering
-- whenever the fed `Event` occurs.
linkPortal :: forall t m a. PortalConstraints t m
  => Event t ()
  -> Portal t m a
  -> m (Portal t m a)
linkPortal e (Portal k m evs pr) = do
  let i = succ k
  e_x <- portalView i pr
  pure $ Portal i (insert i e m) (e_x : evs) pr

-- EVENTS
-- PortalEntered
-- PortalExited
