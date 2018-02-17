module Data.Machine.Moore.Trans where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Monad.Cont (class MonadCont)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT)
--TODO
--import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Newtype (class Newtype)

newtype MooreT s m o = MooreT (StateT s (MaybeT m) o)

derive instance newtypeMooreT :: Newtype (MooreT s m o) _

derive newtype instance functorMooreT :: Functor m => Functor (MooreT s m)

derive newtype instance applyMooreT :: Monad m => Apply (MooreT s m)

derive newtype instance applicativeMooreT :: Monad m => Applicative (MooreT s m)

derive newtype instance altMooreT :: (Monad m, Alt m) => Alt (MooreT s m)

derive newtype instance plusMooreT :: (Monad m, Plus m) => Plus (MooreT s m)

derive newtype instance alternativeMooreT
  :: (Monad m, Alternative m) => Alternative (MooreT s m)

derive newtype instance bindMooreT :: Monad m => Bind (MooreT s m)

derive newtype instance monadMooreT :: Monad m => Monad (MooreT s m)

derive newtype instance monadRecMooreT :: MonadRec m => MonadRec (MooreT s m)

derive newtype instance monadZeroMooreT :: MonadZero m => MonadZero (MooreT s m)

derive newtype instance monadPlusMooreT :: MonadPlus m => MonadPlus (MooreT s m)

--TODO
--derive newtype instance monadTransMooreT :: MonadTrans (MooreT s)

derive newtype instance lazyMooreT :: Lazy (MooreT s m a)

derive newtype instance monadEffState
  :: MonadEff eff m => MonadEff eff (MooreT s m)

derive newtype instance monadContMooreT
  :: MonadCont m => MonadCont (MooreT s m)

derive newtype instance monadThrowMooreT
  :: MonadThrow e m => MonadThrow e (MooreT s m)

derive newtype instance monadErrorMooreT
  :: MonadError e m => MonadError e (MooreT s m)

derive newtype instance monadAskMooreT
  :: MonadAsk r m => MonadAsk r (MooreT s m)

derive newtype instance monadReaderMooreT
  :: MonadReader r m => MonadReader r (MooreT s m)

derive newtype instance monadStateMooreT
  :: Monad m => MonadState s (MooreT s m)

derive newtype instance monadTellMooreT
  :: MonadTell w m => MonadTell w (MooreT s m)

derive newtype instance monadWriterMooreT
  :: MonadWriter w m => MonadWriter w (MooreT s m)
