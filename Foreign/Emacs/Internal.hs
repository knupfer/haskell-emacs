module Foreign.Emacs.Internal where

import           Control.Concurrent
import           Control.Monad.Trans.Reader
import           Data.AttoLisp
import qualified Data.ByteString.Lazy.Char8 as B

newtype Emacs a = EmacsInternal
  {fromEmacs :: ReaderT (MVar Lisp, Chan B.ByteString) IO a}

instance Functor Emacs where
  fmap f (EmacsInternal r) = EmacsInternal $ fmap f r

instance Applicative Emacs where
  pure a = EmacsInternal $ pure a
  (EmacsInternal f) <*> (EmacsInternal x) = EmacsInternal $ f <*> x

instance Monad Emacs where
  (EmacsInternal x) >>= f = EmacsInternal $ x >>= fromEmacs . f
