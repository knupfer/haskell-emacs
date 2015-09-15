{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.Emacs.Internal where

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Data.AttoLisp
import qualified Data.ByteString.Lazy.Char8 as B hiding (length)
import qualified Data.ByteString.Lazy.UTF8  as B (length)
import           Data.Monoid                ((<>))

class ToEmacs a where
  toEmacs :: a -> Either (Emacs Lisp) Lisp

instance ToLisp a => ToEmacs a where
  toEmacs = Right . toLisp

instance ToLisp a => ToEmacs (Emacs a) where
  toEmacs em = Left $ fmap toLisp em

newtype Emacs a = EmacsInternal
  {fromEmacs :: ReaderT (MVar Lisp, Chan B.ByteString) IO a}

instance Functor Emacs where
  fmap f (EmacsInternal r) = EmacsInternal $ fmap f r

instance Applicative Emacs where
  pure a = EmacsInternal $ pure a
  (EmacsInternal f) <*> (EmacsInternal x) = EmacsInternal $ f <*> x

instance Monad Emacs where
  (EmacsInternal x) >>= f = EmacsInternal $ x >>= fromEmacs . f

instance NFData (Emacs Lisp) where
  rnf (EmacsInternal _) = ()

eval :: (ToLisp a, FromLisp a) => [Lisp] -> Emacs a
eval lsp = EmacsInternal $ do
  (mvar, chan) <- ask
  liftIO $ writeChan chan cmd
  List (a:_) <- liftIO $ takeMVar mvar
  case fromLisp a of
       Success b -> return b
       Error msg -> error msg
  where cmd = let x = encode $ List [ Symbol "process-send-string"
                                    , Symbol "haskell-emacs--proc"
                                    , List [ Symbol "format"
                                           , String "|%S"
                                           , List [ Symbol "haskell-emacs--no-properties"
                                                  , List [ Symbol "list"
                                                           , List lsp]]]]
              in encode [B.length x] <> x

eval_ :: [Lisp] -> Emacs ()
eval_ lsp = EmacsInternal $ do
  (_, chan) <- ask
  liftIO $ writeChan chan cmd
  return ()
  where cmd = let x = encode $ List [Symbol "list", List lsp]
              in encode [B.length x] <> x
