module Foreign.Emacs
  ( Lisp(..)
  , Emacs
  , Buffer(..)
  , getBuffer
  , putBuffer
  , modifyBuffer
  , eval
  , eval_
  )
  where

import           Data.AttoLisp
import           Foreign.Emacs.Internal
