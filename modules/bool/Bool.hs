{-# LANGUAGE NoImplicitPrelude #-}
module Bool ( bool
            , not
            , otherwise ) where

import           Data.AttoLisp
import qualified Data.Bool     as B
import           Prelude       (Bool (True))

bool :: Lisp -> Lisp -> Bool -> Lisp
bool = B.bool

not :: Bool -> Bool
not = B.not

otherwise :: Bool
otherwise = True
