module Junk where

import Prelude
  

a = 
    let a = "a"
        b = "b" in
    a <> b <> c
    where 
        c = "c"

b a = (_ == 3)
