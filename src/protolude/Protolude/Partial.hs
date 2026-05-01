module Protolude.Partial
  ( head,
    init,
    tail,
    last,
    foldl,
    foldr,
    foldl',
    foldr',
    foldr1,
    foldl1,
    cycle,
    maximum,
    minimum,
    (!!),
    sum,
    product,
    fromJust,
    read,
  )
where

import Data.Foldable (foldl, foldl', foldl1, foldr, foldr', foldr1, product, sum)
import Data.List ((!!), cycle, head, init, last, maximum, minimum, tail)
import Data.Maybe (fromJust)
import Text.Read (read)
