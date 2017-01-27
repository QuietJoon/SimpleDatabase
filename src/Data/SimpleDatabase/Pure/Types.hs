module Data.SimpleDatabase.Pure.Types (
    module Data.SimpleDatabase.Pure.Types
  , module Data.SimpleDatabase.Types
  ) where


import Data.SimpleDatabase.Types


import Data.HashMap.Strict (HashMap)
import Data.IntMap (IntMap)


type DBII = IntMap Int
type DBSI = HashMap SKey Int
type DBID = IntMap Double
type DBSD = HashMap SKey Double
type DBIS = IntMap SContent
type DBSS = HashMap SKey SContent

type DBIIL = IntMap [Int]
type DBSIL = HashMap SKey [Int]
type DBIDL = IntMap [Double]
type DBSDL = HashMap SKey [Double]
type DBISL = IntMap [SContent]
type DBSSL = HashMap SKey [SContent]
