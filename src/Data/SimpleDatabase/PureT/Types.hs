module Data.SimpleDatabase.PureT.Types (
    module Data.SimpleDatabase.PureT.Types
  , module Data.SimpleDatabase.Types
  ) where


import Data.SimpleDatabase.Types


import Data.IntMap (IntMap)
import Data.Trie (Trie)


type DBII = IntMap Int
type DBSI = Trie Int
type DBID = IntMap Double
type DBSD = Trie Double
type DBIS = IntMap SContent
type DBSS = Trie SContent

type DBIIL = IntMap [Int]
type DBSIL = Trie [Int]
type DBIDL = IntMap [Double]
type DBSDL = Trie [Double]
type DBISL = IntMap [SContent]
type DBSSL = Trie [SContent]
