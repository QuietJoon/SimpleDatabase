module Data.SimpleDatabase.Types where


import Data.ByteString
import Data.Text
import Data.Text.Lazy


type IKey = Int
type SKey = ByteString
type SContent = Data.Text.Lazy.Text
-- SSContent: Type of storing string content
type SSContent = Data.Text.Text
