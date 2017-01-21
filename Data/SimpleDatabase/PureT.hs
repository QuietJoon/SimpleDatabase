{-# LANGUAGE RecordWildCards #-}

module Data.SimpleDatabase.PureT (
    module Data.SimpleDatabase.PureT
  , module Data.SimpleDatabase.PureT.Types
  ) where


import Data.SimpleDatabase.Types
import Data.SimpleDatabase.PureT.Types


import Data.IntMap (IntMap)
import Data.Trie (Trie)
import qualified Data.IntMap as IM
import qualified Data.Trie as T
import Data.Maybe

import Control.Exception


data Database = Database
  { dbii :: DBII
  , dbsi :: DBSI
  , dbid :: DBID
  , dbsd :: DBSD
  , dbis :: DBIS
  , dbss :: DBSS
  , dbiil :: DBIIL
  , dbsil :: DBSIL
  , dbidl :: DBIDL
  , dbsdl :: DBSDL
  , dbisl :: DBISL
  , dbssl :: DBSSL
  }

data DBEntry
  = DBEII IKey Int
  | DBESI SKey Int
  | DBEID IKey Double
  | DBESD SKey Double
  | DBEIS IKey SContent
  | DBESS SKey SContent
  | DBEIIL IKey [Int]
  | DBESIL SKey [Int]
  | DBEIDL IKey [Double]
  | DBESDL SKey [Double]
  | DBEISL IKey [SContent]
  | DBESSL SKey [SContent]

-- Database

initDatabase :: Database
initDatabase =
  Database
    (IM.empty :: DBII)
    (T.empty :: DBSI)
    (IM.empty :: DBID)
    (T.empty :: DBSD)
    (IM.empty :: DBIS)
    (T.empty :: DBSS)
    (IM.empty :: DBIIL)
    (T.empty :: DBSIL)
    (IM.empty :: DBIDL)
    (T.empty :: DBSDL)
    (IM.empty :: DBISL)
    (T.empty :: DBSSL)

insertDBEntryAuto :: Database -> DBEntry -> Database
insertDBEntryAuto db@Database{..} dbEntry =
    case dbEntry of
      DBEII key value -> db { dbii = IM.insert key value dbii}
      DBESI key value -> db { dbsi = T.insert key value dbsi}
      DBEID key value -> db { dbid = IM.insert key value dbid}
      DBESD key value -> db { dbsd = T.insert key value dbsd}
      DBEIS key value -> db { dbis = IM.insert key value dbis}
      DBESS key value -> db { dbss = T.insert key value dbss}
      DBEIIL key value -> db { dbiil = IM.insert key value dbiil}
      DBESIL key value -> db { dbsil = T.insert key value dbsil}
      DBEIDL key value -> db { dbidl = IM.insert key value dbidl}
      DBESDL key value -> db { dbsdl = T.insert key value dbsdl}
      DBEISL key value -> db { dbisl = IM.insert key value dbisl}
      DBESSL key value -> db { dbssl = T.insert key value dbssl}


readDBII :: IKey -> Database -> Int
readDBII key Database{..} =
  fromMaybe (error $ "readDBII| No entry for key("++show key++")")
            (IM.lookup key dbii)

insertDBII :: IKey -> Int -> Database -> Database
insertDBII key value db@Database{..} =
  db { dbii = IM.insert key value dbii }

modifyDBII :: IKey -> (Int -> Int) -> Database -> Database
modifyDBII key mF db@Database{..} =
  db { dbii = IM.adjust mF key dbii }


readDBID :: IKey -> Database -> Double
readDBID key Database{..} =
  fromMaybe (error $ "readDBID| No entry for key("++show key++")")
            (IM.lookup key dbid)

insertDBID :: IKey -> Double -> Database -> Database
insertDBID key value db@Database{..} =
  db { dbid = IM.insert key value dbid }

modifyDBID :: IKey -> (Double -> Double) -> Database -> Database
modifyDBID key mF db@Database{..} =
  db { dbid = IM.adjust mF key dbid }


readDBIS :: IKey -> Database -> SContent
readDBIS key Database{..} =
  fromMaybe (error $ "readDBIS| No entry for key("++show key++")")
            (IM.lookup key dbis)

insertDBIS :: IKey -> SContent -> Database -> Database
insertDBIS key value db@Database{..} =
  db { dbis = IM.insert key value dbis }

modifyDBIS :: IKey -> (SContent -> SContent) -> Database -> Database
modifyDBIS key mF db@Database{..} =
  db { dbis = IM.adjust mF key dbis }


readDBSI :: SKey -> Database -> Int
readDBSI key Database{..} =
  fromMaybe (error $ "readDBSI| No entry for key("++show key++")")
            (T.lookup key dbsi)

insertDBSI :: SKey -> Int -> Database -> Database
insertDBSI key value db@Database{..} =
  db { dbsi = T.insert key value dbsi }

modifyDBSI :: SKey -> (Int -> Int) -> Database -> Database
modifyDBSI key mF db@Database{..} =
  db { dbsi = T.adjust mF key dbsi }


readDBSD :: SKey -> Database -> Double
readDBSD key Database{..} =
  fromMaybe (error $ "readDBSD| No entry for key("++show key++")")
            (T.lookup key dbsd)

insertDBSD :: SKey -> Double -> Database -> Database
insertDBSD key value db@Database{..} =
  db { dbsd = T.insert key value dbsd }

modifyDBSD :: SKey -> (Double -> Double) -> Database -> Database
modifyDBSD key mF db@Database{..} =
  db { dbsd = T.adjust mF key dbsd }


readDBSS :: SKey -> Database -> SContent
readDBSS key Database{..} =
  fromMaybe (error $ "readDBSS| No entry for key("++show key++")")
            (T.lookup key dbss)

insertDBSS :: SKey -> SContent -> Database -> Database
insertDBSS key value db@Database{..} =
  db { dbss = T.insert key value dbss }

modifyDBSS :: SKey -> (SContent -> SContent) -> Database -> Database
modifyDBSS key mF db@Database{..} =
  db { dbss = T.adjust mF key dbss }



readDBIIL :: IKey -> Database -> [Int]
readDBIIL key Database{..} =
  fromMaybe (error $ "readDBIIL| No entry for key("++show key++")")
            (IM.lookup key dbiil)

insertDBIIL :: IKey -> [Int] -> Database -> Database
insertDBIIL key value db@Database{..} =
  db { dbiil = IM.insert key value dbiil }

modifyDBIIL :: IKey -> ([Int] -> [Int]) -> Database -> Database
modifyDBIIL key mF db@Database{..} =
  db { dbiil = IM.adjust mF key dbiil }


readDBIDL :: IKey -> Database -> [Double]
readDBIDL key Database{..} =
  fromMaybe (error $ "readDBIDL| No entry for key("++show key++")")
            (IM.lookup key dbidl)

insertDBIDL :: IKey -> [Double] -> Database -> Database
insertDBIDL key value db@Database{..} =
  db { dbidl = IM.insert key value dbidl }

modifyDBIDL :: IKey -> ([Double] -> [Double]) -> Database -> Database
modifyDBIDL key mF db@Database{..} =
  db { dbidl = IM.adjust mF key dbidl }


readDBISL :: IKey -> Database -> [SContent]
readDBISL key Database{..} =
  fromMaybe (error $ "readDBISL| No entry for key("++show key++")")
            (IM.lookup key dbisl)

insertDBISL :: IKey -> [SContent] -> Database -> Database
insertDBISL key value db@Database{..} =
  db { dbisl = IM.insert key value dbisl }

modifyDBISL :: IKey -> ([SContent] -> [SContent]) -> Database -> Database
modifyDBISL key mF db@Database{..} =
  db { dbisl = IM.adjust mF key dbisl }


readDBSIL :: SKey -> Database -> [Int]
readDBSIL key Database{..} =
  fromMaybe (error $ "readDBSIL| No entry for key("++show key++")")
            (T.lookup key dbsil)

insertDBSIL :: SKey -> [Int] -> Database -> Database
insertDBSIL key value db@Database{..} =
  db { dbsil = T.insert key value dbsil }

modifyDBSIL :: SKey -> ([Int] -> [Int]) -> Database -> Database
modifyDBSIL key mF db@Database{..} =
  db { dbsil = T.adjust mF key dbsil }


readDBSDL :: SKey -> Database -> [Double]
readDBSDL key Database{..} =
  fromMaybe (error $ "readDBSDL| No entry for key("++show key++")")
            (T.lookup key dbsdl)

insertDBSDL :: SKey -> [Double] -> Database -> Database
insertDBSDL key value db@Database{..} =
  db { dbsdl = T.insert key value dbsdl }

modifyDBSDL :: SKey -> ([Double] -> [Double]) -> Database -> Database
modifyDBSDL key mF db@Database{..} =
  db { dbsdl = T.adjust mF key dbsdl }


readDBSSL :: SKey -> Database -> [SContent]
readDBSSL key Database{..} =
  fromMaybe (error $ "readDBSSL| No entry for key("++show key++")")
            (T.lookup key dbssl)

insertDBSSL :: SKey -> [SContent] -> Database -> Database
insertDBSSL key value db@Database{..} =
  db { dbssl = T.insert key value dbssl }

modifyDBSSL :: SKey -> ([SContent] -> [SContent]) -> Database -> Database
modifyDBSSL key mF db@Database{..} =
  db { dbssl = T.adjust mF key dbssl }


-- Evaluate Database to Garbage Collection

evaluateDatabase :: Database -> IO ()
evaluateDatabase Database{..} = do
  _ <- traverse evaluate dbii
  _ <- traverse evaluate dbsi
  _ <- traverse evaluate dbid
  _ <- traverse evaluate dbsd
  _ <- traverse evaluate dbis
  _ <- traverse evaluate dbss
  _ <- traverse evaluate dbiil
  _ <- traverse evaluate dbsil
  _ <- traverse evaluate dbidl
  _ <- traverse evaluate dbsdl
  _ <- traverse evaluate dbisl
  _ <- traverse evaluate dbssl
  return ()

evaluateDatabaseSimple :: Database -> IO ()
evaluateDatabaseSimple Database{..} = do
  _ <- evaluate dbii
  _ <- evaluate dbsi
  _ <- evaluate dbid
  _ <- evaluate dbsd
  _ <- evaluate dbis
  _ <- evaluate dbss
  _ <- evaluate dbiil
  _ <- evaluate dbsil
  _ <- evaluate dbidl
  _ <- evaluate dbsdl
  _ <- evaluate dbisl
  _ <- evaluate dbssl
  return ()
