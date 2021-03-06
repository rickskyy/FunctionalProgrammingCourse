module Section where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type Id = Integer
type Name = String

data Section = Section {id :: Integer, name :: String, teacherId :: Integer} deriving (Show)

-- | adds row to Sections table
createSection :: IConnection a => Name -> Id -> a -> IO Bool
createSection name tid conn =
    withTransaction conn (createSection' name tid)

createSection' name tid conn = do
    changed <- run conn query [SqlString name, SqlInteger tid]
    return $ changed == 1
    where
        query = "insert into sport_univ_section (name, teacherId) " ++
            " values (?, ?)"

-- | return row in Sections by id
readSection :: IConnection a => Id -> a -> IO [Section]
readSection id conn= do
  result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack result
  where
      query = "select * from sport_univ_section where id = ?"
      unpack [SqlInteger uid, SqlByteString name, SqlInteger tid] =
        Section uid (BS.unpack name) tid
      unpack x = error $ "Unexpected result: " ++ show x

-- | return all rows in Sections
readAllSections :: IConnection a => a -> IO [Section]
readAllSections conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from sport_univ_section order by id"
    unpack [SqlInteger uid, SqlByteString name, SqlInteger tid] =
       Section uid (BS.unpack name) tid
    unpack x = error $ "Unexpected result: " ++ show x

-- | update row in Sections by id
updateSection :: IConnection a => Id -> Name -> Id -> a -> IO Bool
updateSection uid name tid conn =
    withTransaction conn (updateSection' uid name tid)

updateSection' uid name tid conn = do
  changed <- run conn query
    [SqlString name, SqlInteger tid, SqlInteger uid]
  return $ changed == 1
  where
    query = "update sport_univ_section set name = ?, teacherId = ? " ++
            " where id = ?"

-- | delete row in Section by id
deleteSection :: IConnection a => Id -> a -> IO Bool
deleteSection uid conn =
    withTransaction conn (deleteSection' uid)

deleteSection' uid conn = do
  changed <- run conn query [SqlInteger uid]
  return $ changed == 1
  where
    query = "delete from sport_univ_section where id = ?"