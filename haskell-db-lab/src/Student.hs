module Student where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type Id = Integer
type Name = String
type Surname = String

data Student = Student {id :: Integer, name :: String, surname :: String, sectionId :: Integer} deriving (Show)

-- | adds row to Student table
createStudent :: IConnection a => Name -> Surname -> Id -> a -> IO Bool
createStudent name surname sectionId conn =
    withTransaction conn (createStudent' name surname sectionId)

createStudent' name surname sectionId conn = do
    changed <- run conn query [SqlString name, SqlString surname, SqlInteger sectionId]
    return $ changed == 1
    where
        query = "insert into sport_univ_student (name, surname, sectionId)" ++
            " values (?, ?, ?)"

-- | return row in Student by id
readStudent :: IConnection a => Id -> a -> IO [Student]
readStudent id conn = do
  query_result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack query_result
  where
      query = "select * from sport_univ_student where id = ?"
      unpack [SqlInteger uid, SqlByteString name, SqlByteString surname, SqlInteger sid] =
        Student uid (BS.unpack name) (BS.unpack surname) sid
      unpack x = error $ "Unexpected result: " ++ show x

-- | return all rows in Student
readAllStudents :: IConnection a => a -> IO [Student]
readAllStudents conn = do
  query_result <- quickQuery' conn query []
  return $ map unpack query_result
  where
    query = "select * from sport_univ_student order by id"
    unpack [SqlInteger uid, SqlByteString name, SqlByteString surname, SqlInteger sid] =
       Student uid (BS.unpack name) (BS.unpack surname) sid
    unpack x = error $ "Unexpected result: " ++ show x

-- | update row in Student by id
updateStudent :: IConnection a => Id -> Name -> Surname -> Id -> a -> IO Bool
updateStudent uid name surname sid conn =
    withTransaction conn (updateStudent' uid name surname sid)

updateStudent' uid name surname sid conn = do
  changed <- run conn query
                 [SqlString name, SqlString surname, SqlInteger sid, SqlInteger uid]
  return $ changed == 1
  where
    query = "update sport_univ_student set name = ?, surname = ?," ++
            " sectionId = ? where id = ?"

-- | delete row in Student by id
deleteStudent :: IConnection a => Id -> a -> IO Bool
deleteStudent uid conn =
    withTransaction conn (deleteStudent' uid)

deleteStudent' uid conn = do
  changed <- run conn query [SqlInteger uid]
  return $ changed == 1
  where
    query = "delete from sport_univ_student where id = ?"