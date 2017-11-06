module Teacher where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type Id = Integer
type Name = String
type Surname = String

-- | adds row to Teacher table
createTeacher :: IConnection a => Name -> Surname -> a -> IO Bool
createTeacher name surname conn =
    withTransaction conn (createTeacher' name surname)

createTeacher' name surname conn = do
    changed <- run conn query [SqlString name, SqlString surname]
    return $ changed == 1
    where
        query = "insert into sport_univ_teacher (name, surname)" ++
            " values (?, ?)"

-- | return row in Teacher by id
readTeacher :: IConnection a => a -> Id -> IO [(Id, Name, Surname)]
readTeacher conn id = do
  result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack result
  where
      query = "select * from sport_univ_teacher where id = ?"
      unpack [SqlInteger uid, SqlByteString name, SqlByteString surname] =
        (uid, BS.unpack name, BS.unpack surname)
      unpack x = error $ "Unexpected result: " ++ show x

-- | return all rows in Teacher
readAllTeachers :: IConnection a => a -> IO [(Id, Name, Surname)]
readAllTeachers conn = do
  result <- quickQuery' conn query []
  return $ map unpack result
  where
    query = "select * from sport_univ_teacher order by id"
    unpack [SqlInteger uid, SqlByteString name, SqlByteString surname] =
       (uid, BS.unpack name, BS.unpack surname)
    unpack x = error $ "Unexpected result: " ++ show x

-- | update row in Teacher by id
updateTeacher :: IConnection a => Id -> Name -> Surname -> a -> IO Bool
updateTeacher uid name surname conn =
    withTransaction conn (updateTeacher' uid name surname)

updateTeacher' uid name surname conn = do
  changed <- run conn query
    [SqlString name, SqlString surname, SqlInteger uid]
  return $ changed == 1
  where
    query = "update sport_univ_teacher set name = ?, surname = ? " ++
            "where id = ?"