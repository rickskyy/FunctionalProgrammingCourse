module Student where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type Id = Integer
type Name = String
type Surname = String

createStudent :: IConnection a => Name -> Surname -> Id -> a -> IO Bool
createStudent name surname sectionId conn =
    withTransaction conn (createStudent' name surname sectionId)

createStudent' name surname sectionId conn = do
    changed <- run conn query [SqlString name, SqlString surname, SqlInteger sectionId]
    return $ changed == 1
    where
        query = "insert into sport_univ_student (name, surname, sectionId)" ++
            " values (?, ?, ?)"

readStudent :: IConnection a => a -> Id -> IO [(Id, Name, Surname, Id)]
readStudent conn id = do
  query_result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack query_result
  where
      query = "select * from sport_univ_student where id = ?"
      unpack [SqlInteger uid, SqlByteString name, SqlByteString surname, SqlInteger sid] =
        (uid, BS.unpack name, BS.unpack surname, sid)
      unpack x = error $ "Unexpected result: " ++ show x


readAllStudents :: IConnection a => a -> IO [(Id, Name, Surname, Id)]
readAllStudents conn = do
  query_result <- quickQuery' conn query []
  return $ map unpack query_result
  where
    query = "select * from sport_univ_student order by id"
    unpack [SqlInteger uid, SqlByteString name, SqlByteString surname, SqlInteger sid] =
       (uid, BS.unpack name, BS.unpack surname, sid)
    unpack x = error $ "Unexpected result: " ++ show x

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