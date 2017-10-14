module Schedule where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS
import Data.Time.LocalTime
import Data.Time

type Id = Integer
type SDay = String
type STime = String
type DBTime = TimeOfDay

parseTime' :: String -> TimeOfDay
parseTime' dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" "10:00:00" :: TimeOfDay

createSchedule :: IConnection a => Id -> SDay -> STime -> STime -> a -> IO Bool
createSchedule secId day beg end conn =
    withTransaction conn (createSchedule' secId day beg end)

createSchedule' secId day beg end conn = do
    changed <- run conn query [SqlInteger secId, SqlString day, SqlLocalTimeOfDay (parseTime' beg), SqlLocalTimeOfDay (parseTime' end)]
    return $ changed == 1
    where
        query = "insert into sport_univ_schedule (sectionId, beginday, begintime, endtime)" ++
            " values (?, ?, ?, ?)"

readSchedule :: IConnection a => a -> Id -> IO [(Id, Id, SDay, DBTime, DBTime)]
readSchedule conn id = do
  query_result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack query_result
  where
      query = "select * from sport_univ_schedule where id = ?"
      unpack [SqlInteger uid, SqlInteger secId, SqlByteString day, SqlLocalTimeOfDay beg, SqlLocalTimeOfDay end] =
        (uid, secId, BS.unpack day, beg, end)
      unpack x = error $ "Unexpected result: " ++ show x


readAllSchedule :: IConnection a => a -> IO [(Id, Id, SDay, DBTime, DBTime)]
readAllSchedule conn = do
  query_result <- quickQuery' conn query []
  return $ map unpack query_result
  where
    query = "select * from sport_univ_schedule order by id"
    unpack [SqlInteger uid, SqlInteger secId, SqlByteString day, SqlLocalTimeOfDay beg, SqlLocalTimeOfDay end] =
       (uid, secId, BS.unpack day, beg, end)
    unpack x = error $ "Unexpected result: " ++ show x

updateSchedule :: IConnection a => Id -> Id -> SDay -> STime -> STime -> a -> IO Bool
updateSchedule uid secId day beg end conn =
    withTransaction conn (updateSchedule' uid secId day beg end)

updateSchedule' uid secId day beg end conn = do
  changed <- run conn query
                 [SqlInteger secId, SqlString day, SqlLocalTimeOfDay (parseTime' beg), SqlLocalTimeOfDay (parseTime' end), SqlInteger uid]
  return $ changed == 1
  where
    query = "update sport_univ_schedule set sectionId = ?, beginday = ?, " ++
            " begintime = ?, endtime = ? where id = ?"