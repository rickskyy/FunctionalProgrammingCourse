module Competition where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS
import Data.Time

type Id = Integer
type Winner = Integer

data Competition = Competition {id :: Integer, sectionId :: Integer, beginTime :: LocalTime, endTime :: LocalTime, winner :: Integer} deriving (Show)

parseDate :: String -> LocalTime
parseDate dateStr = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" dateStr :: LocalTime

-- | adds row to Competition table
createCompetition :: IConnection a => Id -> String -> String -> Winner -> a -> IO Bool
createCompetition sectionId beginTime endTime studentId conn =
    withTransaction conn (createCompetition' sectionId beginTime endTime studentId)

createCompetition' sectionId beginTime endTime studentId conn = do
    changed <- run conn query [SqlInteger sectionId, SqlLocalTime (parseDate beginTime), SqlLocalTime (parseDate endTime), 
                               SqlInteger studentId]
    return $ changed == 1
    where
        query = "insert into sport_univ_competition (sectionId, beginTime, endTime, winner)" ++
            " values (?, ?, ?, ?)"

-- | return row in Competition by id
readCompetition :: IConnection a => Id -> a -> IO [Competition]
readCompetition id conn = do
  query_result <- quickQuery' conn query [SqlInteger id]
  return $ map unpack query_result
  where
      query = "select * from sport_univ_competition where id = ?"
      unpack [SqlInteger uid, SqlInteger sid, SqlLocalTime beginTime, SqlLocalTime endTime, SqlInteger stid] =
        Competition uid sid beginTime endTime stid
      unpack x = error $ "Unexpected result: " ++ show x

-- | return all rows in Competition
readAllCompetitions :: IConnection a => a -> IO [Competition]
readAllCompetitions conn = do
  query_result <- quickQuery' conn query []
  return $ map unpack query_result
  where
    query = "select * from sport_univ_competition order by id"
    unpack [SqlInteger uid, SqlInteger sid, SqlLocalTime beginTime, SqlLocalTime endTime, SqlInteger stid] =
      Competition uid sid beginTime endTime stid
    unpack x = error $ "Unexpected result: " ++ show x

readAllWinnerNames :: IConnection a => a -> IO [(String, String, Integer, String)]
readAllWinnerNames conn = do
  query_result <- quickQuery' conn query []
  return $ map unpack query_result
  where
    query = "select sport_univ_student.name, sport_univ_student.surname, sport_univ_competition.id, sport_univ_section.name " ++
            "from ((sport_univ_competition " ++
            "inner join sport_univ_student on sport_univ_competition.winner = sport_univ_student.id) " ++
            "inner join sport_univ_section on sport_univ_competition.sectionID = sport_univ_section.id)"
    unpack [SqlByteString name, SqlByteString surname, SqlInteger cid, SqlByteString sec_name] =
      (BS.unpack name, BS.unpack surname, cid, BS.unpack sec_name)
    unpack x = error $ "Unexpected result: " ++ show x 

-- | update row in Competition by id
updateCompetition :: IConnection a => Id -> Id -> String -> String -> Winner -> a -> IO Bool
updateCompetition uid sid beginTime endTime stid conn =
    withTransaction conn (updateCompetition' uid sid beginTime endTime stid)

updateCompetition' uid sid beginTime endTime stid conn = do
  changed <- run conn query
                 [SqlInteger sid, SqlLocalTime (parseDate beginTime), SqlLocalTime (parseDate endTime), SqlInteger stid, SqlInteger uid]
  return $ changed == 1
  where
    query = "update sport_univ_competition set sectionId = ?, beginTime = ?," ++
            " endTime = ?, winner = ? where id = ?"

-- | delete row in Competition by id
deleteCompetition :: IConnection a => Id -> a -> IO Bool
deleteCompetition uid conn =
    withTransaction conn (deleteCompetition' uid)

deleteCompetition' uid conn = do
  changed <- run conn query [SqlInteger uid]
  return $ changed == 1
  where
    query = "delete from sport_univ_competition where id = ?"