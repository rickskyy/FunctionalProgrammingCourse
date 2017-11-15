module ConsoleInterface where

import Data.Time
import Database.HDBC

import Student
import Teacher
import Section
import Schedule
import Competition

listStudentsFromInput :: IConnection a => a -> IO()
listStudentsFromInput conn = do
    res <- readAllStudents conn
    putStrLn $ show res

listTeachersFromInput :: IConnection a => a -> IO()
listTeachersFromInput conn = do
    res <- readAllTeachers conn
    putStrLn $ show res

listSectionsFromInput :: IConnection a => a -> IO()
listSectionsFromInput conn = do
    res <- readAllSections conn
    putStrLn $ show res

listScheduleFromInput :: IConnection a => a -> IO()
listScheduleFromInput conn = do
    res <- readAllSchedule conn
    putStrLn $ show res

listCompetitionsFromInput :: IConnection a => a -> IO()
listCompetitionsFromInput conn = do
    res <- readAllCompetitions conn
    putStrLn $ show res

listCompetitionsWinnersFromInput :: IConnection a => a -> IO()
listCompetitionsWinnersFromInput conn = do
    res <- readAllWinnerNames conn
    putStrLn $ show res

----------------------------------------------------------
readStudentFromInput :: IConnection a => a -> IO()
readStudentFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- readStudent (read id) conn
    putStrLn $ show res

readTeacherFromInput :: IConnection a => a -> IO()
readTeacherFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- readTeacher (read id) conn
    putStrLn $ show res

readSectionFromInput :: IConnection a => a -> IO()
readSectionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- readSection (read id) conn
    putStrLn $ show res

readScheduleFromInput :: IConnection a => a -> IO()
readScheduleFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- readSchedule (read id) conn
    putStrLn $ show res

readCompetitionFromInput :: IConnection a => a -> IO()
readCompetitionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- readCompetition (read id) conn
    putStrLn $ show res

-----------------------------------------------------------
createStudentFromInput :: IConnection a => a -> IO()
createStudentFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set surname: "
    surname <- getLine
    putStrLn "Set section id: "
    sid <- getLine
    res <- createStudent name surname (read sid) conn
    putStrLn $ show res

createTeacherFromInput :: IConnection a => a -> IO()
createTeacherFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set surname: "
    surname <- getLine
    res <- createTeacher name surname conn
    putStrLn $ show res

createSectionFromInput :: IConnection a => a -> IO()
createSectionFromInput conn = do
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set Teacher id: "
    tid <- getLine
    res <- createSection name (read tid) conn
    putStrLn $ show res

createScheduleFromInput :: IConnection a => a -> IO()
createScheduleFromInput conn = do
    putStrLn "Set Section id: "
    sid <- getLine
    putStrLn "Set Begin Day: "
    beginDay <- getLine
    putStrLn "Set begin time. Format HH:mm "
    beginTime <- getLine
    putStrLn "Set end time. Format HH:mm "
    endTime <- getLine
    res <- createSchedule (read sid) beginDay beginTime endTime conn
    putStrLn $ show res

createCompetitionFromInput :: IConnection a => a -> IO()
createCompetitionFromInput conn = do
    putStrLn "Set Section id: "
    sid <- getLine
    putStrLn "Set begin time. Format YYYY-MM-DD HH:mm "
    beginTime <- getLine
    putStrLn "Set end time. Format YYYY-MM-DD HH:mm "
    endTime <- getLine
    putStrLn "Set winner (Student id): "
    winner <- getLine
    res <- createCompetition (read sid) beginTime endTime (read winner) conn
    putStrLn $ show res

---------------------------------------------------------------------
updateStudentFromInput :: IConnection a => a -> IO()
updateStudentFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set surname: "
    surname <- getLine
    putStrLn "Set section id: "
    sid <- getLine
    res <- updateStudent (read id) name surname (read sid) conn
    putStrLn $ show res

updateTeacherFromInput :: IConnection a => a -> IO()
updateTeacherFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set surname: "
    surname <- getLine
    res <- updateTeacher (read id) name surname conn
    putStrLn $ show res

updateSectionFromInput :: IConnection a => a -> IO()
updateSectionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set name: "
    name <- getLine
    putStrLn "Set Teacher id: "
    tid <- getLine
    res <- updateSection (read id) name (read tid) conn
    putStrLn $ show res

updateScheduleFromInput :: IConnection a => a -> IO()
updateScheduleFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set Section id: "
    sid <- getLine
    putStrLn "Set Begin Day: "
    beginDay <- getLine
    putStrLn "Set begin time. Format HH:mm "
    beginTime <- getLine
    putStrLn "Set end time. Format HH:mm "
    endTime <- getLine
    res <- updateSchedule (read id) (read sid) beginDay beginTime endTime conn
    putStrLn $ show res

updateCompetitionFromInput :: IConnection a => a -> IO()
updateCompetitionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    putStrLn "Set Section id: "
    sid <- getLine
    putStrLn "Set begin time. Format YYYY-MM-DD HH:mm "
    beginTime <- getLine
    putStrLn "Set end time. Format YYYY-MM-DD HH:mm "
    endTime <- getLine
    putStrLn "Set winner (Student id): "
    winner <- getLine
    res <- updateCompetition (read id) (read sid) beginTime endTime (read winner) conn
    putStrLn $ show res

---------------------------------------------------------------------------------
deleteStudentFromInput :: IConnection a => a -> IO()
deleteStudentFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- deleteStudent (read id) conn
    putStrLn $ show res

deleteTeacherFromInput :: IConnection a => a -> IO()
deleteTeacherFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- deleteTeacher (read id) conn
    putStrLn $ show res

deleteSectionFromInput :: IConnection a => a -> IO()
deleteSectionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- deleteSection (read id) conn
    putStrLn $ show res

deleteScheduleFromInput :: IConnection a => a -> IO()
deleteScheduleFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- deleteSchedule (read id) conn
    putStrLn $ show res

deleteCompetitionFromInput :: IConnection a => a -> IO()
deleteCompetitionFromInput conn = do
    putStrLn "Set id: "
    id <- getLine
    res <- deleteCompetition (read id) conn
    putStrLn $ show res
