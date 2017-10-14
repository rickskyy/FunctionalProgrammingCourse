module Main where

import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Student
import Teacher
import Section
import Schedule

-- Example commands

manageStudents :: IConnection a => a -> IO ()
manageStudents c = do
    putStrLn "Manage Students"

    result <- readAllStudents c
    putStrLn $ show result
    result1 <- createStudent "Don" "Huan" 2 c
    putStrLn $ show result1
    result <- updateStudent 3 "Brian" "Sailman" 2 c
    putStrLn $ show result
    result <- readStudent c 3
    putStrLn $ show result

manageTeachers :: IConnection a => a -> IO ()
manageTeachers c = do
    putStrLn "Manage Teachers"

    result <- readAllTeachers c
    putStrLn $ show result
    result <- createTeacher "Kyle" "Superuser" c
    putStrLn $ show result
    result <- updateTeacher 3 "Mister" "Steel" c
    putStrLn $ show result
    result <- readTeacher c 3
    putStrLn $ show result

manageSections :: IConnection a => a -> IO ()
manageSections c = do
    putStrLn "Manage Sections"

    result <- readAllSections c
    putStrLn $ show result
    result <- createSection "Run" 2 c
    putStrLn $ show result
    result <- updateSection 3 "Athletics" 2 c
    putStrLn $ show result
    result <- readSection c 3
    putStrLn $ show result

manageSchedule :: IConnection a => a -> IO ()
manageSchedule c = do
    putStrLn "Manage Schedule"

    result <- readAllSchedule c
    putStrLn $ show result
    result <- createSchedule 1 "Fd" "14:00:00" "16:00:00" c
    putStrLn $ show result
    result <- updateSchedule 1 1 "Mon" "16:00:00" "18:30:00" c
    putStrLn $ show result
    result <- readSchedule c 1
    putStrLn $ show result


main = do
    c <- connectPostgreSQL "host=localhost dbname=sport_univ_db user=postgres password=root"

    manageStudents c
    manageTeachers c
    manageSections c
    manageSchedule c

    commit c
    disconnect c
    return ()
