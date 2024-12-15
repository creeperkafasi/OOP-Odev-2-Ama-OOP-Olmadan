import System.Environment (getArgs)
import Common
import GHC.Debug (debugLn)
import Data.List (sort, find)
import Text.Printf (printf)

parseMetadata :: String -> IO (Int, Int)
parseMetadata line = do
  let (studentCount : pricePerCredit : _) = map (read :: String -> Int) (words line)
  return (studentCount, pricePerCredit)

parseStudents :: String -> IO [Student]
parseStudents text = do
  return $ takeStudents (tail (lines text))
  where
    takeStudents :: [String] -> [Student]
    takeStudents [] = []
    takeStudents lines' = do
      let (student, rest) = takeStudent lines'
      student : takeStudents rest

    takeStudent :: [String] -> (Student, [String])
    takeStudent lines' = do
      let args = reverse $ words (head lines')
      let (courseCount,hasPaid,idNumber,name) =
           ( read   (head args)
           , "Y" == (args !! 1)
           , read   (args !! 2)
           , (unwords . reverse) (drop 3 args) )
      let courseArgsList = map ( reverse . words . (lines' !!)) [1..courseCount]
      let courses = map (\courseArgs -> Course
            ((unwords . reverse) (drop 3 courseArgs))
            (courseArgs !! 2)
            (read (courseArgs !! 1))
            (read (head courseArgs))) courseArgsList
      (Student name idNumber hasPaid courses, drop (1 + courseCount) lines')

calculateBill :: [Course] -> Int -> Int
calculateBill courses pricePerCredit = 
  sum $ map ((pricePerCredit *) . creditHours) courses

printStudent :: Int -> Student -> IO ()
printStudent pricePerCredit student = do
  putStrLn "--------------------"
  putStrLn $ name student <> " - " <> show (idNumber student)
  if tuitionPaid student then (do
      putStrLn "Tuition unpaid, grades are withdrawn"
      printf "Bill: %d₺\n" (calculateBill (courses student) pricePerCredit)
    )
  else mapM_ 
    (\course -> do
      putStrLn (
           courseNo course           <> " "
        <> courseName course         <> " " 
        <> show (creditHours course) <> " "
        <> show (grade course)              )
    )
    (courses student)

getArgs' :: IO (String, Maybe Int )
getArgs' = do
  args <- getArgs
  case args of
    [inputFile, studentNumberStr] ->
      return (inputFile, return $ read studentNumberStr)
    [inputFile] ->
      return (inputFile, Nothing)
    _ -> error $  "Yanlış Kullanım. Doğrusu:\n"
               <> "./main <dosya.txt>\n"
               <> "veya \n"
               <> "./main <dosya.txt> <numara>\n"

main :: IO ()
main = do
  (inputFile, studentNumber) <- getArgs'
  (studentCount, pricePerCredit) <- readFile inputFile >>= parseMetadata  
  students <- readFile inputFile >>= parseStudents
  case studentNumber of
    Nothing -> mapM_ (printStudent pricePerCredit) students
    Just id -> case find (( id == ) . idNumber) students of
      Nothing -> printf "Student with id %d not found" id
      Just student -> printStudent pricePerCredit student
