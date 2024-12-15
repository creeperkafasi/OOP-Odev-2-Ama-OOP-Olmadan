import System.Environment (getArgs)
import Common
import GHC.Debug (debugLn)
import Data.List (sort, find)
import Text.Printf (printf)
import GHC.Base (eqInt)

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

totalCredits :: [Course] -> Int
totalCredits = sum . map creditHours

calculateGPA :: Student -> Float
calculateGPA student = 
    (sum . map (\c -> (fromIntegral.creditHours) c * (points.grade) c)) (courses student)
    / fromIntegral (totalCredits $ courses student)
  where
    points :: Grade -> Float
    points A = 4.0
    points B = 3.0
    points C = 2.0
    points D = 1.0
    points E = 0.0
    points F = 0.0
  

printStudent :: Int -> Student -> IO ()
printStudent pricePerCredit student = do
  putStrLn $ replicate 60 '='
  putStrLn $ "Student Name: " <> name student
  putStrLn $ "Student ID: " <> show (idNumber student)
  putStrLn $ "Number of courses enrolled: " <> (show.length.courses) student
  if tuitionPaid student then (do
      putStrLn "Tuition unpaid, grades are withdrawn"
      printf "Bill: %d₺\n" (calculateBill (courses student) pricePerCredit)
    )
  else do
    (putStrLn . unwords)
        [ fitWidth "Course No" 10, "|"
        , fitWidth "Course Name" 25, "|"
        , fitWidth "Credits" 9, "|"
        , fitWidth "Grade" 8 ]
    putStrLn $ replicate 60 '-'
    mapM_ (\course -> do
      (putStrLn . unwords)
        [ fitWidth (courseNo course) 10, "|"
        , fitWidth (courseName course) 25, "|"
        , fitWidth (show (creditHours course)) 9, "|"
        , fitWidth (show (grade course)) 8 ]
      )
      (courses student)
    putStrLn ("Total number of credits: " <> (show . totalCredits . courses) student)
    putStrLn ("Mid-Semester GPA: " <> (show . (*) 0.01 . fromIntegral . round . (100 *) . calculateGPA) student)

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
    Just id -> case find (eqInt id . idNumber) students of
      Nothing -> putStrLn $ "Student with id " <> show id <> " not found"
      Just student -> printStudent pricePerCredit student
