import System.Random
import Control.Monad (replicateM)
import Data.Foldable (for_)
import Data.List (intercalate)
import System.Environment (getArgs)
import Common

-- | Generates a random Grade
randomGrade :: IO Grade
randomGrade = do
  let minG = fromEnum (minBound :: Grade)
  let maxG = fromEnum(maxBound :: Grade)
  randomIndex <- randomRIO(minG, maxG)
  (return . toEnum) randomIndex

-- Bounds for generated course counts

minCourseCount = 3
maxCourseCount = 10

-- | List of courses used in the random generation without their grades
allCourses :: [Grade -> Course]
allCourses = 
  [ Course "AyrıkMatematik"     "BIL2015" 3
  , Course "ElektronikDevreler" "BIL2017" 3
  , Course "ElektronikLab"      "BIL2021" 2
  , Course "DataStructures"     "COM2005" 3
  , Course "DigitalDesign"      "COM2007" 3
  , Course "OOP"                "COM2009" 3
  , Course "Diferansiyel"       "MAT2031" 4 ]

-- | Renders the course into a `String`
-- Returned String is in the format:
-- "courseName courseNo creditHours grade"
renderCourse :: Course -> String
renderCourse course = 
     courseName course <> " "
  <> courseNo course <> " "
  <> show (creditHours course) <> " "
  <> show (grade course)

randomCourse :: IO Course
randomCourse =
  randomGrade >>= \grade ->
  pickFromList allCourses >>= \(course, _) ->
  return $ course grade


renderStudent :: Student -> String
renderStudent student =
  intercalate "\n" $
      studentInfo 
    : map renderCourse (courses student)
  where
    studentInfo =
         name student <> " " 
      <> show (idNumber student) <> " "
      <> (if tuitionPaid student then "Y" else "N") <> " "
      <> show (length $ courses student)

randomStudent :: IO Student
randomStudent =
  pickFromList firstNames >>= \(firstName, _) ->
  pickFromList lastNames >>= \(lastName, _) ->
  randomRIO (False, True) >>= \hasPaid ->
  randomRIO (100_000, 999_999) >>= \idN ->
  randomRIO (minCourseCount, maxCourseCount) >>= \courseCount ->
  pickFromListN allCourses courseCount >>= mapM (\course ->
    randomGrade >>= \grade ->
    return $ course grade ) >>= \gradedCourses ->
  return $ Student (firstName <> " " <> lastName) idN hasPaid gradedCourses
  where
    firstNames :: [String]
    firstNames = 
      [ "Ahmet", "Mehmet", "Mustafa", "Ali", "Hüseyin",
        "Ayşe", "Fatma", "Emine", "Zeynep", "Hatice",
        "İbrahim", "Hasan", "Osman", "Yusuf", "Selim",
        "Elif", "Ceren", "Gizem", "Aylin", "Ebru",
        "Burak", "Eren", "Hakan", "Furkan", "Kerem",
        "Merve", "Buse", "Seda", "Esra", "Tuğba",
        "Gökhan", "Orhan", "Kemal", "Sinan", "Uğur",
        "Hande", "Meltem", "Nazlı", "Damla", "Şule",
        "Cem", "Levent", "Batuhan", "Barış", "Deniz",
        "Volkan", "Berna", "Selin", "Duygu", "Aysel",
        "Emre", "Ege", "Ramazan"]
    lastNames :: [String]
    lastNames = 
      [ "Yılmaz", "Demir", "Şahin", "Çelik", "Yıldırım",
        "Kaya", "Öztürk", "Aydın", "Arslan", "Doğan",
        "Kılıç", "Aslan", "Taş", "Koç", "Aksoy",
        "Güneş", "Bozkurt", "Çetin", "Kara", "Erdoğan",
        "Bulut", "Kurt", "Özdemir", "Pektaş", "Eren",
        "Korkmaz", "Şimşek", "Dinçer", "Yücel", "Ateş",
        "Avcı", "Ekinci", "Uzun", "Karaca", "Uysal",
        "Alkan", "Güler", "Polat", "Karakaya", "Ergin",
        "Sezer", "Keskin", "Duran", "Candan", "Başar",
        "Yavuz", "Gök", "Durmaz", "Aktaş", "Bayraktar",
        "Tunç", "Camuzoğlu", "Yolsal", "Sari", "Kırım", "Karabacak"]

getArgs' :: IO (Int, Int, String)
getArgs' = do
  args <- getArgs
  case args of
    [studentCountStr, priceStr, fileName] ->
      return (read studentCountStr, read priceStr, fileName)
    _ -> error "Kullanım: ./generate <öğrenci sayısı> <ücret> <dosya adı>"

main = do
  (studentCount, price, fileName) <- getArgs'
  students <- replicateM studentCount randomStudent
  let renderedStudents = map renderStudent students
  writeFile fileName $ unlines
    ( show studentCount <> " " <> show price
    : renderedStudents )
