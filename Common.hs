module Common where
import System.Random (randomRIO)
import Data.List (intercalate)

data Grade = 
    F | E | D | C | B | A
    deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Course = Course
    { courseName   :: String
    , courseNo     :: String
    , creditHours  :: Int
    , grade        :: Grade}
    deriving Show

data Student = Student
  { name        :: String
  , idNumber    :: Int
  , tuitionPaid :: Bool
  , courses     :: [Course] }
  deriving Show

pickFromList :: [t] -> IO (t, [t])
pickFromList list =
  randomRIO (0, length list - 1) >>= \i ->
  return ( list !! i
         , take i list ++ drop (i+1) list)

-- | Picks at most `n` non repeating elements from `list`
pickFromListN :: [t] -> Int -> IO [t]
pickFromListN _  0 = return []
pickFromListN [] _ = return []
pickFromListN list n = do
  (picked, remaining) <- pickFromList list
  rest <- pickFromListN remaining (n-1)
  return (picked : rest)