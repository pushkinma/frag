{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
-- MapCfg.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005

-- | Reads the level configuration files and map media files.
module MapCfg where

import           Control.Exception
import           Data.Foldable
import           Data.HashTable.IO as H
import           Data.IORef
import           Data.List         hiding (insert)
import           System.IO

import           BSP               hiding (size)
import           Camera
import           IdentityList
import           MD3
import           Object
import           ObjectBehavior

-- |
data ObjectConstructor
  = ConsCamera Camera
  | ConsAICube
  { startPosition :: (Double, Double, Double)
  , size          :: (Double, Double, Double)
  , wayPoints     :: [(Double, Double, Double)]
  , modlName      :: String
  } deriving (Read, Show)

-- |
readMapCfg
  :: FilePath
  -> IO [IntermediateObject]
readMapCfg filepath = do
  withBinaryFile filepath ReadMode $ \handle -> do
    lines <- readLines handle
    print lines
    let objects  = map lines2ObjectCons lines
    return $ map objectCons2IntermediateObjects objects

-- |
readMapMedia
  :: FilePath
  -> IO (IORef BSPMap, BasicHashTable String Model)
readMapMedia filepath = do
  withBinaryFile filepath ReadMode $ \handle -> do
    lines <- readLines handle
    print lines
    let levelModels = lines2LevelModels lines
        (MMap lvlName) = head levelModels
    bsp <- readBSP lvlName
    hash <- fromList []
    for_ (tail levelModels) $ readLevelModels hash
    return (bsp, hash)

-- |
readLevelModels
  :: BasicHashTable String Model
  -> LevelModel
  -> IO ()
readLevelModels hash (MWeapon name) =
  getWeaponModel hash name
readLevelModels hash (MPlayerModel name weaponName) =
  getModel hash name weaponName

-- |
getModel
  :: BasicHashTable String Model
  -> String
  -> String
  -> IO ()
getModel hash name weaponName = do
  getWeaponModel hash weaponName
  Just weapon <- H.lookup hash weaponName
  model <- readModel name weapon
  insert hash name model

-- |
getWeaponModel
  :: BasicHashTable String Model
  -> String
  -> IO ()
getWeaponModel hash name = do
  H.lookup hash name >>= \case
    Just _  -> return ()
    Nothing -> do
      !weaponModel <- readWeaponModel ("tga/models/weapons/" ++ name ++ ".md3") ("tga/models/weapons/" ++ name ++ ".shader")
      insert hash name weaponModel

-- |
readLines
  :: Handle
  -> IO [String]
readLines handle = do
  hIsEOF handle >>= \case
    True  -> return []
    False -> do
      line  <- hGetLine  handle
      lines <- readLines handle
      return (line:lines)

-- |
data LevelModel
  = MWeapon String
  | MPlayerModel String String
  | MMap String
  deriving (Read, Show)

-- |
data IntermediateObject
  = ICamera ([(String, AnimState, AnimState)] -> [(ILKey, Message)] -> ILKey -> Object)
  | IAICube ((AnimState, AnimState) -> ILKey -> Object) String

-- |
lines2ObjectCons
  :: String
  -> ObjectConstructor
lines2ObjectCons str
 | head (words str) == "ConsCamera" = read str :: ObjectConstructor
 | head (words str) == "ConsAICube" = read str :: ObjectConstructor

-- |
lines2LevelModels :: [String] -> [LevelModel]
lines2LevelModels = map read

-- |
objectCons2IntermediateObjects
  :: ObjectConstructor
  -> IntermediateObject
objectCons2IntermediateObjects (ConsCamera cam) =
  ICamera (camera cam)
objectCons2IntermediateObjects c@ConsAICube {} =
  IAICube
    (aicube (startPosition c) (size c) (wayPoints c)(modlName c)) (modlName c)

-- |
toCompleteObjects
  :: [(String, AnimState, AnimState)]
  -> [IntermediateObject]
  -> [ILKey -> Object]
toCompleteObjects animList =
  map (toCompleteObject animList)

-- |
toCompleteObject
  :: [(String, AnimState, AnimState)]
  -> IntermediateObject
  -> (ILKey -> Object)
toCompleteObject animList (ICamera func) =
  func animList []
toCompleteObject animList (IAICube func modelname) =
  func (findModelAnim modelname animList)

-- |
findModelAnim
  :: String
  -> [(String, AnimState, AnimState)]
  -> (AnimState, AnimState)
findModelAnim name anms = (ua, la)
  where
    Just (_, ua, la) = find (\(x, _, _) -> x == name) anms
