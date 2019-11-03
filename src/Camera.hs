{-# LANGUAGE DeriveGeneric #-}
-- Camera.hs; Mun Hon Cheong (mhch295@cse.unsw.edu.au) 2005
-- | Functions to manipulate a camera.
module Camera where

import           FRP.Yampa.Geometry hiding (Vector3)
import           GHC.Generics
import           Graphics.UI.GLUT

import           Matrix
import           PhysicalDimensions

-- |
data Camera = Camera
  { cpos    :: !(Double, Double, Double)
  , viewPos :: !(Double, Double, Double)
  , upVec   :: !(Double, Double, Double)
  } deriving (Show, Read, Generic)

-- | Initialise a camera.
initCamera
  ::(Real a)
  => (a, a, a)
  -> (a, a, a)
  -> (a, a, a)
  -> Camera
initCamera (a, b, c) (d, e, f) (g, h, i) = Camera
  { cpos    = (realToFrac a, realToFrac b, realToFrac c)
  , viewPos = (realToFrac d, realToFrac e, realToFrac f)
  , upVec   = (realToFrac g, realToFrac h, realToFrac i)
  }

-- | Convert from a tuple of doubles to an OpenGL vector.
toVector3
  :: Vec3
  -> Vector3 GLdouble
toVector3 (a, b, c) = Vector3 a b c

-- | Convert from a tuple of doubles to an OpenGL vertex.
toVertex3
  :: Vec3
  -> Vertex3 GLdouble
toVertex3 (a, b, c) = Vertex3 a b c

-- | Gets OpenGL to assume the view of the camera.
cameraLook
  :: Camera
  -> IO ()
cameraLook camera = do
  let headPos  = vectorAdd (0, 30, 0) (cpos camera)
      headView = vectorAdd (0, 30, 0) (viewPos camera)
  lookAt (toVertex3 headPos) (toVertex3 headView) (toVector3 (upVec camera))

-- | Sets the view vector of the camera based on the displacement
-- of the cursor from the center of the screen. Hardcoded to
-- work at resolutions of 640x480.
setView
  :: (Position2, Camera)
  -> Camera
setView (Point2 x y, cam) = rotateView cam1 (realToFrac (320 - x) / 250.0) (0, 1, 0)
  where
    cam1 =
      rotateView
        cam
        (realToFrac (240 - y) / 250.0)
        (normalise (crossProd (vectorSub (viewPos cam) (cpos cam)) (upVec cam)))

-- | Gets the vector from the players "muzzle point" to
-- infinity along the view vector. The muzzle point
-- isn't at the tip of the gun, its actually close
-- just a bit to the right of the start of the view vector.
-- It just creates the illusion that its being fired from
-- the players gun.
firePos
  :: Vec3
  -> Vec3
  -> (Vec3, Vec3)
firePos (x, y, z) (vx, vy, vz) = ((sx2, sy2, sz2), infin)
  where
    (sx, sy, sz) = normalise $ crossProd (normalise $ vectorSub (vx, vy, vz) (x, y, z)) (0, 1, 0)
    (vsx, vsy, vsz) = normalise $ crossProd (normalise $ vectorSub (vx, vy, vz) (x, y, z)) (sx, sy, sz)
    (sx1, sy1, sz1) = vectorAdd (2 * sx, 2 * sy, 2 * sz) (x, y + 30, z)
    (sx2, sy2, sz2) = vectorAdd (2 * vsx, 2 * vsy, 2 * vsz) (sx1, sy1, sz1)
    infin = (x + (15000000 * (vx - x)),
             (y + 30) + (15000000 * ((vy + 30) - (y + 30))),
             z + (15000000 * (vz - z)))
    _ = normalise $ vectorSub (sx2, sy2, sz2) infin

-- | Rotates the view vector along a vector.
rotateView
  :: Camera
  -> Double
  -> Vec3
  -> Camera
rotateView cam angle (x, y, z) = Camera
  { cpos = cpos cam
  , viewPos = (nVx, nVy, nVz)
  , upVec = upVec cam
  }
  where
    (viewX, viewY, viewZ) = vectorSub (viewPos cam) (cpos cam)
    cosTheta = cos angle
    sinTheta = sin angle
    minusCosTheta = 1 - cosTheta
    nextViewX = (cosTheta   + minusCosTheta * x * x) * viewX +
                (minusCosTheta * x * y - z * sinTheta) * viewY +
                (minusCosTheta * x * z + y * sinTheta) * viewZ
    nextViewY = (minusCosTheta * x * y + z * sinTheta) * viewX +
                (cosTheta   + minusCosTheta * y * y) * viewY +
                (minusCosTheta * y * z - x * sinTheta) * viewZ
    nextViewZ = (minusCosTheta * x * y - y * sinTheta) * viewX +
                (minusCosTheta * y * z + x * sinTheta) * viewY +
                (cosTheta   + minusCosTheta * z * z) * viewZ
    (nVx, nVy, nVz) = vectorAdd (cpos cam) (nextViewX, nextViewY, nextViewZ)

-- | Moves the camera along the view vector.
move
  :: (Double, Camera)
  -> Camera
move (speed, cam) = Camera
  { cpos    = (x + newx, y + newy, z + newz)
  , viewPos = (vpx + newvx, vpy + newvy, vpz + newvz)
  , upVec   = upVec cam
  }
  where
    (x, y, z)       = cpos cam
    (vpx, vpy, vpz) = viewPos cam
    (vx, vy, vz)    = normalise (vectorSub (viewPos cam) (cpos cam))
    newx = vx * speed
    newy = vy * speed
    newz = vz * speed
    newvx = vx * speed
    newvy = vy * speed
    newvz = vz * speed

-- | Moves the camera perpendicular to the view vector.
strafe
  :: (Double, Camera)
  -> Camera
strafe (speed, cam) = Camera
  { cpos    = (newx, y, newz)
  , viewPos = (newvx, vy, newvz)
  , upVec   = upVec cam
  }
  where
    (sx, _, sz) = normalise (crossProd (vectorSub (viewPos cam) (cpos cam)) (upVec cam))
    (x, y, z) = cpos cam
    (vx, vy, vz) = viewPos cam
    newx = x + sx * speed
    newz = z + sz * speed
    newvx = vx + sx * speed
    newvz = vz + sz * speed

-- | Sets the position of the camera.
setPos
  :: Vec3
  -> Camera
  -> Camera
setPos vec cam = Camera
  { cpos    = vec
  , viewPos = viewPos cam
  , upVec   = upVec cam
  }

-- | Lowers the camera along the y-axis.
dropCam
  :: (Camera, Double)
  -> Camera
dropCam (cam, vel) = setViewPos (vectorAdd (viewPos cam1) (0, vel, 0)) cam1
  where
    cam1 = setPos (vectorAdd (cpos cam) (0, vel, 0)) cam

-- | Sets the target position the camera is looking at.
setViewPos
  :: Vec3
  -> Camera
  -> Camera
setViewPos vec cam = Camera
  { cpos    = cpos cam
  , viewPos = vec
  , upVec   = upVec cam
  }
