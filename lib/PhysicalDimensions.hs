-- |
module PhysicalDimensions where

import           FRP.Yampa.Core
import           FRP.Yampa.Geometry

-- Many of the physical dimensions below are related to time, and variables
-- of these types can thus be expected to occur in numerical expressions along
-- with variables of type time. To facilitate things, they should thus share
-- the same representation. Maybe it is a mistake that FRP.Yampa has fixed the
-- type of Time (currently to Double)?

-- | Dimensionless type. Same representation as FRP.Yampa's Time.
type InvaderReal = Time

------------------------------------------------------------------------------
-- * One-dimensional types
------------------------------------------------------------------------------

-- | [Hz].
type Frequency    = InvaderReal

-- | [kg].
type Mass         = InvaderReal

-- | [m].
type Length       = InvaderReal

-- | [m].
-- (absolute).
type Position     = InvaderReal

-- | [m].
-- (relative).
type Distance     = InvaderReal

-- | [m/s].
-- (unsigned, speed = abs(velocity)).
type Speed        = InvaderReal

-- | [m/s].
-- (signed).
type Velocity     = InvaderReal

-- | [m/s^2].
type Acceleration = InvaderReal

-- | [rad].
-- (relative).
type Angle        = InvaderReal

-- | [rad].
-- (angle relative to x-axis = east).
type Heading      = InvaderReal

-- | [deg].
-- (compass direction, 0 = N, 90 = E).
type Bearing      = InvaderReal

-- | [rad/s].
type RotVel       = InvaderReal

-- | [rad/s^2].
type RotAcc       = InvaderReal

------------------------------------------------------------------------------
-- * Two-dimensional types
------------------------------------------------------------------------------

-- | [m].
-- (absolute).
type Position2     = Point2 Position

-- | [m].
-- (relative).
type Distance2     = Vector2 Distance

-- | [m/s].
type Velocity2     = Vector2 Velocity

-- | [m/s^2].
type Acceleration2 = Vector2 Acceleration

------------------------------------------------------------------------------
-- * Three-dimensional types
------------------------------------------------------------------------------

-- | [m].
-- (absolute).
type Position3     = Point3 Position

-- | [m].
-- (relative).
type Distance3     = Vector3 Distance

-- | [m/s].
type Velocity3     = Vector3 Velocity

-- | [m/s^2].
type Acceleration3 = Vector3 Acceleration
