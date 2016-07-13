{- $Id: PhysicalDimensions.hs,v 1.2 2003/11/10 21:28:58 antony Exp $
******************************************************************************
*                              I N V A D E R S                               *
*                                                                            *
*       Module:         PhysicalDimensions                                   *
*       Purpose:        Type synonyms for physical dimensions and some       *
*                       related operations.                                  *
*       Author:         Henrik Nilsson                                       *
*                                                                            *
*             Copyright (c) Yale University, 2003                            *
*                                                                            *
******************************************************************************
-}

module PhysicalDimensions (
    InvaderReal,

-- One dimensional
    Time,
    Frequency,
    Mass,
    Length,
    Distance,
    Position,
    Speed,
    Velocity,
    Acceleration,
    Angle,
    Heading,
    Bearing,
    RotVel,
    RotAcc,

-- Two dimensional
    Distance2,
    Position2,
    Velocity2,
    Acceleration2,

-- Three dimensional
    Distance3,
    Position3,
    Velocity3,
    Acceleration3,
) where

import FRP.Yampa.Core (Time)
import FRP.Yampa.Geometry (Vector2, Vector3, Point2, Point3)


-- Many of the physical dimensions below are related to time, and variables
-- of these types can thus be expected to occur in numerical expressions along
-- with variables of type time. To facilitate things, they should thus share
-- the same representation. Maybe it is a mistake that FRP.Yampa has fixed the
-- type of Time (currently to Double)?

-- Dimensionless type. Same representation as FRP.Yampa's Time.
type InvaderReal = Time

------------------------------------------------------------------------------
-- One-dimensional types
------------------------------------------------------------------------------

type Frequency    = InvaderReal -- [Hz]
type Mass         = InvaderReal -- [kg]
type Length       = InvaderReal -- [m]
type Position     = InvaderReal -- [m]   (absolute)
type Distance     = InvaderReal -- [m]   (relative)
type Speed        = InvaderReal -- [m/s] (unsigned, speed = abs(velocity))
type Velocity     = InvaderReal -- [m/s] (signed)
type Acceleration = InvaderReal -- [m/s^2]
type Angle        = InvaderReal -- [rad] (relative)
type Heading      = InvaderReal -- [rad] (angle relative to x-axis = east)
type Bearing      = InvaderReal -- [deg] (compass direction, 0 = N, 90 = E)
type RotVel       = InvaderReal -- [rad/s]
type RotAcc       = InvaderReal -- [rad/s^2]


------------------------------------------------------------------------------
-- Two-dimensional types
------------------------------------------------------------------------------

type Position2     = Point2 Position                    -- [m]     (absolute)
type Distance2     = Vector2 Distance                   -- [m]     (relative)
type Velocity2     = Vector2 Velocity                   -- [m/s]
type Acceleration2 = Vector2 Acceleration               -- [m/s^2]


------------------------------------------------------------------------------
-- Three-dimensional types
------------------------------------------------------------------------------

type Position3     = Point3 Position                    -- [m]     (absolute)
type Distance3     = Vector3 Distance                   -- [m]     (relative)
type Velocity3     = Vector3 Velocity                   -- [m/s]
type Acceleration3 = Vector3 Acceleration               -- [m/s^2]
