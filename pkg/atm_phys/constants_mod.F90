!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                                                   !!
!!                   GNU General Public License                      !!
!!                                                                   !!
!! This file is part of the Flexible Modeling System (FMS).          !!
!!                                                                   !!
!! FMS is free software; you can redistribute it and/or modify       !!
!! it and are expected to follow the terms of the GNU General Public !!
!! License as published by the Free Software Foundation.             !!
!!                                                                   !!
!! FMS is distributed in the hope that it will be useful,            !!
!! but WITHOUT ANY WARRANTY; without even the implied warranty of    !!
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the     !!
!! GNU General Public License for more details.                      !!
!!                                                                   !!
!! You should have received a copy of the GNU General Public License !!
!! along with FMS; if not, write to:                                 !!
!!          Free Software Foundation, Inc.                           !!
!!          59 Temple Place, Suite 330                               !!
!!          Boston, MA  02111-1307  USA                              !!
!! or see:                                                           !!
!!          http://www.gnu.org/licenses/gpl.txt                      !!
!!                                                                   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! $Header: /u/gcmpack/MITgcm/pkg/atm_phys/constants_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $
! $Name:  $

module constants_mod

! <CONTACT EMAIL="Bruce.Wyman@noaa.gov">
!   Bruce Wyman
! </CONTACT>

! <HISTORY SRC="http://www.gfdl.noaa.gov/fms-cgi-bin/cvsweb.cgi/FMS/"/>

! <OVERVIEW>
!    Defines useful constants for Earth.
! </OVERVIEW>

! <DESCRIPTION>
!   Constants are defined as real parameters,
!   except for PI and RADIAN, which are calculated in constants_init to promote
!   consistency and accuracy among various compilers.
!
!   Constants are accessed through the "use" statement.
! </DESCRIPTION>

!use fms_mod, only: write_version_number

implicit none
private

character(len=128) :: version='$Id: constants_mod.F90,v 1.1 2013/05/08 22:14:14 jmc Exp $'
character(len=128) :: tagname='$Name:  $'
logical :: do_log = .true.
logical :: module_is_initialized = .FALSE.
!dummy variable to use in HUGE initializations
real :: realnumber

!------------ physical constants ---------------
! <DATA NAME="RADIUS" UNITS="m" TYPE="real" DEFAULT="6371.e3">
!   radius of the earth
! </DATA>
! <DATA NAME="OMEGA" UNITS="1/s" TYPE="real" DEFAULT="7.292e-5">
!   rotation rate of the planet (earth)
! </DATA>
! <DATA NAME="GRAV" UNITS="m/s^2" TYPE="real" DEFAULT="9.80">
!   acceleration due to gravity
! </DATA>
! <DATA NAME="RDGAS" UNITS="J/kg/deg" TYPE="real" DEFAULT="287.04">
!   gas constant for dry air
! </DATA>
! <DATA NAME="KAPPA" TYPE="real" DEFAULT="2./7.">
!   RDGAS / CP_AIR
! </DATA>
! <DATA NAME="CP_AIR" UNITS="J/kg/deg" TYPE="real" DEFAULT="RDGAS/KAPPA">
!   specific heat capacity of dry air at constant pressure
! </DATA>
! <DATA NAME="CP_OCEAN" UNITS="J/kg/deg" TYPE="real" DEFAULT="3989.24495292815">
!   specific heat capacity taken from McDougall (2002) "Potential Enthalpy ..."
! </DATA>
! <DATA NAME="RHO0" UNITS="kg/m^3" TYPE="real" DEFAULT="1.035e3">
!   average density of sea water
! </DATA>
! <DATA NAME="RHO0R" UNITS="m^3/kg" TYPE="real" DEFAULT="1.0/RHO0">
!   reciprocal of average density of sea water
! </DATA>
! <DATA NAME="RHO_CP" UNITS="J/m^3/deg" TYPE="real" DEFAULT="RHO0*CP_OCEAN">
!   (kg/m^3)*(cal/kg/deg C)(joules/cal) = (joules/m^3/deg C)
! </DATA>

real, public, parameter :: RADIUS = 6371.0e3
real, public, parameter :: OMEGA  = 7.292e-5
real, public, parameter :: GRAV   = 9.80
real, public, parameter :: RDGAS  = 287.04
real, public, parameter :: KAPPA  = 2./7.
real, public, parameter :: CP_AIR = RDGAS/KAPPA
real, public, parameter :: CP_OCEAN = 3989.24495292815
real, public, parameter :: RHO0    = 1.035e3
real, public, parameter :: RHO0R   = 1.0/RHO0
real, public, parameter :: RHO_CP  = RHO0*CP_OCEAN

!------------ water vapor constants ---------------
! <DATA NAME="RVGAS" UNITS="J/kg/deg" TYPE="real" DEFAULT="461.50">
!   gas constant for water vapor
! </DATA>
! <DATA NAME="DENS_H2O" UNITS="kg/m^3" TYPE="real" DEFAULT="1000.">
!   density of liquid water
! </DATA>
! <DATA NAME="HLV" UNITS="J/kg" TYPE="real" DEFAULT="2.500e6">
!   latent heat of evaporation
! </DATA>
! <DATA NAME="HLF" UNITS="J/kg" TYPE="real" DEFAULT="3.34e5">
!   latent heat of fusion
! </DATA>
! <DATA NAME="HLS" UNITS="J/kg" TYPE="real" DEFAULT="2.834e6">
!   latent heat of sublimation
! </DATA>
! <DATA NAME="TFREEZE" UNITS="degK" TYPE="real" DEFAULT="273.16">
!   temp where fresh water freezes
! </DATA>

real, public, parameter :: RVGAS = 461.50
real, public, parameter :: DENS_H2O = 1000.
real, public, parameter :: HLV = 2.500e6
real, public, parameter :: HLF = 3.34e5
real, public, parameter :: HLS = 2.834e6
real, public, parameter :: TFREEZE = 273.16

!-------------- radiation constants -----------------

! <DATA NAME="WTMAIR" UNITS="AMU" TYPE="real" DEFAULT="2.896440E+01">
!  molecular weight of air
! </DATA>
! <DATA NAME="WTMH2O" UNITS="AMU" TYPE="real" DEFAULT="1.801534E+01">
!  molecular weight of water
! </DATA>
! <DATA NAME="WTMO3" UNITS="AMU" TYPE="real" DEFAULT="47.99820E+01">
!   molecular weight of ozone
! </DATA>
! <DATA NAME="DIFFAC" TYPE="real" DEFAULT="1.660000E+00">
! diffusivity factor
! </DATA>
! <DATA NAME="SECONDS_PER_DAY" UNITS="seconds" TYPE="real" DEFAULT="8.640000E+04">
! seconds in a day
! </DATA>
! <DATA NAME="AVOGNO" UNITS="atoms/mole" TYPE="real" DEFAULT="6.023000E+23">
!  Avogadro's number
! </DATA>
! <DATA NAME="PSTD" UNITS="dynes/cm^2" TYPE="real" DEFAULT="1.013250E+06">
!  mean sea level pressure
! </DATA>
! <DATA NAME="PSTD_MKS" UNITS="Newtons/m^2" TYPE="real" DEFAULT="101325.0">
!  mean sea level pressure
! </DATA>
! <DATA NAME="REARTH" UNITS="cm" TYPE="real" DEFAULT="6.356766E+08">
!  radius of the earth
! </DATA>

real, public, parameter :: WTMAIR = 2.896440E+01
real, public, parameter :: WTMH2O = 1.801534E+01
real, public, parameter :: WTMO3       = 47.99820E+01
real, public, parameter :: DIFFAC = 1.660000E+00
real, public, parameter :: SECONDS_PER_DAY  = 8.640000E+04
real, public, parameter :: AVOGNO = 6.023000E+23
real, public, parameter :: PSTD    = 1.013250E+06
real, public, parameter :: PSTD_MKS    = 101325.0
real, public, parameter :: REARTH  = 6.356766E+08

! <DATA NAME="RADCON" UNITS="deg sec/(cm day)" TYPE="real" DEFAULT="((1.0E+02*GRAV)/(1.0E+04*CP_AIR))*SECONDS_PER_DAY">
!  factor used to convert flux divergence to heating rate in degrees per day
! </DATA>
! <DATA NAME="RADCON_MKS" UNITS="deg sec/(m day)" TYPE="real" DEFAULT="(GRAV/CP_AIR)*SECONDS_PER_DAY">
!  factor used to convert flux divergence to heating rate in degrees per day
! </DATA>
! <DATA NAME="O2MIXRAT" TYPE="real" DEFAULT="2.0953E-01">
! mixing ratio of molecular oxygen in air
! </DATA>
! <DATA NAME="RHOAIR" UNITS="kg/m^3" TYPE="real" DEFAULT="1.292269">
!  reference atmospheric density
! </DATA>
! <DATA NAME="ALOGMIN" TYPE="real" DEFAULT="-50.0">
!  minimum value allowed as argument to log function
! </DATA>
! <DATA NAME="FREZDK" UNITS="deg K" TYPE="real" DEFAULT="273.16">
!   melting point of water
! </DATA>

real, public, parameter :: RADCON = ((1.0E+02*GRAV)/(1.0E+04*CP_AIR))*SECONDS_PER_DAY
real, public, parameter :: RADCON_MKS  = (GRAV/CP_AIR)*SECONDS_PER_DAY
real, public, parameter :: O2MIXRAT    = 2.0953E-01
real, public, parameter :: RHOAIR      = 1.292269
real, public, parameter :: ALOGMIN     = -50.0
real, public, parameter :: FREZDK      = 273.16

!------------ miscellaneous constants ---------------
! <DATA NAME="STEFAN" UNITS="W/m^2/deg^4" TYPE="real" DEFAULT="5.6734e-8">
!   Stefan-Boltzmann constant
! </DATA>
! <DATA NAME="VONKARM"  TYPE="real" DEFAULT="0.40">
!   Von Karman constant
! </DATA>
! <DATA NAME="PI" TYPE="real" DEFAULT="4.0*ATAN(1.0)">
!    ratio of circle circumference to diameter
! </DATA>
! <DATA NAME="RADIAN"  TYPE="real" DEFAULT="180.0/PI">
!   degrees per radian
! </DATA>
! <DATA NAME="C2DBARS" UNITS="dbars" TYPE="real" DEFAULT="1.e-4">
!   converts rho*g*z (in mks) to dbars: 1dbar = 10^4 (kg/m^3)(m/s^2)m
! </DATA>
! <DATA NAME="KELVIN" TYPE="real" DEFAULT="273.15">
!   degrees Kelvin at zero Celsius
! </DATA>
! <DATA NAME="EPSLN" TYPE="real" DEFAULT="1.0e-40">
!   a small number to prevent divide by zero exceptions
! </DATA>

real, public, parameter :: STEFAN  = 5.6734e-8
real, public, parameter :: VONKARM = 0.40
real, public            :: PI      = HUGE(realnumber)
real, public            :: RADIAN  = HUGE(realnumber)
real, public, parameter :: C2DBARS = 1.e-4
real, public, parameter :: KELVIN  = 273.15
real, public, parameter :: EPSLN   = 1.0e-40
!-----------------------------------------------------------------------

public constants_init

contains

! <SUBROUTINE NAME="constants_init">

!   <OVERVIEW>
!     Initialization routine. The purpose of this routine
!     is initialize the value of PI and RADIAN, and to write the
!     version and tag name information to the log file.
!   </OVERVIEW>
!   <DESCRIPTION>
!     If this routine is called more than once or called
!     from other than the root PE it will return silently.
!     There are no arguments.
!   </DESCRIPTION>
!   <TEMPLATE>
!     call constants_init
!   </TEMPLATE>

subroutine constants_init

  if (module_is_initialized) return
  module_is_initialized = .TRUE.

  if (.not.do_log) return
! call write_version_number (version,tagname)
  do_log = .false.

  PI = 4.0*ATAN(1.0)
  RADIAN = 180.0/PI

end subroutine constants_init
! </SUBROUTINE>

!-----------------------------------------------------------------------

end module constants_mod

! <INFO>

!   <FUTURE>
!   1.  Renaming of constants.
!   </FUTURE>
!   <FUTURE>
!   2.  Additional constants.
!   </FUTURE>
!   <NOTE>
!    Constants have been declared as type REAL, PARAMETER.
!
!    The value a constant can not be changed in a users program.
!    New constants can be defined in terms of values from the
!    constants module using a parameter statement.<br><br>
!
!    The name given to a particular constant may be changed.<br><br>
!
!    Constants can be used on the right side on an assignment statement
!    (their value can not be reassigned).
!
!    As PI is calculated, it cannot be a parameter variable.
!    Do not assign a new value to PI.
!
!<TESTPROGRAM NAME="EXAMPLE">
!<PRE>
!    use constants_mod, only:  TFREEZE, grav_new => GRAV
!    real, parameter :: grav_inv = 1.0 / grav_new
!    tempc(:,:,:) = tempk(:,:,:) - TFREEZE
!    geopotential(:,:) = height(:,:) * grav_new
!</PRE>
!</TESTPROGRAM>
!   </NOTE>

! </INFO>

