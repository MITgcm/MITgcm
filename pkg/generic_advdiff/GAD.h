C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD.h,v 1.13 2005/10/22 19:56:33 jmc Exp $
C $Name:  $

CBOP
C !ROUTINE: GAD.h

C !INTERFACE:
C #include "GAD.h"

C !DESCRIPTION:
C Contains enumerated constants for distinguishing between different
C advection schemes and tracers.
C
C Unfortunately, there is no easy way to make use of the
C tokens in namelist input so for now we have to enter the
C tokens value into "data" (ie. 2 for 2nd order etc.)

C !USES:
#include "GAD_OPTIONS.h"

C !DEFINED PARAMETERS:

C ENUM_UPWIND_1RST :: 1rst Order Upwind
      INTEGER ENUM_UPWIND_1RST
      PARAMETER(ENUM_UPWIND_1RST=1)

C ENUM_CENTERED_2ND :: Centered 2nd order
      INTEGER ENUM_CENTERED_2ND
      PARAMETER(ENUM_CENTERED_2ND=2)

C ENUM_UPWIND_3RD :: 3rd order upwind
      INTEGER ENUM_UPWIND_3RD
      PARAMETER(ENUM_UPWIND_3RD=3)

C ENUM_CENTERED_4TH :: Centered 4th order
      INTEGER ENUM_CENTERED_4TH
      PARAMETER(ENUM_CENTERED_4TH=4)

C ENUM_DST2 :: 2nd Order Direct Space and Time (= Lax-Wendroff)
      INTEGER ENUM_DST2
      PARAMETER(ENUM_DST2=20)

C ENUM_FLUX_LIMIT :: Non-linear flux limiter
      INTEGER ENUM_FLUX_LIMIT
      PARAMETER(ENUM_FLUX_LIMIT=77)

C ENUM_DST3 :: 3rd Order Direst Space and Time
      INTEGER ENUM_DST3
      PARAMETER(ENUM_DST3=30)

C ENUM_DST3_FLUX_LIMIT :: 3-DST flux limited
      INTEGER ENUM_DST3_FLUX_LIMIT
      PARAMETER(ENUM_DST3_FLUX_LIMIT=33)

C oneSixth :: Third/fourth order interpolation factor
      _RL oneSixth
      PARAMETER(oneSixth=1.D0/6.D0)

C Differentiate between tracers (needed for KPP - arrgh!!!)
cph                              and GMRedi arrgh*arrgh!!!)
cph  indices are used for TAF key computations, so need to
cph  running from 1, 2, ...
c
C GAD_TEMPERATURE :: temperature
      INTEGER GAD_TEMPERATURE
      PARAMETER(GAD_TEMPERATURE=1)
C GAD_SALINITY :: salinity
      INTEGER GAD_SALINITY
      PARAMETER(GAD_SALINITY=2)
C GAD_TR1 :: passive tracer 1
      INTEGER GAD_TR1
      PARAMETER(GAD_TR1=3)
CEOP

C--   COMMON /GAD_PARM_L/ Logical parameters for GAD pkg routines
C tempMultiDimAdvec :: set to T if using multi-dim advection for Temp
C saltMultiDimAdvec :: set to T if using multi-dim advection for Salt
C tempAdamsBashforth :: set to T if using Adams-Bashforth stepping for Temp
C saltAdamsBashforth :: set to T if using Adams-Bashforth stepping for Salt
      LOGICAL tempMultiDimAdvec
      LOGICAL saltMultiDimAdvec
      LOGICAL tempAdamsBashforth
      LOGICAL saltAdamsBashforth
      COMMON /GAD_PARM_L/
     & tempMultiDimAdvec , saltMultiDimAdvec ,
     & tempAdamsBashforth, saltAdamsBashforth

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
