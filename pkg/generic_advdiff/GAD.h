C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD.h,v 1.9 2002/06/15 03:31:17 jmc Exp $
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

C ENUM_CENTERED_2ND :: Centered 2nd order
      INTEGER ENUM_CENTERED_2ND
      PARAMETER(ENUM_CENTERED_2ND=2)

C ENUM_UPWIND_3RD :: 3rd order upwind 
      INTEGER ENUM_UPWIND_3RD
      PARAMETER(ENUM_UPWIND_3RD=3)

C ENUM_CENTERED_4TH :: Centered 4th order
      INTEGER ENUM_CENTERED_4TH
      PARAMETER(ENUM_CENTERED_4TH=4)

C ENUM_FLUX_LIMIT :: Non-linear flux limiter
      INTEGER ENUM_FLUX_LIMIT
      PARAMETER(ENUM_FLUX_LIMIT=77)

C ENUM_DST3 :: 3-DST 
      INTEGER ENUM_DST3
      PARAMETER(ENUM_DST3=30)

C ENUM_DST3_FLUX_LIMIT :: 3-DST flux limited
      INTEGER ENUM_DST3_FLUX_LIMIT
      PARAMETER(ENUM_DST3_FLUX_LIMIT=33)

C oneSixth :: Third/fourth order interpolation factor
      _RL oneSixth
      PARAMETER(oneSixth=1.D0/6.D0)

C Differentiate between tracers (only needed for KPP -  arrgh!!!)
C GAD_TEMPERATURE :: temperature
      INTEGER GAD_TEMPERATURE
      PARAMETER(GAD_TEMPERATURE=101)
C GAD_SALINITY :: salinity
      INTEGER GAD_SALINITY
      PARAMETER(GAD_SALINITY=102)
C GAD_TR1 :: pssive tracer 1
      INTEGER GAD_TR1
      PARAMETER(GAD_TR1=103)
CEOP

C--   COMMON /GAD_PARM_L/ Logical parameters for GAD pkg routines
C     tempMultiDimAdvec :: set to T if using multi-dimension advection for Temp
C     saltMultiDimAdvec :: set to T if using multi-dimension advection for Salt
C     tempAdamsBashforth :: set to T if using Adams-Bashforth stepping for Temp
C     saltAdamsBashforth :: set to T if using Adams-Bashforth stepping for Salt
      LOGICAL tempMultiDimAdvec
      LOGICAL saltMultiDimAdvec
      LOGICAL tr1_MultiDimAdvec
      LOGICAL tempAdamsBashforth
      LOGICAL saltAdamsBashforth
      LOGICAL tr1_AdamsBashforth
      COMMON /GAD_PARM_L/
     & tempMultiDimAdvec , saltMultiDimAdvec , tr1_MultiDimAdvec,
     & tempAdamsBashforth, saltAdamsBashforth, tr1_AdamsBashforth
