C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/GAD.h,v 1.4 2001/07/30 20:42:45 heimbach Exp $
C $Name:  $

C Parameter common bloack
      INTEGER gad_advection_scheme
c     COMMON /GAD_PARS/
c    &    gad_advection_scheme
      PARAMETER(gad_advection_scheme=2)

C Enumerated constants for selecting advection schemes

C Centered 2nd order
      INTEGER ENUM_CENTERED_2ND
      PARAMETER(ENUM_CENTERED_2ND=2)

C 3rd order upwind 
      INTEGER ENUM_UPWIND_3RD
      PARAMETER(ENUM_UPWIND_3RD=3)

C Centered 4th order
      INTEGER ENUM_CENTERED_4TH
      PARAMETER(ENUM_CENTERED_4TH=4)

C Non-linear flux limiter
      INTEGER ENUM_FLUX_LIMIT
      PARAMETER(ENUM_FLUX_LIMIT=77)

C Third/fourth order interpolation factor
      _RL oneSixth
      PARAMETER(oneSixth=1.D0/6.D0)

C Differentiate between tracers (only needed for KPP -  arrgh!!!)
      INTEGER GAD_TEMPERATURE
      PARAMETER(GAD_TEMPERATURE=101)
      INTEGER GAD_SALINITY
      PARAMETER(GAD_SALINITY=102)
      INTEGER GAD_TR1
      PARAMETER(GAD_TR1=103)
