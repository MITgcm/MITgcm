C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ave.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CStartofinterface
C     !ROUTINE: THSICE_AVE
C     !INTERFACE:
      SUBROUTINE THSICE_AVE( bi, bj, myThid )
C     *==========================================================*
C     | S/R  THSICE_AVE                                              
C     | o save values for timeaveraging
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
c #include "EEPARAMS.h"
c #include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE.h"
#include "THSICE_DIAGS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
      INTEGER bi,bj
      INTEGER myThid
CEndofinterface

#ifdef ALLOW_THSICE
#ifdef ALLOW_TIMEAVE

C--    Time-average
       CALL TIMEAVE_CUMULATE(ICE_iceMask_AVE,iceMask, 
     &                       1, thSIce_deltaT, bi, bj, myThid )
C-- area weighted average (with ice-fraction)
       CALL TIMEAVE_CUMUL_2V( ICE_iceH_AVE, iceheight, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ICE_snowH_AVE, snowheight, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ICE_Tsrf_AVE,  Tsrf,  iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ICE_Tice1_AVE, Tice1, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ICE_Tice2_AVE, Tice2, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ICE_snow_AVE,  snow,  iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- simple average (without ice-fraction)
c      CALL TIMEAVE_CUMULATE(ICE_iceH_AVE,iceheight,
c    &                       1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE(ICE_snowH_AVE,snowHeight,
c    &                       1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE(ICE_Tsrf_AVE, Tsrf,
c    &                       1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE(ICE_Tice1_AVE,Tice1,
c    &                       1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE(ICE_Tice2_AVE,Tice2,
c    &                       1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE(ICE_snow_AVE, snow,
c    &                       1, thSIce_deltaT, bi, bj, myThid )

       ICE_timeave(1,bi,bj)=ICE_timeave(1,bi,bj)+thSIce_deltaT

#endif /* ALLOW_TIMEAVE */
#endif  /* ALLOW_THSICE */

      RETURN
      END
