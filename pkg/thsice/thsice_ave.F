C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_ave.F,v 1.15 2013/05/02 20:01:21 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_AVE
C     !INTERFACE:
      SUBROUTINE THSICE_AVE(
     I                       bi, bj, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_AVE
C     | o save values for timeaveraging
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"
#include "THSICE_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     frwAtm  :: net fresh-water flux (E-P) to the atmosphere [kg/m2/s]
C     flxAtm  :: net heat flux from the atmosphere (+=down) [W/m2]
C     bi,bj   :: tile indices
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     myThid  :: thread number for this instance of the routine.
c     _RL frwAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
c     _RL flxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER bi,bj
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE
C     == Local variables ==
c     LOGICAL  DIFFERENT_MULTIPLE
c     EXTERNAL DIFFERENT_MULTIPLE
c     INTEGER wrtIter

#ifdef ALLOW_TIMEAVE

      IF ( thSIce_taveFreq .GT. 0. _d 0 ) THEN

C--    Time-average
       CALL TIMEAVE_CUMULATE( ice_fract_Ave, iceMask,
     &                        1, thSIce_deltaT, bi, bj, myThid )
C-- area weighted average (with ice-fraction)
       CALL TIMEAVE_CUMUL_2V( ice_iceH_Ave,  iceHeight, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ice_snowH_Ave, snowHeight, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ice_Tsrf_Ave,  Tsrf,  iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ice_Tice1_Ave, Tice1, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMUL_2V( ice_Tice2_Ave, Tice2, iceMask,
     &                    1, 0, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMUL_2V( ice_snowPr_Ave,snowPrc, iceMask,
c    &                    1, 0, thSIce_deltaT, bi, bj, myThid )
C--    notes: siceAlb is already multiplied by iceFraction
       CALL TIMEAVE_CUMULATE( ice_albedo_Ave, siceAlb,
     &                        1, thSIce_deltaT, bi, bj, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- simple average (without ice-fraction)
c      CALL TIMEAVE_CUMULATE( ice_iceH_Ave,iceHeight,
c    &                        1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE( ice_snowH_Ave,snowHeight,
c    &                        1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE( ice_Tsrf_Ave, Tsrf,
c    &                        1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE( ice_Tice1_Ave,Tice1,
c    &                        1, thSIce_deltaT, bi, bj, myThid )
c      CALL TIMEAVE_CUMULATE( ice_Tice2_Ave,Tice2,
c    &                        1, thSIce_deltaT, bi, bj, myThid )

C-- average of atmospheric fluxes :
      IF ( fluidIsWater ) THEN
       CALL TIMEAVE_CUMULATE( ice_flxAtm_Ave, icFlxAtm,
     &                        1, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMULATE( ice_frwAtm_Ave, icFrwAtm,
     &                        1, thSIce_deltaT, bi, bj, myThid )
      ENDIF

C-- average of oceanic fluxes :
#ifndef CHECK_ENERGY_CONSERV
#ifdef RS_IS_REAL8
       CALL TIMEAVE_CUMULATE( ice_flx2oc_Ave, Qnet,
     &                        1, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMULATE( ice_frw2oc_Ave, EmPmR,
     &                        1, thSIce_deltaT, bi, bj, myThid )
       CALL TIMEAVE_CUMULATE( ice_salFx_Ave, saltFlux,
     &                        1, thSIce_deltaT, bi, bj, myThid )
#endif
#endif /* CHECK_ENERGY_CONSERV */

       ice_timeAve(bi,bj) = ice_timeAve(bi,bj)+thSIce_deltaT

      ENDIF
#endif /* ALLOW_TIMEAVE */

c     IF ( DIFFERENT_MULTIPLE(thSIce_diagFreq,
c    &                        myTime+deltaTClock,deltaTClock)
c    &     .AND. fluidIsWater ) THEN
c       wrtIter = myIter+1
c       CALL WRITE_LOCAL_RL('ice_flxAtm','I10',1,flxAtm,
c    &                      bi,bj,1,wrtIter,myThid)
c       CALL WRITE_LOCAL_RL('ice_frwAtm','I10',1,frwAtm,
c    &                      bi,bj,1,wrtIter,myThid)
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN

C-- Atmospheric fluxes :
       IF ( fluidIsWater ) THEN
        CALL DIAGNOSTICS_FILL(icFlxAtm,'SIflxAtm',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(icFrwAtm,'SIfrwAtm',0,1,1,bi,bj,myThid)
       ENDIF

C-- Oceanic fluxes :
c #ifndef CHECK_ENERGY_CONSERV
       CALL DIAGNOSTICS_FILL_RS(Qnet,    'SIflx2oc',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL_RS(EmPmR,   'SIfrw2oc',0,1,1,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL_RS(saltFlux,'SIsaltFx',0,1,1,bi,bj,myThid)
c #endif /* CHECK_ENERGY_CONSERV */

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif  /* ALLOW_THSICE */

      RETURN
      END
