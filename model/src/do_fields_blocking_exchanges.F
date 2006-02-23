C $Header: /u/gcmpack/MITgcm/model/src/do_fields_blocking_exchanges.F,v 1.20 2006/02/23 20:55:48 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_FIELDS_BLOCKING_EXCHANGES
C     !INTERFACE:
      SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES(myThid)
C     !DESCRIPTION: \bv
C     /==========================================================\
C     | SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES                  |
C     | o Controlling routine for exchanging edge info.          |
C     |==========================================================|
C     | One key trick used in UV us that we over-compute and     |
C     | arrange our time-stepping loop so that we only need one  |
C     | edge exchange for the explicit code per timestep.        |
C     \==========================================================/
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#ifdef ALLOW_CD_CODE
#include "CD_CODE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

c     _EXCH_XYZ_R8( Gu , myThid )
c     _EXCH_XYZ_R8( Gv , myThid )
c     _EXCH_XYZ_R8( Gt , myThid )
c     _EXCH_XYZ_R8( Gs , myThid )
c     _EXCH_XYZ_R8( gUNm1 , myThid )
c     _EXCH_XYZ_R8( gVNm1 , myThid )
c     _EXCH_XYZ_R8( gTNm1 , myThid )
c     _EXCH_XYZ_R8( gSNm1 , myThid )
c     _EXCH_XYZ_R8( uVel , myThid )
c     _EXCH_XYZ_R8( vVel , myThid )
#ifndef ALLLOW_OFFLINE
      IF ( .NOT.staggerTimeStep .OR. .NOT.useMultiDimAdvec ) THEN
       CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
       IF ( .NOT.implicitIntGravWave )
     & _EXCH_XYZ_R8( wVel , myThid )
      ENDIF
c     IF ( exactConserv .AND. implicDiv2Dflow .NE. 0. _d 0)
c    & _EXCH_XY_R8( etaN , myThid )
      IF ( .NOT.implicitIntGravWave ) THEN
       _EXCH_XYZ_R8( theta, myThid )
       _EXCH_XYZ_R8( salt , myThid )
      ENDIF
#endif

#ifndef ALLOW_OFFLINE
#ifdef ALLOW_CD_CODE
c     _EXCH_XYZ_R8( guCD , myThid )
c     _EXCH_XYZ_R8( gvCD , myThid )
      _EXCH_XYZ_R8( uVelD , myThid )
      _EXCH_XYZ_R8( vVelD , myThid )
#endif
      IF ( useDynP_inEos_Zc )
     & _EXCH_XYZ_RL( totPhiHyd , myThid )
#endif

#ifdef ALLOW_PTRACERS
      IF (usePTRACERS)
     &  CALL PTRACERS_FIELDS_BLOCKING_EXCH(myThid)
#endif /* ALLOW PTRACERS */

#ifdef ALLOW_GGL90
      IF (useGGL90)
     &  CALL GGL90_EXCHANGES(myThid)
#endif /* ALLOW_GGL90 */

      RETURN
      END
