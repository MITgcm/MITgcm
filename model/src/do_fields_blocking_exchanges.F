C $Header: /u/gcmpack/MITgcm/model/src/do_fields_blocking_exchanges.F,v 1.12 2003/10/28 22:57:59 edhill Exp $
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
#ifdef ALLOW_PASSIVE_TRACER
#include "TR1.h"
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
      CALL EXCH_UV_XYZ_RL(uVel,vVel,.TRUE.,myThid)
      _EXCH_XYZ_R8( wVel , myThid )
      IF ( exactConserv .AND. implicDiv2Dflow .NE. 0. _d 0)
     & _EXCH_XY_R8( etaN , myThid )
      IF ( .NOT.staggerTimeStep .OR. cAdjFreq .NE. 0. _d 0) THEN
      _EXCH_XYZ_R8( theta , myThid )
      _EXCH_XYZ_R8( salt , myThid )
      ENDIF
#ifdef ALLOW_PASSIVE_TRACER
ceh3 needs an IF ( usePASSIVE_TRACER ) THEN
      _EXCH_XYZ_R8( Tr1  , myThid )
#endif

#ifdef ALLOW_CD_CODE
c     _EXCH_XYZ_R8( guCD , myThid )
c     _EXCH_XYZ_R8( gvCD , myThid )
      _EXCH_XYZ_R8( uVelD , myThid )
      _EXCH_XYZ_R8( vVelD , myThid )
#endif
      IF ( useDynP_inEos_Zc )
     & _EXCH_XYZ_RL( totPhiHyd , myThid )

#ifdef ALLOW_PTRACERS
ceh3 needs an IF ( usePTRACER ) THEN
      CALL PTRACERS_FIELDS_BLOCKING_EXCHANGES(myThid)
#endif /* ALLOW PTRACERS */

      RETURN
      END
