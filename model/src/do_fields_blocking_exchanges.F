C $Header: /u/gcmpack/MITgcm/model/src/do_fields_blocking_exchanges.F,v 1.3 2001/02/04 14:38:46 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

      SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES(myThid)
C     /==========================================================\
C     | SUBROUTINE DO_FIELDS_BLOCKING_EXCHANGES                  |
C     | o Controlling routine for exchanging edge info.          |
C     |==========================================================|
C     | One key trick used in UV us that we over-compute and     |
C     | arrange our time-stepping loop so that we only need one  |
C     | edge exchange for the explicit code per timestep.        |
C     \==========================================================/
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"

C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid

c     _EXCH_XYZ_R8( Gu , myThid )
c     _EXCH_XYZ_R8( Gv , myThid )
c     _EXCH_XYZ_R8( Gt , myThid )
c     _EXCH_XYZ_R8( Gs , myThid )
c     _EXCH_XYZ_R8( gUNm1 , myThid )
c     _EXCH_XYZ_R8( gVNm1 , myThid )
c     _EXCH_XYZ_R8( gTNm1 , myThid )
c     _EXCH_XYZ_R8( gSNm1 , myThid )
      _EXCH_XYZ_R8( uVel , myThid )
      _EXCH_XYZ_R8( vVel , myThid )
      _EXCH_XYZ_R8( theta , myThid )
      _EXCH_XYZ_R8( salt , myThid )

#ifdef INCLUDE_CD_CODE
c     _EXCH_XYZ_R8( guCD , myThid )
c     _EXCH_XYZ_R8( gvCD , myThid )
      _EXCH_XYZ_R8( uVelD , myThid )
      _EXCH_XYZ_R8( vVelD , myThid )
#endif

      RETURN
      END
 
