C $Header: /u/gcmpack/MITgcm/model/src/Attic/do_gterm_blocking_exchanges.F,v 1.7 1998/10/28 03:11:37 cnh Exp $

#include "CPP_OPTIONS.h"

      SUBROUTINE DO_GTERM_BLOCKING_EXCHANGES(myThid)
C     /==========================================================\
C     | SUBROUTINE DO_GTERM_BLOCKING_EXCHANGES                   |
C     | o Controlling routine for exchanging edge info.          |
C     |==========================================================|
C     | One key trick used in UV us that we over-compute and     |
C     | arrange our time-stepping loop so that we only need one  |
C     | edge exchange for the explicit code per timestep.        |
C     \==========================================================/

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
      _EXCH_XYZ_R8( gUNm1 , myThid )
      _EXCH_XYZ_R8( gVNm1 , myThid )
      _EXCH_XYZ_R8( gTNm1 , myThid )
      _EXCH_XYZ_R8( gSNm1 , myThid )
c     _EXCH_XYZ_R8( uVel , myThid )
c     _EXCH_XYZ_R8( vVel , myThid )
c     _EXCH_XYZ_R8( theta , myThid )
c     _EXCH_XYZ_R8( salt , myThid )
#ifdef ALLOW_CD
      _EXCH_XYZ_R8( guCD , myThid )
      _EXCH_XYZ_R8( gvCD , myThid )
#endif
CcnhDebugStarts
Cdbg  _EXCH_XYZ_R8( phSave , myThid )
CcnhDebugEnds

      RETURN
      END
 
