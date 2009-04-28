C $Header: /u/gcmpack/MITgcm/model/src/Attic/do_gterm_blocking_exchanges.F,v 1.18 2009/04/28 18:01:14 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DO_GTERM_BLOCKING_EXCHANGES
C     !INTERFACE:
      SUBROUTINE DO_GTERM_BLOCKING_EXCHANGES(myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE DO_GTERM_BLOCKING_EXCHANGES                    
C     | o Controlling routine for exchanging edge info.           
C     *==========================================================*
C     | One key trick used in UV us that we over-compute and      
C     | arrange our time-stepping loop so that we only need one   
C     | edge exchange for the explicit code per timestep.         
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid - Thread number for this instance of the routine.
      INTEGER myThid
CEOP

c     _EXCH_XYZ_RL( Gu , myThid )
c     _EXCH_XYZ_RL( Gv , myThid )
c     _EXCH_XYZ_RL( Gt , myThid )
c     _EXCH_XYZ_RL( Gs , myThid )
#ifdef ALLOW_ADAMSBASHFORTH_3
C_jmc: requires to pass "myIter" as argument !!!
C_jmc: leave it commented since this S/R is never called
c     m1 = 1 + mod(myIter+1,2)
c     CALL EXCH_UV_XYZ_RL(guNm(1-Olx,1-Oly,1,1,1,m1),
c    &                    gvNm(1-Olx,1-Oly,1,1,1,m1),.TRUE.,myThid)
c     _EXCH_XYZ_RL( gtNm(1-Olx,1-Oly,1,1,1,m1), myThid )
c     _EXCH_XYZ_RL( gsNm(1-Olx,1-Oly,1,1,1,m1), myThid )
#else /* ALLOW_ADAMSBASHFORTH_3 */
      CALL EXCH_UV_XYZ_RL(guNm1,gvNm1,.TRUE.,myThid)
c     _EXCH_XYZ_RL( guNm1 , myThid )
c     _EXCH_XYZ_RL( gvNm1 , myThid )
      _EXCH_XYZ_RL( gtNm1 , myThid )
      _EXCH_XYZ_RL( gsNm1 , myThid )
#endif /* ALLOW_ADAMSBASHFORTH_3 */
c     _EXCH_XYZ_RL( uVel , myThid )
c     _EXCH_XYZ_RL( vVel , myThid )
c     _EXCH_XYZ_RL( theta , myThid )
c     _EXCH_XYZ_RL( salt , myThid )

#ifdef ALLOW_PTRACERS
ceh3 add an IF ( usePTRACERS ) THEN
      CALL PTRACERS_GTERM_BLOCKING_EXCH(myThid)
#endif /* ALLOW PTRACERS */

      RETURN
      END
