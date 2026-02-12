#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: COST_OBCS
C     !INTERFACE:
      subroutine cost_obcs(
     I                      myTime, myIter, myThid )
C     !DESCRIPTION: \bv
c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================
c
c     o Evaluate cost function contributions for obcs
c
c     ==================================================================
c     SUBROUTINE cost_obcs
c     ==================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

c     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"

#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
#endif
#ifdef ALLOW_OBCS_CONTROL
# include "CTRL_OBCS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
      _RL     myTime
      integer myIter
      integer myThid

#if ( defined ALLOW_CTRL && defined ALLOW_OBCS )
C     !LOCAL VARIABLES:
      integer startrec
      integer endrec
      integer ivar
      integer iobcsN, iobcsS, iobcsE, iobcsW
CEOP

C     Find ctrl-indices
      iobcsN = 0
      iobcsS = 0
      iobcsE = 0
      iobcsW = 0
      DO ivar = 1, maxcvars
       IF ( ncvargrd(ivar) .EQ. 'm' ) THEN
        IF ( ncvarindex(ivar) .EQ. 1 ) iobcsN = ivar
        IF ( ncvarindex(ivar) .EQ. 2 ) iobcsS = ivar
        IF ( ncvarindex(ivar) .EQ. 3 ) iobcsE = ivar
        IF ( ncvarindex(ivar) .EQ. 4 ) iobcsW = ivar
       ENDIF
      ENDDO
#if ( defined ALLOW_OBCSN_CONTROL && defined ALLOW_OBCSN_COST_CONTRIBUTION )
      IF ( iobcsN.GT.0 ) THEN
cgg   North boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsN)
       endrec   = ncvarrecsend(iobcsN)
       call cost_obcsn( startrec, endrec,
     I                  myTime, myIter, myThid )
      ENDIF
#endif
#if ( defined ALLOW_OBCSS_CONTROL && defined ALLOW_OBCSS_COST_CONTRIBUTION )
      IF ( iobcsS.GT.0 ) THEN
cgg   South boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsS)
       endrec   = ncvarrecsend(iobcsS)
       call cost_obcss( startrec, endrec,
     I                  myTime, myIter, myThid )
      ENDIF
#endif
#if ( defined ALLOW_OBCSW_CONTROL && defined ALLOW_OBCSW_COST_CONTRIBUTION )
      IF ( iobcsW.GT.0 ) THEN
cgg   West boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsW)
       endrec   = ncvarrecsend(iobcsW)
       call cost_obcsw( startrec, endrec,
     I                  myTime, myIter, myThid )
      ENDIF
#endif
#if ( defined ALLOW_OBCSE_CONTROL && defined ALLOW_OBCSE_COST_CONTRIBUTION )
      IF ( iobcsE.GT.0 ) THEN
cgg   East boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsE)
       endrec   = ncvarrecsend(iobcsE)
       call cost_obcse( startrec, endrec,
     I                  myTime, myIter, myThid )
      ENDIF
#endif

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
      IF ( ( iobcsN+iobcsS+iobcsE+iobcsW ) .GT.0 ) THEN
       call cost_obcsvol( startrec, endrec,
     I                    myTime, myIter, myThid )
      ENDIF
#endif

#endif /* ALLOW_CTRL and ALLOW_OBCS */

      RETURN
      END
