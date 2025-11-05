#include "OBCS_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C !ROUTINE: OBCS_COST_DRIVER

C !INTERFACE: ==========================================================
      SUBROUTINE OBCS_COST_DRIVER(
     I                             myTime, myIter, myThid )

C !DESCRIPTION:
C     - Calculate cost function contributions (Tikhonov regularisation)
C       for obcs
C     - modified from pkg/ecco/cost_obcs.F

C !USES: ===============================================================
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
#endif
#ifdef ALLOW_OBCS_CONTROL
# include "CTRL_OBCS.h"
#endif

C !INPUT PARAMETERS: ===================================================
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_CTRL
C !LOCAL VARIABLES: ====================================================
      INTEGER startrec
      INTEGER endrec
      INTEGER ivar
      INTEGER iobcsN, iobcsS, iobcsE, iobcsW
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
#ifdef ALLOW_OBCSN_CONTROL
      IF ( iobcsN.GT.0 ) THEN
cgg   North boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsN)
       endrec   = ncvarrecsend(iobcsN)
       CALL OBCS_COST_OBCSN( startrec, endrec,
     I                       myTime, myIter, myThid )
      ENDIF
#endif
#ifdef ALLOW_OBCSS_CONTROL
      IF ( iobcsS.GT.0 ) THEN
cgg   South boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsS)
       endrec   = ncvarrecsend(iobcsS)
       CALL OBCS_COST_OBCSS( startrec, endrec,
     I                       myTime, myIter, myThid )
      ENDIF
#endif
#ifdef ALLOW_OBCSW_CONTROL
      IF ( iobcsW.GT.0 ) THEN
cgg   West boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsW)
       endrec   = ncvarrecsend(iobcsW)
       CALL OBCS_COST_OBCSW( startrec, endrec,
     I                       myTime, myIter, myThid )
      ENDIF
#endif
#ifdef ALLOW_OBCSE_CONTROL
      IF ( iobcsE.GT.0 ) THEN
cgg   East boundary contribution to cost function.
       startrec = ncvarrecstart(iobcsE)
       endrec   = ncvarrecsend(iobcsE)
       CALL OBCS_COST_OBCSE( startrec, endrec,
     I                       myTime, myIter, myThid )
      ENDIF
#endif

#ifdef OBCS_VOLFLUX_COST_CONTRIBUTION
      STOP 'CPP-flag OBCS_VOLFLUX_COST_CONTRIBUTION defined: '//
     &     'S/R COST_OBCSVOL needs to be fixed!'
c      IF ( ( iobcsN+iobcsS+iobcsE+iobcsW ) .GT.0 ) THEN
c       CALL COST_OBCSVOL( startrec, endrec,
c     I                    myTime, myIter, myThid )
c      ENDIF
#endif

#endif /* ALLOW_CTRL */

      RETURN
      END
