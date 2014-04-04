C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_check.F,v 1.5 2014/04/04 21:39:56 jmc Exp $
C $Name:  $

#include "GRDCHK_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE GRDCHK_CHECK( myThid )

C     ==================================================================
C     SUBROUTINE grdchk_check
C     ==================================================================
C
C     o check some gradient check parameter settings for consistency
C
C     ==================================================================
C     SUBROUTINE grdchk_check
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ctrl.h"
#include "grdchk.h"

C     == routine arguments ==

      INTEGER myThid

#ifdef ALLOW_GRDCHK
C     == local variables ==

C     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

      WRITE(msgBuf,'(A)') 'GRDCHK_CHECK: grdchk package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT, myThid )

      IF ( ncvarindex(grdchkvarindex) .LE. 0 ) THEN
         WRITE(msgBuf,'(A,I4)')
     &        'Control variable not enabled for grdchkvarindex = ',
     &        grdchkvarindex
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R GRDCHK_CHECK'
      ENDIF

#endif /* ALLOW_GRDCHK */

      RETURN
      END
