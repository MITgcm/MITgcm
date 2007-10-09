C $Header: /u/gcmpack/MITgcm/pkg/grdchk/grdchk_check.F,v 1.2 2007/10/09 00:05:45 jmc Exp $
C $Name:  $

#include "CTRL_CPPOPTIONS.h"


      subroutine grdchk_check( mythid )

c     ==================================================================
c     SUBROUTINE grdchk_check
c     ==================================================================
c
c     o check some gradient check parameter settings for consistency
c
c     ==================================================================
c     SUBROUTINE grdchk_check
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "ctrl.h"
#include "grdchk.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_GRDCHK
c     == local variables ==

C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     == end of interface ==

      WRITE(msgBuf,'(A)') 'CTRL_CHECK: ctrl package'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &     SQUEEZE_RIGHT , 1)

      if ( ncvarindex(grdchkvarindex) .LE. 0 ) then
         WRITE(msgBuf,'(A,I4)')
     &        'Control variable not enabled for grdchkvarindex = ',
     &        grdchkvarindex
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R GRDCHK_CHECK'
      endif

#endif /* ALLOW_GRDCHK */

      end

