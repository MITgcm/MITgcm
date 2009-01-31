C $Header: /u/gcmpack/MITgcm/pkg/flt/flt_init_fixed.F,v 1.1 2009/01/31 22:21:31 jmc Exp $
C $Name:  $

#include "FLT_OPTIONS.h"

      SUBROUTINE FLT_INIT_FIXED( myThid  )

C     ==================================================================
C     SUBROUTINE FLT_INIT_FIXED
C     ==================================================================
C
C     o This routine initializes the start/restart positions.
C       It does the following:
C     o First it checks for local files. These are supposed to be restarts
C       from a previous integration. The floats can therefore be read in
C       without any further check (because they exist on the specific tile).
C     o If no local files are available the routine assumes that this
C       is an initialization. In that case it reads a global file
C       (that has the same format as local files) and sorts those floats
C       that exist on the specific tile into the local array.
C     o At the end the float positions are written to the trajectory file
C
C     ==================================================================
C     SUBROUTINE flt_init
C     ==================================================================

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FLT.h"

C     == routine arguments ==

C     myThid - thread number for this instance of the routine.
      INTEGER myThid

C     == local variables ==
      INTEGER bi, bj
      _RL xLo, xHi, yLo, yHi

      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     == end of interface ==

C Set domain dependent constants:
      xLo = -xG( 1,  1,1,1)
      xHi = xG(sNx+1,1,1,1)
      yLo = -yG(1, 1,  1,1)
      yHi = yG(1,sNy+1,1,1)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         xLo = MAX( xLo, -xG(1,1,bi,bj) )
         xHi = MAX( xHi, xG(sNx+1,1,bi,bj) )
         yLo = MAX( yLo, -yG(1,1,bi,bj) )
         yHi = MAX( yHi, yG(1,sNy+1,bi,bj) )
       ENDDO
      ENDDO
      _GLOBAL_MAX_R8( xLo, myThid )
      _GLOBAL_MAX_R8( xHi, myThid )
      _GLOBAL_MAX_R8( yLo, myThid )
      _GLOBAL_MAX_R8( yHi, myThid )

      _BEGIN_MASTER(myThid)
       flt_xLo = -xLo
       flt_xHi =  xHi
       flt_yLo = -yLo
       flt_yHi =  yHi
       WRITE(msgBuf,'(A,2F18.8)')
     &    '  FLT_xLo,xHi=', xLo, xHi
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A,2F18.8)')
     &    '  FLT_yLo,yHi=', yLo, yHi
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
      _END_MASTER(myThid)

      RETURN
      END
