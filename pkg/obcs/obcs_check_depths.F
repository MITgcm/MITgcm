C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check_depths.F,v 1.5 2012/09/18 19:45:21 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CHECK_DEPTHS( myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_CHECK_DEPTHS
C     | o Check for non-zero normal gradient across open
C     |   boundaries
C     | o fix them if required and print a message
C     *==========================================================*
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"

C     === Routine arguments ===
C     myThid    :: my Thread Id number
      INTEGER myThid

#ifdef ALLOW_OBCS
C     === Local variables ===
C     msgBuf    :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, i, j, ichanged

      IF ( OBCSfixTopo ) THEN
C--   Modify topography to ensure that outward d(topography)/dn >= 0,
C     topography at open boundary points must be equal or shallower than
C     topography one grid-point inward from open boundary
      ichanged = 0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_OBCS_NORTH
        DO i=1,sNx
         j = OB_Jn(i,bi,bj)
         IF ( j.NE.OB_indexNone ) THEN
          IF ( R_low(i,j,bi,bj) .LT. R_low(i,j-1,bi,bj) ) THEN
           ichanged = ichanged + 1
           R_low(i,j,bi,bj) = R_low(i,j-1,bi,bj)
           WRITE(msgBuf,'(2A,2I6,2I4)')
     &          'OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj)=',  i, j, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDIF
        ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
        DO i=1,sNx
         j = OB_Js(i,bi,bj)
         IF ( j.NE.OB_indexNone ) THEN
          IF ( R_low(i,j,bi,bj) .LT. R_low(i,j+1,bi,bj) ) THEN
           ichanged = ichanged + 1
           R_low(i,j,bi,bj) = R_low(i,j+1,bi,bj)
           WRITE(msgBuf,'(2A,2I6,2I4)')
     &          'OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj)=',  i, j, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDIF
        ENDDO
#endif
#ifdef ALLOW_OBCS_EAST
        DO j=1,sNy
         i = OB_Ie(j,bi,bj)
         IF ( i.NE.OB_indexNone ) THEN
          IF ( R_low(i,j,bi,bj) .LT. R_low(i-1,j,bi,bj) ) THEN
           ichanged = ichanged + 1
           R_low(i,j,bi,bj) = R_low(i-1,j,bi,bj)
           WRITE(msgBuf,'(2A,2I6,2I4)')
     &          'OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj)=',  i, j, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDIF
        ENDDO
#endif
C Western boundary
#ifdef ALLOW_OBCS_WEST
        DO j=1,sNy
         i = OB_Iw(j,bi,bj)
         IF ( i.NE.OB_indexNone ) THEN
          IF ( R_low(i,j,bi,bj) .LT. R_low(i+1,j,bi,bj) ) THEN
           ichanged = ichanged + 1
           R_low(i,j,bi,bj) = R_low(i+1,j,bi,bj)
           WRITE(msgBuf,'(2A,2I6,2I4)')
     &          'OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj)=',  i, j, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDIF
        ENDDO
#endif

       ENDDO
      ENDDO
C--   some diagnostics to stdout
      CALL GLOBAL_SUM_INT( ichanged, myThid )
      IF ( ichanged .GT. 0 ) THEN
       _BEGIN_MASTER(myThid)
       WRITE(msgBuf,'(2A,I7,A)') 'OBCS_CHECK_DEPTHS: ',
     &   'Topography gradients normal to open boundaries:'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       WRITE(msgBuf,'(2A,I7,A)') 'OBCS_CHECK_DEPTHS: ',
     &   '==> corrected ', ichanged,' problematic grid-points'
c      WRITE(msgBuf,'(A,I7,A,A)')
c    &      'OBCS message: corrected ', ichanged,
c    &      ' instances of problematic topography gradients',
c    &      ' normal to open boundaries'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER(myThid)
      ENDIF

C     endif (OBCSfixTopo)
      ENDIF
#endif /* ALLOW_OBCS */

      RETURN
      END
