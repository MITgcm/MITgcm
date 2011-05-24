C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check_depths.F,v 1.2 2011/05/24 14:31:14 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CHECK_DEPTHS( myThid )
C     *==========================================================*
C     | SUBROUTINE OBCS_CHECK_DEPTHS                         |
C     | o Check for non-zero normal gradient across open         |
C     |   boundaries                                             |
C     | o fix them if required and print a message               |
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
C     myThid -  Number of this instances
      INTEGER myThid

#ifdef ALLOW_OBCS
C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj, I, J, K, ichanged

      IF ( OBCSfixTopo ) THEN
C--   Modify topography to ensure that outward d(topography)/dn >= 0,
C     topography at open boundary points must be equal or shallower than
C     topography one grid-point inward from open boundary
      ichanged = 0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

        DO K=1,Nr
#ifdef ALLOW_OBCS_NORTH
         DO I=1,sNx
          J=OB_Jn(I,bi,bj)
          IF ( J .NE. 0 .AND.
     &         ( R_low(I,J,bi,bj) .LT. R_low(I,J-1,bi,bj) ) ) THEN
           ichanged = ichanged + 1
           R_low(I,J,bi,bj) = R_low(I,J-1,bi,bj)
           WRITE(msgBuf,'(2A,(1X,4I6))')
     &          'S/R OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj) = ',  I, J, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDDO
#endif
#ifdef ALLOW_OBCS_SOUTH
         DO I=1,sNx
          J=OB_Js(I,bi,bj)
          IF ( J .NE. 0 .AND.
     &         ( R_low(I,J,bi,bj) .LT. R_low(I,J+1,bi,bj) ) ) THEN
           ichanged = ichanged + 1
           R_low(I,J,bi,bj) = R_low(I,J+1,bi,bj)
           WRITE(msgBuf,'(2A,(1X,4I6))')
     &          'S/R OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj) = ',  I, J, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDDO
#endif
#ifdef ALLOW_OBCS_EAST
         DO J=1,sNy
          I = OB_Ie(J,bi,bj)
          IF ( I .NE. 0 .AND.
     &         ( R_low(I,J,bi,bj) .LT. R_low(I-1,J,bi,bj) ) ) THEN
           ichanged = ichanged + 1
           R_low(I,J,bi,bj) = R_low(I-1,J,bi,bj)
           WRITE(msgBuf,'(2A,(1X,4I6))')
     &          'S/R OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj) = ',  I, J, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDDO
#endif
C Western boundary
#ifdef ALLOW_OBCS_WEST
         DO J=1,sNy
          I = OB_Iw(J,bi,bj)
          IF ( I .NE. 0 .AND.
     &         ( R_low(I,J,bi,bj) .LT. R_low(I+1,J,bi,bj) ) ) THEN
           ichanged = ichanged + 1
           R_low(I,J,bi,bj) = R_low(I+1,J,bi,bj)
           WRITE(msgBuf,'(2A,(1X,4I6))')
     &          'S/R OBCS_CHECK_DEPTHS: fixed topography at ',
     &          '(i,j,bi,bj) = ',  I, J, bi, bj
           CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &          SQUEEZE_RIGHT, myThid)
          ENDIF
         ENDDO
#endif
        ENDDO

       ENDDO
      ENDDO
C--   some diagnostics to stdout
      IF ( ichanged .GT. 0 ) THEN
       WRITE(msgBuf,'(A,I7,A,A)')
     &      'OBCS message: corrected ', ichanged,
     &      ' instances of problematic topography gradients',
     &      ' normal to open boundaries'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT, myThid)
      ENDIF
C     endif (OBCSfixTopo)
      ENDIF
#endif /* ALLOW_OBCS */

      RETURN
      END
