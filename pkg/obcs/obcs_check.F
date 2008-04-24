C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_check.F,v 1.12 2008/04/24 08:22:06 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

      SUBROUTINE OBCS_CHECK( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CHECK                                    |
C     | o Check dependences with other packages                  |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "OBCS.h"

C     === Routine arguments ===
C     myThid -  Number of this instances
      INTEGER myThid

#ifdef ALLOW_OBCS

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i,j,bi,bj

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_OBCS'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#ifdef ALLOW_CD_CODE
      IF ( useCDscheme ) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useCDscheme = .TRUE.'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: The CD-scheme does not work with OBCS.'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: Sorry, not yet implemented.'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif /* ALLOW_CD_CODE */

#ifdef ALLOW_ORLANSKI
      WRITE(msgBuf,'(A)') 'OBCS_CHECK: #define ALLOW_ORLANSKI'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)
#else
      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: #undef OBCS_RADIATE_ORLANSKI  and'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: one of useOrlanski* logicals is true'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF 
#endif /* ALLOW_ORLANSKI */

      IF (useOrlanskiNorth.OR.useOrlanskiSouth.OR.
     &    useOrlanskiEast.OR.useOrlanskiWest) THEN
       IF (nonlinFreeSurf.GT.0) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: nonlinFreeSurf not yet implemented'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (usePTracers) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: pTracers not yet implemented'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
       IF (useSEAICE) THEN
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOrlanski* Rad OBC with'
         CALL PRINT_ERROR( msgBuf , 1)
        WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: SEAICE not yet implemented'
         CALL PRINT_ERROR( msgBuf , 1)
         STOP 'ABNORMAL END: S/R OBCS_CHECK'
       ENDIF
      ENDIF

#ifndef ALLOW_OBCS_PRESCRIBE
      IF (useOBCSprescribe) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: useOBCSprescribe = .TRUE. for'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: undef ALLOW_OBCS_PRESCRIBE'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF
#endif

      IF (useSEAICE .AND. .NOT. useEXF) THEN
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: ERROR: for SEAICE OBCS, use'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'OBCS_CHECK: pkg/exf to read input files'
       CALL PRINT_ERROR( msgBuf , 1)
       STOP 'ABNORMAL END: S/R OBCS_CHECK'
      ENDIF

      IF ( debugLevel.GE.debLevB ) THEN
       _BEGIN_MASTER( myThid )
       DO bj = 1,nSy
        DO bi = 1,nSx
         write(*,*) 'bi,bj:',bi,bj,' OB_Jn=',(OB_Jn(i,bi,bj),i=1,sNx)
         write(*,*) 'bi,bj:',bi,bj,' OB_Js=',(OB_Js(i,bi,bj),i=1,sNx)
         write(*,*) 'bi,bj:',bi,bj,' OB_Ie=',(OB_Ie(j,bi,bj),j=1,sNy)
         write(*,*) 'bi,bj:',bi,bj,' OB_Iw=',(OB_Iw(j,bi,bj),j=1,sNy)
        ENDDO
       ENDDO
       _END_MASTER(myThid)
      ENDIF

      WRITE(msgBuf,'(A)') 'OBCS_CHECK: OK'
      CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &                   SQUEEZE_RIGHT,myThid)

#endif /* ALLOW_OBCS */
      RETURN
      END

      SUBROUTINE OBCS_CHECK_TOPOGRAPHY( myThid )
C     /==========================================================\
C     | SUBROUTINE OBCS_CHECK_TOPOGRAPHY                         |
C     | o Check for non-zero normal gradient across open         |
C     |   boundaries                                             |
C     | o fix them if required and print a message               |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS.h"

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
     &          'S/R OBCS_CHECK_TOPOGRAPHY: fixed topography at ',
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
     &          'S/R OBCS_CHECK_TOPOGRAPHY: fixed topography at ',
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
     &          'S/R OBCS_CHECK_TOPOGRAPHY: fixed topography at ',
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
     &          'S/R OBCS_CHECK_TOPOGRAPHY: fixed topography at ',
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
