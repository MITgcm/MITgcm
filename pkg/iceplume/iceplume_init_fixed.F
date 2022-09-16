
#include "ICEPLUME_OPTIONS.h"

      SUBROUTINE ICEPLUME_INIT_FIXED( myThid )
C     *============================================================*
C     | SUBROUTINE ICEPLUME_INIT_FIXED
C     | o Routine to initialize ICEPLUME parameters and variables.
C     *============================================================*
C     | Initialize ICEPLUME parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "ICEPLUME.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEPLUME_INIT_FIXED
      INTEGER myThid
      INTEGER myIter
      INTEGER fp, iRec0, iRec1
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     myTime

#ifdef ALLOW_ICEPLUME
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj, iTracer

      iRec0 = 1.

      DO bi = 1,nSx
       DO bj = 1,nSy
        DO I = 1-Olx,sNx+Olx
         DO J = 1-Oly,sNy+Oly
          plumeMask  (I,J,bi,bj)  = 0. _d 0

#ifdef ALLOW_PTRACERS

           DO iTracer = 1,PTRACERS_num
            ptracerMask (I,J,iTracer,bi,bj) = 0. _d 0
           ENDDO

#endif /* ALLOW_PTRACERS */

         ENDDO
        ENDDO
       ENDDO
      ENDDO

C      Read plume mask data
       IF ( plumeMaskFile .NE. ' ' ) THEN
        CALL READ_REC_XY_RS
     &     (plumeMaskFile,plumeMask,iRec0,myIter,myThid)
       ENDIF

C Load runoff

      IF ( .NOT. periodicExternalForcing ) THEN
C If periodic external forcing is used, this is read in S/R external_fields_load

#ifndef ALLOW_MDSIO
         STOP 'ABNORMAL END: OBCS_FIELDS_LOAD: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */
        _BARRIER

C      Read constant runoff input only for myIter = nIter0

        WRITE(msgBuf,'(1X,A,I10,1P1E20.12)')
     &       'ICEPLUME_INIT_FIXED: Reading initial data:',
     &       myIter, myTime

        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid)

        IF ( runoffVelFile .NE. ' ') THEN
       CALL READ_REC_XY_RL(
     &      runoffVelFile,runoffVel,iRec0,myIter,myThid)
        ENDIF

        IF ( runoffRadFile .NE. ' ') THEN
       CALL READ_REC_XY_RL(
     &      runoffRadFile,runoffRad,iRec0,myIter,myThid)
        ENDIF

       ENDIF ! not periodicExternalForcing

#ifdef ALLOW_PTRACERS
C      Read ptracer mask data
       IF ( ptracerMaskFile .NE. ' ' ) THEN
        CALL READ_REC_3D_RS
     &     (ptracerMaskFile,readBinaryPrec,PTRACERS_num,ptracerMask,
     &      iRec0,myIter,myThid)
       ENDIF
#endif /* ALLOW_PTRACERS */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL ICEPLUME_DIAGNOSTICS_INIT(myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_ICEPLUME */


      RETURN
      END
