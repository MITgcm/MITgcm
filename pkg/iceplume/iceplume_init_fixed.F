#include "ICEPLUME_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif /* ALLOW_EXF */

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
#ifdef ALLOW_EXF
#include "EXF_PARAM.h"
#endif /* ALLOW_EXF */

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEPLUME_INIT_FIXED
      INTEGER myThid
      INTEGER myIter
      INTEGER fp, iRec0, iRec1
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     myTime

#ifdef ALLOW_EXF
      INTEGER errCount
#endif /* ALLOW_EXF */

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
          plumeLength  (I,J,bi,bj)  = 0. _d 0

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
ckiki read in plume length(s)
       IF ( plumeLengthFile .NE. ' ' ) THEN
        CALL READ_REC_XY_RS
     &     (plumeLengthFile,plumeLength,iRec0,myIter,myThid)
       ENDIF

C Load runoff

#ifdef ALLOW_EXF
      IF ( useEXF .AND. runoffQsgfile .NE. ' ' ) THEN
       _BEGIN_MASTER( myThid )
       errCount = 0
# ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_CALL('GETFFIELD_START runoffQsg',myThid)
# endif
catn useExfYearlyFields is a twoDigitYear to identify year or undef if have
catn cycle. Note the double "FF" in getffield, which is the newer version
       CALL EXF_GETFFIELD_START( useExfYearlyFields,
     I                    'iceplume', 'runoffQsg', runoffQsgperiod,
     I                    runoffQsgstartdate1, runoffQsgstartdate2,
     U                    runoffQsgStartTime, errCount,
     I                    myThid )
       IF ( errCount.GE.1 ) THEN
        WRITE(msgBuf,'(A,I3,A)')
     &     'ICEPLUME_INIT_FIXED: detected', errCount,' fatal error(s)'
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( 0 )
        STOP 'ABNORMAL END: S/R ICEPLUME_INIT_FIXED'
       ENDIF
       _END_MASTER( myThid )
       _BARRIER
      ENDIF
#else
      IF ( .NOT. periodicExternalForcing ) THEN
C If periodic external forcing is used, this is read in S/R external_fields_load

#ifndef ALLOW_MDSIO
         STOP 'ABNORMAL END: ICEPLUME_INIT_FIXED: NEEDS MSDIO PKG'
#endif /* ALLOW_MDSIO */
        _BARRIER

C      Read constant runoff input only for myIter = nIter0

        WRITE(msgBuf,'(1X,A,I10,1P1E20.12)')
     &       'ICEPLUME_INIT_FIXED: Reading initial data:',
     &       myIter, myTime

        CALL PRINT_MESSAGE(msgBuf,standardMessageUnit,
     &       SQUEEZE_RIGHT,myThid)

        IF ( runoffQsgFile .NE. ' ') THEN
       CALL READ_REC_XY_RL(
     &      runoffQsgFile,runoffQsg,iRec0,myIter,myThid)
        ENDIF

catn        IF ( runoffVelFile .NE. ' ') THEN
catn       CALL READ_REC_XY_RL(
catn     &      runoffVelFile,runoffVel,iRec0,myIter,myThid)
catn        ENDIF
catn
catn        IF ( runoffRadFile .NE. ' ') THEN
catn       CALL READ_REC_XY_RL(
catn     &      runoffRadFile,runoffRad,iRec0,myIter,myThid)
catn        ENDIF

       ENDIF ! not periodicExternalForcing
#endif /* ALLOW_EXF */

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
