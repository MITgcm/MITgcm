C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check_pickup.F,v 1.14 2014/05/23 20:00:41 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_CHECK_PICKUP
C     !INTERFACE:
      SUBROUTINE SEAICE_CHECK_PICKUP(
     I                        missFldList,
     I                        nMissing, nbFields,
     I                        myIter, myThid )

C     !DESCRIPTION:
C     Check that fields that are needed to restart have been read.
C     In case some fields are missing, stop if pickupStrictlyMatch=T
C     or try, if possible, to restart without the missing field.

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     missFldList :: List of missing fields   (attempted to read but not found)
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     myIter      :: Iteration number
C     myThid      :: my Thread Id. number
      CHARACTER*(8) missFldList(*)
      INTEGER nMissing
      INTEGER nbFields
      INTEGER myIter
      INTEGER myThid
CEOP

C     !FUNCTIONS
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == Local variables ==
C     nj          :: record & field number
C     ioUnit      :: temp for writing msg unit
C     msgBuf      :: Informational/error message buffer
C     i,j,k       :: loop indices
C     bi,bj       :: tile indices
      INTEGER nj, ioUnit
      INTEGER tIceFlag, warnCnts
      LOGICAL stopFlag
c     LOGICAL oldIceAge
#ifdef SEAICE_ITD
C     Flag indicating absence of ITD fields such as AREAITD
C      in this case try to use average fields such as AREA
C      (program will stop if fields liek AREA are missing)
      LOGICAL useAvgFldsForITD
#endif
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(8) fldName
c     INTEGER i,j,k,bi,bj
#ifdef ALLOW_SITRACER
      INTEGER iTracer
      CHARACTER*(2) fldNum
#endif
CEOP

c     IF ( seaice_pickup_read_mdsio ) THEN

       IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        tIceFlag = 0
c       oldIceAge = .TRUE.
        DO nj=1,nMissing
         IF ( missFldList(nj).EQ.'siTICES ' ) tIceFlag = tIceFlag + 2
         IF ( missFldList(nj).EQ.'siTICE  ' ) tIceFlag = tIceFlag + 1
c        IF ( missFldList(nj).EQ.'siAGE   ' ) oldIceAge = .FALSE.
        ENDDO
        stopFlag = .FALSE.
#ifdef SEAICE_ITD
        useAvgFldsForITD = .FALSE.
#endif
        warnCnts = nMissing

        DO nj=1,nMissing
         fldName = missFldList(nj)
         IF     ( fldName.EQ.'siTICE  ' .AND. tIceFlag.LE.1 ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') '** WARNING ** SEAICE_CHECK_PICKUP:',
     &      ' restart with Tice from 1rst category'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
          ENDIF
         ELSEIF ( fldName.EQ.'siTICES ' .AND. tIceFlag.LE.2 ) THEN
          IF ( .NOT.pickupStrictlyMatch .AND. SEAICE_multDim.GT.1 ) THEN
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') '** WARNING ** SEAICE_CHECK_PICKUP:',
     &      ' restart from single category Tice (copied to TICES)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
C     copy TICE -> TICES, already done in s/r seaice_read_pickup
          ENDIF
         ELSEIF ( fldName(1:6).EQ.'siSigm' ) THEN
C- Note: try to restart without Sigma1,2,12 (as if SEAICEuseEVPpickup=F)
C        An alternative would be to restart only if SEAICEuseEVPpickup=F:
C        if SEAICEuseEVPpickup then stop / else warning / endif
          IF ( .NOT.pickupStrictlyMatch ) THEN
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') '** WARNING ** SEAICE_CHECK_PICKUP:',
     &      ' restart without "',fldName,'" (set to zero)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
          ENDIF
         ELSEIF ( fldName(1:8).EQ.'siUicNm1' .OR.
     &            fldName(1:8).EQ.'siVicNm1' ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
C     print a warning and restart anyway
           SEAICEmomStartBDF = 0
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') '** WARNING ** SEAICE_CHECK_PICKUP:',
     &      ' restart without "',fldName,'" (set to zero)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           _END_MASTER( myThid )
          ENDIF
         ELSEIF ( fldName.EQ.'siTICES ' .OR.
     &            fldName.EQ.'siTICE  ' .OR.
     &            fldName.EQ.'siUICE  ' .OR.
     &            fldName.EQ.'siVICE  ' .OR.
     &            fldName.EQ.'siAREA  ' .OR.
     &            fldName.EQ.'siHEFF  ' .OR.
     &            fldName.EQ.'siHSNOW ' .OR.
     &            fldName.EQ.'siHSALT ' ) THEN
           stopFlag = .TRUE.
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &       'cannot restart without field "',fldName,'"'
           CALL PRINT_ERROR( msgBuf, myThid )
           _END_MASTER( myThid )
#ifdef SEAICE_ITD
         ELSEIF ( fldName.EQ.'siAREAn ' .OR.
     &            fldName.EQ.'siHEFFn ' .OR.
     &            fldName.EQ.'siHSNOWn' ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
C          generate ITD from mean ice thickness
           useAvgFldsForITD = .TRUE.
          ELSE
C          if strict match is requested
C          run will bestopped in case of missing ITD fields
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &       'cannot restart without ITD field "',fldName,'"'
           CALL PRINT_ERROR( msgBuf, myThid )
          ENDIF
#endif
#ifdef ALLOW_SITRACER
         ELSEIF ( fldName(1:6).EQ.'siTrac' ) THEN
           IF ( .NOT.pickupStrictlyMatch ) THEN
            _BEGIN_MASTER( myThid )
            DO iTracer = 1, SItrMaxNum
             WRITE(fldNum,'(I2.2)') iTracer
             IF ( fldName(7:8).EQ.fldNum ) THEN
              WRITE(msgBuf,'(4A)')
     &         '** WARNING ** SEAICE_CHECK_PICKUP: ',
     &         'restart without "',fldName,'" (set to zero)'
              CALL PRINT_MESSAGE(
     &         msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
             ENDIF
            ENDDO
            _END_MASTER( myThid )
           ENDIF
#endif /* ALLOW_SITRACER */
         ELSE
C-    not recognized fields:
           stopFlag = .TRUE.
           _BEGIN_MASTER( myThid )
           WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &       'missing field "',fldName,'" not recognized'
           CALL PRINT_ERROR( msgBuf, myThid )
           _END_MASTER( myThid )
         ENDIF
C-    end nj loop
        ENDDO

        IF ( stopFlag ) THEN
         STOP 'ABNORMAL END: S/R SEAICE_CHECK_PICKUP'
        ELSEIF ( pickupStrictlyMatch ) THEN
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
         CALL PRINT_ERROR( msgBuf, myThid )
         _END_MASTER( myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK_PICKUP'
        ELSEIF ( warnCnts .GT. 0 ) THEN
         _BEGIN_MASTER( myThid )
#ifdef SEAICE_ITD
         IF ( useAvgFldsForITD ) THEN
          WRITE(msgBuf,'(3A)') '** WARNING ** SEAICE_CHECK_PICKUP:',
     &     ' no ITD fields available, restart from single category',
     &     ' fields, i.e. AREA -> AREAITD, HEFF -> HEFFITD, etc.'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL SEAICE_ITD_PICKUP( myIter, myThid )
         ENDIF
#endif
         WRITE(msgBuf,'(4A)') '** WARNING ** SEAICE_CHECK_PICKUP: ',
     &     'Will get only an approximated Restart'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )
        ENDIF

       ENDIF

C--   end: seaice_pickup_read_mdsio
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
