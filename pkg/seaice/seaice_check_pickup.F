C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_check_pickup.F,v 1.1 2007/11/25 21:39:32 jmc Exp $
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
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

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
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef SEAICE_MULTICATEGORY
      INTEGER i,j,k,bi,bj
#endif
CEOP

c     IF ( seaice_pickup_read_mdsio ) THEN

       IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        tIceFlag = 0
        DO nj=1,nMissing
         IF ( missFldList(nj).EQ.'siTICES ' ) tIceFlag = tIceFlag + 2
         IF ( missFldList(nj).EQ.'siTICE  ' ) tIceFlag = tIceFlag + 1
        ENDDO
        stopFlag = .FALSE.
        warnCnts = nMissing
        DO nj=1,nMissing
         IF     ( missFldList(nj).EQ.'siTICE  '
     &                      .AND. tIceFlag.LE.1 ) THEN
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNINGS ** SEAICE_CHECK_PICKUP: ',
     &      'restart with Tice from 1rst category'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(nj).EQ.'siTICES '
     &                      .AND. tIceFlag.LE.2 ) THEN
#ifdef SEAICE_MULTICATEGORY
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNINGS ** SEAICE_CHECK_PICKUP: ',
     &      'restart from single category Tice (copied to TICES)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
C      copy TICE -> TICES
           DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
             DO k=1,MULTDIM
              DO j=1-OLy,sNy+OLy
               DO i=1-OLx,sNx+OLx
                TICES(i,j,k,bi,bj) = TICE(i,j,bi,bj)
               ENDDO
              ENDDO
             ENDDO
            ENDDO
           ENDDO
          ENDIF
#endif
         ELSEIF ( missFldList(nj).EQ.'siSigm1 ' .OR.
     &            missFldList(nj).EQ.'siSigm2 ' .OR.
     &            missFldList(nj).EQ.'siSigm12' ) THEN
C- Note: try to restart without Sigma1,2,12 (as if SEAICEuseEVPpickup=F)
C        An alternative would be to restart only if SEAICEuseEVPpickup=F:
C        if SEAICEuseEVPpickup then stop / else warning / endif
          IF ( .NOT.pickupStrictlyMatch ) THEN
           WRITE(msgBuf,'(4A)') '** WARNINGS ** SEAICE_CHECK_PICKUP: ',
     &      'restart without "',missFldList(nj),'" (set to zero)'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ELSEIF ( missFldList(nj).EQ.'siTICES ' .OR.
     &            missFldList(nj).EQ.'siTICE  ' .OR.
     &            missFldList(nj).EQ.'siUICE  ' .OR.
     &            missFldList(nj).EQ.'siVICE  ' .OR.
     &            missFldList(nj).EQ.'siAREA  ' .OR.
     &            missFldList(nj).EQ.'siHEFF  ' .OR.
     &            missFldList(nj).EQ.'siHSNOW ' .OR.
     &            missFldList(nj).EQ.'siHSALT ' ) THEN
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
           CALL PRINT_ERROR( msgBuf, myThid )
         ELSE
C-    not recognized fields:
           stopFlag = .TRUE.
           WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &       'missing field "',missFldList(nj),'" not recognized'
           CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
C-    end nj loop
        ENDDO

        IF ( stopFlag ) THEN
         STOP 'ABNORMAL END: S/R SEAICE_CHECK_PICKUP'
        ELSEIF ( pickupStrictlyMatch ) THEN
         WRITE(msgBuf,'(4A)') 'SEAICE_CHECK_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R SEAICE_CHECK_PICKUP'
        ELSEIF ( warnCnts .GT. 0 ) THEN
         WRITE(msgBuf,'(4A)') '** WARNINGS ** SEAICE_CHECK_PICKUP: ',
     &     'Will get only an approximated Restart'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
        ENDIF

       ENDIF

C--   end: seaice_pickup_read_mdsio
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      RETURN
      END
