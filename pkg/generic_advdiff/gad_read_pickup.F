C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_read_pickup.F,v 1.4 2011/03/14 01:37:04 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GAD_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE GAD_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of 2nd.Order moments from a pickup file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "GAD_SOM_VARS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     !INPUT PARAMETERS:
C     myIter  :: time-step number
C     myThid  :: thread number
      INTEGER myIter
      INTEGER myThid

#ifdef GAD_ALLOW_TS_SOM_ADV

C     !LOCAL VARIABLES:
C     n       :: 2nd.O. moment loop index
C     iRec    :: record number
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
      INTEGER n, prec, iRec
      CHARACTER*(MAX_LEN_FNAM) fn, filNam
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
      LOGICAL useCurrentDir, fileExist
CEOP

C-    Need to synchronize here before doing master-thread IO
C note: not presently needed (synchronized through MDS_CHECK4FILE call)
c     _BARRIER
      ioUnit = errorMessageUnit

#ifdef ALLOW_MNC
      IF ( useMNC .AND. pickup_read_mnc ) THEN
c      IF ( tempSOM_Advection ) THEN
C       Read variables from the pickup file
c       WRITE(fn,'(a)') 'pickup_som'
c       CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
c       CALL MNC_CW_SET_UDIM(fn, 1, myThid)
c       CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
c       prefix = 'somT_'
c       DO n = 1,nSOM
c         CALL MNC_CW_RL_R('D',fn,0,0, som_name,
c    &         som_T(1-OLx,1-OLy,1,1,1,n),myThid)
c       ENDDO
c      ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

c     IF ( pickup_read_mdsio .AND. tempSOM_Advection ) THEN
      IF ( tempSOM_Advection ) THEN
C--   Pot. Temp. 2nd.Order moments

        IF (pickupSuff .EQ. ' ') THEN
          WRITE(fn,'(A,I10.10)') 'pickup_somT.', myIter
        ELSE
          WRITE(fn,'(A,A10)') 'pickup_somT.', pickupSuff
        ENDIF

C-      First check if pickup file exist
#ifdef ALLOW_MDSIO
        useCurrentDir = .FALSE.
        CALL MDS_CHECK4FILE(
     I                       fn, '.data', 'GAD_READ_PICKUP',
     O                       filNam, fileExist,
     I                       useCurrentDir, myThid )
#else
        STOP 'ABNORMAL END: S/R GAD_READ_PICKUP: Needs MDSIO pkg'
#endif

        IF ( fileExist ) THEN
C-      Read 2nd Order moments as consecutive records
          prec = precFloat64
          DO n=1,nSOM
            iRec = n
            CALL READ_REC_3D_RL( fn, prec, Nr,
     O                som_T(1-Olx,1-Oly,1,1,1,n),
     I                iRec, myIter, myThid )
          ENDDO
        ELSE
          IF ( pickupStrictlyMatch ) THEN
            WRITE(msgBuf,'(4A)') 'GAD_READ_PICKUP: ',
     &        'try with " pickupStrictlyMatch=.FALSE.,"',
     &        ' in file: "data", NameList: "PARM03"'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
            STOP 'ABNORMAL END: S/R GAD_READ_PICKUP'
          ELSE
            WRITE(msgBuf,'(2A)') 'WARNING >> GAD_READ_PICKUP: ',
     &        'approximated restart: reset SOM_T to zero'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF

      ENDIF

c     IF ( pickup_read_mdsio .AND. saltSOM_Advection ) THEN
      IF ( saltSOM_Advection ) THEN
C--   Salinity 2nd.Order moments

        IF (pickupSuff .EQ. ' ') THEN
          WRITE(fn,'(A,I10.10)') 'pickup_somS.', myIter
        ELSE
          WRITE(fn,'(A,A10)') 'pickup_somS.', pickupSuff
        ENDIF

C-      First check if pickup file exist
#ifdef ALLOW_MDSIO
        useCurrentDir = .FALSE.
        CALL MDS_CHECK4FILE(
     I                       fn, '.data', 'GAD_READ_PICKUP',
     O                       filNam, fileExist,
     I                       useCurrentDir, myThid )
#else
        STOP 'ABNORMAL END: S/R GAD_READ_PICKUP: Needs MDSIO pkg'
#endif

        IF ( fileExist ) THEN
C-      Read 2nd Order moments as consecutive records
          prec = precFloat64
          DO n=1,nSOM
            iRec = n
            CALL READ_REC_3D_RL( fn, prec, Nr,
     O                som_S(1-Olx,1-Oly,1,1,1,n),
     I                iRec, myIter, myThid )
          ENDDO
        ELSE
          IF ( pickupStrictlyMatch ) THEN
            WRITE(msgBuf,'(4A)') 'GAD_READ_PICKUP: ',
     &        'try with " pickupStrictlyMatch=.FALSE.,"',
     &        ' in file: "data", NameList: "PARM03"'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
            STOP 'ABNORMAL END: S/R GAD_READ_PICKUP'
          ELSE
            WRITE(msgBuf,'(2A)') 'WARNING >> GAD_READ_PICKUP: ',
     &        'approximated restart: reset SOM_S to zero'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
        ENDIF

      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */

      RETURN
      END
