C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_read_pickup.F,v 1.2 2008/05/09 21:43:16 jmc Exp $
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
      INTEGER n,prec,iRec
      CHARACTER*(MAX_LEN_MBUF) fn
CEOP

C-    Need to synchronize here before doing master-thread IO
      _BARRIER

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
        prec = precFloat64
C       Read 2nd Order moments as consecutive records
        DO n=1,nSOM
          iRec = n
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O              som_T(1-Olx,1-Oly,1,1,1,n),
     I              iRec, myIter, myThid )
        ENDDO

      ENDIF

c     IF ( pickup_read_mdsio .AND. saltSOM_Advection ) THEN
      IF ( saltSOM_Advection ) THEN
C--   Salinity 2nd.Order moments

        IF (pickupSuff .EQ. ' ') THEN
          WRITE(fn,'(A,I10.10)') 'pickup_somS.', myIter
        ELSE
          WRITE(fn,'(A,A10)') 'pickup_somS.', pickupSuff
        ENDIF
        prec = precFloat64
C       Read 2nd Order moments as consecutive records
        DO n=1,nSOM
          iRec = n
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O         som_S(1-Olx,1-Oly,1,1,1,n),
     I         iRec, myIter, myThid )
        ENDDO

      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */

      RETURN
      END
