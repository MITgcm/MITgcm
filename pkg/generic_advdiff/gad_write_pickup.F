C $Header: /u/gcmpack/MITgcm/pkg/generic_advdiff/gad_write_pickup.F,v 1.3 2008/05/09 21:43:16 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"

CBOP
C !ROUTINE: GAD_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE GAD_WRITE_PICKUP( suff,
     I                             myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of 2nd.Order moments to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "GAD_SOM_VARS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: model time
C     myIter  :: time-step number
C     myThid  :: thread number
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef GAD_ALLOW_TS_SOM_ADV
C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C     n       :: 2nd.O. moment loop index
C     iRec    :: record number
C     fn      :: character buffer for creating filename
C     prec    :: precision of pickup files
C     msgBuf  :: Informational/error meesage buffer
      INTEGER n,prec,iRec,lChar
      CHARACTER*(MAX_LEN_FNAM) fn
#ifdef ALLOW_MNC
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif
CEOP

#ifdef ALLOW_MNC
      IF ( useMNC .AND. pickup_write_mnc ) THEN
       IF ( tempSOM_Advection .OR. saltSOM_Advection ) THEN
        WRITE(msgBuf,'(A)') 'GAD_WRITE_PICKUP: MNC not yet coded',
     &                      ' => write bin file instead'
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid)
       ENDIF
      ENDIF
#endif /*  ALLOW_MNC  */

c     IF ( pickup_write_mdsio ) THEN
      IF ( .TRUE. ) THEN
       lChar = ILNBLNK(suff)

       IF ( tempSOM_Advection ) THEN
C--   Pot. Temp. 2nd.Order moments
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_somT'
        ELSE
          WRITE(fn,'(2A)') 'pickup_somT.',suff(1:lChar)
        ENDIF
        prec = precFloat64
C       Write 2nd Order moments as consecutive records
        DO n=1,nSOM
          iRec = n
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     I               som_T(1-Olx,1-Oly,1,1,1,n),
     I               iRec, myIter, myThid )
        ENDDO
       ENDIF

       IF ( saltSOM_Advection ) THEN
C--   Salinity 2nd.Order moments
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_somS'
        ELSE
          WRITE(fn,'(2A)') 'pickup_somS.',suff(1:lChar)
        ENDIF
        prec = precFloat64
C       Write 2nd Order moments as consecutive records
        DO n=1,nSOM
          iRec = n
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     I               som_S(1-Olx,1-Oly,1,1,1,n),
     I               iRec, myIter, myThid )
        ENDDO
       ENDIF

      ENDIF

#endif /* GAD_ALLOW_TS_SOM_ADV */

      RETURN
      END
