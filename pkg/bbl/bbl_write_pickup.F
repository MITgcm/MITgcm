C $Header: /u/gcmpack/MITgcm/pkg/bbl/bbl_write_pickup.F,v 1.3 2013/01/13 22:46:37 jmc Exp $
C $Name:  $

#include "BBL_OPTIONS.h"

CBOP
C !ROUTINE: BBL_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE BBL_WRITE_PICKUP( permPickup,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BBL.h"

C !INPUT PARAMETERS: ===================================================
C     permPickup      :: write a permanent pickup
C     suff            :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime          :: model time
C     myIter          :: time-step number
C     myThid          :: thread number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_BBL

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C     j           :: loop index / field number
C     fp          :: pickup-file precision
C     glf         :: local flag for "globalFiles"
C     fn          :: character buffer for creating filename
C     nWrFlds     :: number of fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     msgBuf      :: Informational/error message buffer
      INTEGER j, fp, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 3 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

        lChar = ILNBLNK(suff)
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_bbl'
        ELSE
          WRITE(fn,'(2A)') 'pickup_bbl.',suff(1:lChar)
        ENDIF
        fp = precFloat64
        j  = 0

C     Write 2-D fields
C     record number < 0 : a hack not to write meta files now:

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        bbl_theta, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'bblTheta'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        bbl_salt, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'bblSalt '

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        bbl_eta, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'bblEta  '

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'BBL_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'BBL_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R BBL_WRITE_PICKUP (list-size Pb)'
        ENDIF
#ifdef ALLOW_MDSIO
C     uses this specific S/R to write meta file
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &                         0, 0, 1, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         j, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* ALLOW_BBL */

      RETURN
      END
