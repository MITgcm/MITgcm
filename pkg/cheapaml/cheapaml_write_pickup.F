C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml_write_pickup.F,v 1.10 2013/01/13 22:46:38 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"

CBOP
C !ROUTINE: CHEAPAML_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE CHEAPAML_WRITE_PICKUP( permPickup,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of cheapaml variables  to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "CHEAPAML.h"

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

#ifdef ALLOW_CHEAPAML

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C     j           :: loop index / field number
C     nj          :: record number
C     fp          :: pickup-file precision
C     glf         :: local flag for "globalFiles"
C     fn          :: character buffer for creating filename
C     nWrFlds     :: number of fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     msgBuf      :: Informational/error message buffer
      INTEGER j, nj, fp, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 12 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

        lChar = ILNBLNK(suff)
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_cheapaml'
        ELSE
          WRITE(fn,'(2A)') 'pickup_cheapaml.',suff(1:lChar)
        ENDIF
        fp = precFloat64
        j  = 0

C       Firstly, write 3-D fields as consecutive records,

C     record number < 0 : a hack not to write meta files now:

C-    switch to 2-D fields:
        nj = -j*Nr

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        Tair, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Tair    '
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        gTairm, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'gTairNm1'

       IF (useFreshWaterFlux) THEN
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        qair, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Qair    '
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        gqairm, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'gQairNm1'
       ENDIF

       IF (useCheapTracer) THEN
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        Cheaptracer, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'cTracer '
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        gCheaptracerm, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'gTracNm1'
       ENDIF

C--------------------------
       nWrFlds = j
       IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'CHEAPAML_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'CHEAPAML_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R CHEAPAML_WRITE_PICKUP (list-size Pb)'
       ENDIF
#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta files
        j  = 1
        nj = ABS(nj)
        IF ( nWrFlds*Nr .EQ. nj ) THEN
          j  = Nr
          nj = nWrFlds
        ENDIF
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &                         0, 0, j, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* ALLOW_CHEAPAML */

      RETURN
      END
