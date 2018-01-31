#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: CPL_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE CPL_WRITE_PICKUP(
     I                             suff, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CPL_WRITE_PICKUP
C     | o Store coupling state for restart.
C     | - Atmospheric version -
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef COMPONENT_MODULE
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
      PARAMETER( listDim = 18 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      lChar = ILNBLNK(suff)
      IF ( lChar.EQ.0 ) THEN
        WRITE(fn,'(2A)') 'pickup_cpl'
      ELSE
        WRITE(fn,'(2A)') 'pickup_cpl.',suff(1:lChar)
      ENDIF
      fp = precFloat64
      j  = 0

C-    Firstly, write 3-D fields as consecutive records
C-    Then switch to 2-D fields:
c       nj = -j*Nr
C     record number < 0 : a hack not to write meta files now:
c       nj = nj-1
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        HeatFlux  , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'qHeatFlx'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        qShortWave, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'qShortW '

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        tauX      , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'surfTauX'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        tauY      , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'surfTauY'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        EvMPrFlux , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'Evp-Prec'

#ifdef ALLOW_LAND
      IF ( atm_cplExch_RunOff ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        RunOffFlux, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'RunOffFx'
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        RunOffEnFx, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'RnOfEnFx'
      ENDIF
#endif /* ALLOW_LAND */
#ifdef ALLOW_THSICE
      IF ( atm_cplExch1W_sIce ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        iceSaltFlx, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'saltFlux'
      ENDIF
      IF ( atm_cplExch_SaltPl ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        saltPlmFlx_cpl, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'sltPlmFx'
      ENDIF
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AIM
      IF ( atm_cplExch_DIC ) THEN
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        airCO2    , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'atm-CO2 '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, fp, 1,
     &                        sWSpeed   , -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'wndSpeed'
      ENDIF
#endif /* ALLOW_AIM */
C-    with only 2-D fields:
        nj = -j

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'CPL_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'CPL_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R CPL_WRITE_PICKUP (list-size Pb)'
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

#endif /* COMPONENT_MODULE */

      RETURN
      END
