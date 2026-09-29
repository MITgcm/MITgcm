#include "BLING_OPTIONS.h"

CBOP
      subroutine BLING_WRITE_PICKUP( permPickup,
     I                             suff, myTime, myIter, myThid )

C     =================================================================
C     | subroutine bling_write_pickup
C     | o Writes BLING arrays (needed for a restart) to a pickup file
C     =================================================================

      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BLING_VARS.h"

C     === Routine arguments ===
C     permPickup :: write a permanent pickup
C     suff       :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime     :: Current time in simulation
C     myIter     :: Current iteration number in simulation
C     myThid     :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_BLING

C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      LOGICAL glf
      _RL     timList(1)
      INTEGER j
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 6 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

        prec = precFloat64

        WRITE(fn,'(A,A)') 'pickup_bling.',suff
        j = 0

C     Firstly, write 3-D fields as consecutive records,

C       record number < 0 : a hack not to write meta files now:
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, pH, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_pH3d'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, irr_mem,
     &                        -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_irrm'

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, chl, -j, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_chl '

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, phyto_sm, -j, myIter,
     & myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_Psm '

        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, phyto_lg, -j, myIter,
     & myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_Plg '

#ifndef USE_BLING_V1
        j = j + 1
        CALL WRITE_REC_3D_RL( fn, prec, Nr, phyto_diaz, -j, myIter,
     & myThid )
        IF (j.LE.listDim) wrFldList(j) = 'BLG_Pdia'
#endif

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'BLING_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'BLING_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R BLING_WRITE_PICKUP (list-size Pb)'
        ENDIF

#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta
C     files
        j  = 1
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, prec, glf, .FALSE.,
     &                         0, 0, Nr, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         j, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* ALLOW_BLING */

      RETURN
      END
