#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_WRITE_PICKUP( permPickup,
     I                             suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes DIC arrays (needed for a restart) to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"
#include "DIC_ATMOS.h"

C !INPUT PARAMETERS: ===================================================
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

#ifdef ALLOW_DIC

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
      INTEGER ioUnit
      _RL tmpFld(2)
      _RS dummyRS(1)
#ifdef DIC_BIOTIC
      LOGICAL glf
      _RL     timList(1)
      INTEGER j, nj
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 2 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

c     IF ( DIC_pickup_write_mdsio ) THEN
        prec = precFloat64

        IF ( dic_int1.EQ.3 ) THEN
          WRITE(fn,'(A,A)') 'pickup_dic_co2atm.',suff
          ioUnit = 0
#ifdef ALLOW_OPENAD
          tmpFld(1) = total_atmos_carbon%v
          tmpFld(2) = atpco2%v
#else /* ALLOW_OPENAD */
          tmpFld(1) = total_atmos_carbon
          tmpFld(2) = atpco2
#endif /* ALLOW_OPENAD */
#ifdef ALLOW_MDSIO
          CALL MDS_WRITEVEC_LOC(
     I                         fn, prec, ioUnit,
     I                         'RL', 2, tmpFld, dummyRS,
     I                         0, 0, 1, myIter, myThid )
#endif
        ENDIF

#ifdef DIC_BIOTIC
        WRITE(fn,'(A,A)') 'pickup_dic.',suff
        j = 0

C--   write DIC 3-D fields for restart
#ifdef DIC_CALCITE_SAT
        IF ( useCalciteSaturation ) THEN
         j = j + 1
C       record number < 0 : a hack not to write meta files now:
         CALL WRITE_REC_3D_RL( fn, prec, Nr, pH3D, -j, myIter, myThid )
         IF (j.LE.listDim) wrFldList(j) = 'DIC_pH3d'
        ENDIF
#endif

C-    switch to 2-D fields:
        nj = -j*Nr

C--   write DIC 2-D fields for restart
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, prec, 1, pH, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'DIC_pH2d'

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'DIC_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'DIC_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R DIC_WRITE_PICKUP (list-size Pb)'
        ENDIF

#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta
C     files
        j  = 1
        nj = ABS(nj)
        IF ( nWrFlds*Nr .EQ. nj ) THEN
          j  = Nr
          nj = nWrFlds
        ENDIF
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, prec, glf, .FALSE.,
     &                         0, 0, j, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* DIC_BIOTIC  */

c     ENDIF /* DIC_pickup_write_mdsio */

#endif /* ALLOW_DIC  */

      RETURN
      END
