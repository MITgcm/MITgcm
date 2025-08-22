#include "STREAMICE_OPTIONS.h"

CBOP
C !ROUTINE: STREAMICE_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE STREAMICE_WRITE_PICKUP( permPickup,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

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

#ifdef ALLOW_STREAMICE
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
          WRITE(fn,'(2A)') 'pickup_streamice'
        ELSE
          WRITE(fn,'(2A)') 'pickup_streamice.',suff(1:lChar)
        ENDIF
        fp = precFloat64
        j  = 0

C       Firstly, write 3-D fields as consecutive records,

#ifdef STREAMICE_HYBRID_STRESS
C     record number < 0 : a hack not to write meta files now:
        j = j + 1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, Nr, visc_streamice_full,
     &                        -j, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, Nr, visc_streamice_full%v,
     &                        -j, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'visc3d  '
#endif /* STREAMICE_HYBRID_STRESS */

C-    switch to 2-D fields:
        nj = -j*Nr

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, area_shelf_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, area_shelf_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_area '

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RS( fn, fp, 1, STREAMICE_hmask,
     &                        nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'SI_hmask'

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, U_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, U_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_uvel '

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, V_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, V_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_vvel '

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, H_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, H_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_thick'

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, tau_beta_eff_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, tau_beta_eff_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_betaF'

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, visc_streamice,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, visc_streamice%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_visc '

#ifdef STREAMICE_HYBRID_STRESS
        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, streamice_taubx,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, streamice_taubx%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_taubx'

        j = j + 1
        nj = nj-1
#ifndef ALLOW_OPENAD
        CALL WRITE_REC_3D_RL( fn, fp, 1, streamice_tauby,
     &                        nj, myIter, myThid )
#else
        CALL WRITE_REC_3D_RL( fn, fp, 1, streamice_tauby%v,
     &                        nj, myIter, myThid )
#endif
        IF (j.LE.listDim) wrFldList(j) = 'SI_tauby'
#endif

c       j = j + 1
c       nj = nj-1
c       CALL WRITE_REC_3D_RL( fn, fp, 1, myPa_Surf2,
c    &                        nj, myIter, myThid )
c       IF (j.LE.listDim) wrFldList(j) = 'myPaSur2'

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'STREAMICE_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'STREAMICE_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R STREAMICE_WRITE_PICKUP (list-size Pb)'
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

#endif /* ALLOW_STREAMICE */

      RETURN
      END
