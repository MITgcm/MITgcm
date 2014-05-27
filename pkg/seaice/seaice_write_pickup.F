C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_write_pickup.F,v 1.23 2014/05/27 15:24:00 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"

CBOP
C     !ROUTINE: SEAICE_WRITE_PICKUP
C     !INTERFACE:
      SUBROUTINE SEAICE_WRITE_PICKUP ( permPickup, suff,
     I                                 myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_WRITE_PICKUP
C     | o Write sea ice pickup file for restarting.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#include "SEAICE_TRACER.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     permPickup :: write a permanent pickup
C     suff    :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     fp          :: pickup-file precision ( precFloat64 )
C     glf         :: local flag for "globalFiles"
C     fn          :: Temp. for building file name.
C     nWrFlds     :: number of fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     j           :: loop index / field number
C     nj          :: record number
C     msgBuf      :: Informational/error message buffer
      INTEGER fp
      LOGICAL  glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 20 )
      CHARACTER*(8) wrFldList(listDim)
      INTEGER j, nj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_SITRACER
      CHARACTER*(8) fldName
      INTEGER iTrac
#endif
CEOP

C--   Write model fields
      WRITE(fn,'(A,A)') 'pickup_seaice.',suff

c     IF ( seaice_pickup_write_mdsio ) THEN

       fp = precFloat64
       j = 0
       nj = 0
C     record number < 0 : a hack not to write meta files now:

C--   write Sea-Ice Thermodynamics State variables, starting with 3-D fields:
       IF ( .NOT.useThSIce ) THEN

#ifdef SEAICE_ITD

        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, nITD, TICES,   -j, myIter,myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siTICES '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, nITD, AREAITD, -j, myIter,myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siAREAn '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, nITD, HEFFITD, -j, myIter,myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHEFFn '
        j = j + 1
        CALL WRITE_REC_3D_RL( fn,fp, nITD, HSNOWITD,-j, myIter,myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHSNOWn'
C-    switch to 2-D fields:
        nj = -j*nITD

#else /* SEAICE_ITD */

        j = j + 1
        nj = nj-1
        IF (SEAICE_multDim.GT.1) THEN
         CALL WRITE_REC_3D_RL(fn,fp,nITD,TICES, nj, myIter, myThid )
         IF (j.LE.listDim) wrFldList(j) = 'siTICES '
C-    switch to 2-D fields:
c         nj = nj*nITD
         nj = nj-nITD+1
        ELSE
         CALL WRITE_REC_LEV_RL( fn, fp, nITD, 1, 1, TICES,
     I                          nj, myIter, myThid )
         IF (j.LE.listDim) wrFldList(j) = 'siTICE  '
        ENDIF

C---  continue to write 2-D fields:
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, AREA , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siAREA  '
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, HEFF , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHEFF  '
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, HSNOW , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHSNOW '

#endif /* SEAICE_ITD */

#ifdef SEAICE_VARIABLE_SALINITY
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, HSALT , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHSALT '
#endif
#ifdef ALLOW_SITRACER
         DO iTrac = 1, SItrNumInUse
          WRITE(fldName,'(A6,I2.2)') 'siTrac', iTrac
          j = j + 1
          nj = nj-1
          CALL WRITE_REC_3D_RL( fn, fp,  1,
     &         SItracer(1-OLx,1-OLy,1,1,iTrac),
     &         nj, myIter, myThid )
          IF (j.LE.listDim) wrFldList(j) = fldName
         ENDDO
#endif
       ENDIF

C--    write Sea-Ice Dynamics variables (all 2-D fields):
       j = j + 1
       nj = nj-1
       CALL WRITE_REC_3D_RL( fn, fp,  1, UICE , nj, myIter, myThid )
       IF (j.LE.listDim)  wrFldList(j) = 'siUICE  '

       j = j + 1
       nj = nj-1
       CALL WRITE_REC_3D_RL( fn, fp,  1, VICE , nj, myIter, myThid )
       IF (j.LE.listDim)  wrFldList(j) = 'siVICE  '

       IF ( SEAICEuseBDF2 ) THEN
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, uIceNm1 , nj, myIter, myThid )
        IF (j.LE.listDim)  wrFldList(j) = 'siUicNm1'

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, vIceNm1 , nj, myIter, myThid )
        IF (j.LE.listDim)  wrFldList(j) = 'siVicNm1'
       ENDIF
#if (defined(SEAICE_CGRID) && defined(SEAICE_ALLOW_EVP))
       IF ( SEAICEuseEVP ) THEN
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, seaice_sigma1,
     &                                            nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siSigm1 '

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, seaice_sigma2,
     &                                            nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siSigm2 '

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, seaice_sigma12,
     &                                            nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siSigm12'
       ENDIF
#endif /* SEAICE_ALLOW_EVP */

       nWrFlds = j
       IF ( nWrFlds.GT.listDim ) THEN
         WRITE(msgBuf,'(2A,I5,A)') 'WRITE_SEAICE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I5,A)') 'WRITE_SEAICE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R WRITE_SEAICE_PICKUP (list-size Pb)'
       ENDIF

#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta files
       nj = ABS(nj)
       glf  = globalFiles
       timList(1) = myTime
       CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &                         0, 0, 1, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         nj, myIter, myThid )
C
#endif /* ALLOW_MDSIO */
C--------------------------
c     ENDIF

      RETURN
      END
