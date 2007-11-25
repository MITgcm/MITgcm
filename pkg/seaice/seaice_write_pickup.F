C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_write_pickup.F,v 1.1 2007/11/25 21:39:32 jmc Exp $
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
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

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
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 20 )
      CHARACTER*(8) wrFldList(listDim)
      INTEGER j, nj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
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
#ifdef SEAICE_MULTICATEGORY
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL(fn,fp,MULTDIM,TICES, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siTICES '
C-    switch to 2-D fields:
        nj = nj*MULTDIM
#else
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, TICE  , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siTICE  '
#endif /* SEAICE_MULTICATEGORY */

C---  continue to write 2-D fields:
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_LEV_RL(fn, fp, 3,1,1,AREA, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siAREA  '

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_LEV_RL(fn, fp, 3,1,1,HEFF, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHEFF  '

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, HSNOW , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHSNOW '
#ifdef SEAICE_SALINITY
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp,  1, HSALT , nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'siHSALT '
#endif
       ENDIF

C--    write Sea-Ice Dynamics variables (all 2-D fields):
       j = j + 1
       nj = nj-1
       CALL WRITE_REC_LEV_RL( fn, fp, 3,1,1,UICE, nj, myIter, myThid )
       IF (j.LE.listDim)  wrFldList(j) = 'siUICE  '

       j = j + 1
       nj = nj-1
       CALL WRITE_REC_LEV_RL( fn, fp, 3,1,1,VICE, nj, myIter, myThid )
       IF (j.LE.listDim)  wrFldList(j) = 'siVICE  '

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
       CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &                         0, 0, 1, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, myTime,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------
c     ENDIF

      RETURN
      END
