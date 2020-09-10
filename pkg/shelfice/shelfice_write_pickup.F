#include "SHELFICE_OPTIONS.h"

CBOP
C !ROUTINE: SHELFICE_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_WRITE_PICKUP( permPickup,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"

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

#ifdef ALLOW_SHELFICE
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

      IF ( SHELFICEMassStepping .OR.
     &     SHELFICEremeshFrequency.GT.zeroRL ) THEN
        lChar = ILNBLNK(suff)
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_shelfice'
        ELSE
          WRITE(fn,'(2A)') 'pickup_shelfice.',suff(1:lChar)
        ENDIF
        fp = precFloat64
        j  = 0

C       Firstly, write 3-D fields as consecutive records,

C-    switch to 2-D fields:
        nj = -j*Nr

      IF ( SHELFICEMassStepping ) THEN

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, fp, 1, ShelficeMass,
     &                        nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'SHI_mass'

      ENDIF ! SHELFICEMassStepping

#ifdef ALLOW_SHELFICE_REMESHING
      IF ( SHELFICEremeshFrequency.GT.zeroRL ) THEN

        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RS( fn, fp, 1, R_shelfIce,
     &                        nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'R_Shelfi'

      ENDIF ! shelfice-remeshing
#endif /* ALLOW_SHELFICE_REMESHING */

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'SHELFICE_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'SHELFICE_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R SHELFICE_WRITE_PICKUP (list-size Pb)'
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
      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END
