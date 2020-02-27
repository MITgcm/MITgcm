#include "SHELFICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE SHELFICE_READ_PICKUP( seqFlag, myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of SHELFICE from a pickup file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SHELFICE.h"

C     !INPUT PARAMETERS:
C     seqFlag :: flag that indicate where this S/R is called from:
C             :: =0 called early on, from: ini_masks_etc.F
C             :: =1 called from INIT_VARIA (i.e. usual place)
C     myIter  :: my time-step number
C     myThid  :: my Thread Id number
      INTEGER seqFlag
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     fn          :: character buffer for creating filename
C     fp          :: precision of pickup files
C     filePrec    :: pickup-file precision (read from meta file)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     missFldList :: List of missing fields   (attempted to read but not found)
C     missFldDim  :: Dimension of missing fields list array: missFldList
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     j           :: loop index
C     nj          :: record number
C     ioUnit      :: temp for writing msg unit
C     msgBuf      :: Informational/error message buffer
      INTEGER fp
      INTEGER filePrec, nbFields
      INTEGER missFldDim, nMissing
      INTEGER j, nj, ioUnit
      PARAMETER( missFldDim = 12 )
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(8) missFldList(missFldDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( pickupSuff.EQ.' ' ) THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_shelfice.', nIter0
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, nIter0, myThid )
          WRITE(fn,'(A,A)') 'pickup_shelfice.', suff
        ENDIF
      ELSE
        WRITE(fn,'(A,A10)')    'pickup_shelfice.', pickupSuff
      ENDIF
      fp = precFloat64

      CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, nIter0, myThid )

      _BEGIN_MASTER( myThid )
      IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'SHELFICE_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'SHELFICE_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R SHELFICE_READ_PICKUP (data-prec Pb)'
      ENDIF
      _END_MASTER( myThid )

      IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'SHELFICE_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'SHELFICE_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R SHELFICE_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> SHELFICE_READ_PICKUP: ',
     &      ' no field-list found'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         IF ( nbFields.EQ.-1 ) THEN
C-      No meta-file
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as currently written'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ELSE
C-      Old meta-file without List of Fields
c         WRITE(msgBuf,'(4A)') 'WARNING >> ',
c    &      ' try to read pickup as it used to be written'
c         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c         WRITE(msgBuf,'(4A)') 'WARNING >> ',
c    &      ' until checkpoint59l (2007 Dec 17)'
c         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(4A)') 'SHELFICE_READ_PICKUP: ',
     &      'no field-list found in meta-file'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R SHELFICE_READ_PICKUP'
         ENDIF
        ENDIF
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( nbFields.EQ.0 ) THEN
C---  Old way to read pickup:

      ELSE
C---  New way to read SHELFICE pickup:
        nj = 0
C-    read SHELFICE 3-D fields for restart
        nj = nj*Nr
C-    read SHELFICE 2-D fields for restart

        IF ( seqFlag.EQ.1 .AND. SHELFICEMassStepping ) THEN
          CALL READ_MFLDS_3D_RL( 'SHI_mass', shelficeMass,
     &                                 nj, fp, 1 , nIter0, myThid )
        ENDIF

#ifdef ALLOW_SHELFICE_REMESHING
        IF ( seqFlag.EQ.0 .AND. SHELFICEremeshFrequency.GT.zeroRL ) THEN
          CALL READ_MFLDS_LEV_RS( 'R_Shelfi', R_shelfIce,
     &                            nj, fp, 1, 1, 1, nIter0, myThid )
        ENDIF
#endif /* ALLOW_SHELFICE_REMESHING */

C--   end: new way to read pickup file
      ENDIF

C--   Check for missing fields:
      nMissing = missFldDim
      CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     nIter0, myThid )
      IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'SHELFICE_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R SHELFICE_READ_PICKUP (list-size Pb)'
      ENDIF
      IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        DO j=1,nMissing
         WRITE(msgBuf,'(4A)') 'SHELFICE_READ_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDDO
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R SHELFICE_READ_PICKUP'
      ENDIF

C--   Update overlap regions:
      IF ( seqFlag.EQ.1 .AND. SHELFICEMassStepping ) THEN
        CALL EXCH_XY_RL( shelficeMass, myThid )
      ENDIF
#ifdef ALLOW_SHELFICE_REMESHING
      IF ( seqFlag.EQ.0 .AND. SHELFICEremeshFrequency.GT.zeroRL ) THEN
        CALL EXCH_XY_RS( R_shelfIce, myThid )
      ENDIF
#endif

#endif /* ALLOW_SHELFICE */

      RETURN
      END
