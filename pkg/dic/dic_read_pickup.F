#include "DIC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: DIC_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE DIC_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of DIC from a pickup file

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"

C     !INPUT PARAMETERS:
C     myIter      :: time-step number in simulation
C     myThid      :: my Thread Id number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_DIC
#ifdef DIC_BIOTIC
C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

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
      CHARACTER*(MAX_LEN_FNAM) tmpNam
C- note: to avoid beeing caught by tools/OAD_support/stop2print.sed,
C        change "stopFlag" to "StopFlag"
      LOGICAL useCurrentDir, fileExist, StopFlag
      INTEGER iL
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( pickupSuff.EQ.' ' ) THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_dic.', myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
          WRITE(fn,'(A,A)') 'pickup_dic.', suff
        ENDIF
       ELSE
        WRITE(fn,'(A,A10)') 'pickup_dic.', pickupSuff
       ENDIF
       fp = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )
       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.fp ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'DIC_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'DIC_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R DIC_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       ioUnit = errorMessageUnit
       StopFlag = .FALSE.
       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        IF ( pickupStrictlyMatch ) THEN
         WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         StopFlag = .TRUE.
        ELSE
         WRITE(msgBuf,'(4A)') 'WARNING >> DIC_READ_PICKUP: ',
     &      ' no field-list found'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         IF ( nbFields.EQ.-1 ) THEN
C-      No meta-file: then check if binary pickup file (i.e., ".data") exist
#ifdef ALLOW_MDSIO
          useCurrentDir = .FALSE.
          CALL MDS_CHECK4FILE(
     I                     fn, '.data', 'DIC_READ_PICKUP',
     O                     tmpNam, fileExist,
     I                     useCurrentDir, myThid )
#else
          STOP 'ABNORMAL END: S/R DIC_READ_PICKUP: Needs MDSIO pkg'
#endif
          IF ( fileExist ) THEN
           WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as currently written'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ELSE
           iL = ILNBLNK(fn)
           WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'missing both "meta" & "data" files for "', fn(1:iL), '"'
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
           nbFields = -2
          ENDIF
         ELSE
C-      Old meta-file without List of Fields
c         WRITE(msgBuf,'(4A)') 'WARNING >> ',
c    &      ' try to read pickup as it used to be written'
c         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c         WRITE(msgBuf,'(4A)') 'WARNING >> ',
c    &      ' until checkpoint59l (2007 Dec 17)'
c         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'no field-list found in meta-file'
          CALL PRINT_ERROR( msgBuf, myThid )
          StopFlag = .TRUE.
         ENDIF
        ENDIF
       ENDIF
       IF ( StopFlag ) THEN
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R DIC_READ_PICKUP'
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.EQ.0 ) THEN
C---   Old meta-file without List of Fields: use the old way to read pickup

       ELSEIF ( nbFields.NE.-2 ) THEN
C---   New way to read DIC pickup:
        nj = 0
C---    read DIC 3-D fields for restart
#ifdef DIC_CALCITE_SAT
        IF ( useCalciteSaturation ) THEN
         CALL READ_MFLDS_3D_RL( 'DIC_pH3d', pH3D,
     &                                nj, fp, Nr, myIter, myThid )
         _BEGIN_MASTER( myThid )
         pH_isLoaded(2) = .TRUE.
         _END_MASTER( myThid )
        ENDIF
#endif

C-    switch to 2-D fields:
        nj = nj*Nr
C---    read DIC 2-D fields for restart
        CALL READ_MFLDS_3D_RL( 'DIC_pH2d', pH,
     &                                 nj, fp, 1 , myIter, myThid )
        _BEGIN_MASTER( myThid )
        pH_isLoaded(1) = .TRUE.
        _END_MASTER( myThid )

C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'DIC_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R DIC_READ_PICKUP (list-size Pb)'
       ENDIF
       IF ( nMissing.GE.1 ) THEN
        DO j=1,nMissing
         IF ( missFldList(nj) .EQ. 'DIC_pH2d' ) THEN
           _BEGIN_MASTER( myThid )
           pH_isLoaded(1) = .FALSE.
           _END_MASTER( myThid )
         ELSEIF ( missFldList(nj) .EQ. 'DIC_pH3d' ) THEN
           _BEGIN_MASTER( myThid )
           pH_isLoaded(2) = .FALSE.
           _END_MASTER( myThid )
         ELSE
          StopFlag = .TRUE.
          WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
          CALL PRINT_ERROR( msgBuf, myThid )
         ENDIF
        ENDDO
        IF ( pickupStrictlyMatch .AND. .NOT.StopFlag ) THEN
          StopFlag = .TRUE.
          WRITE(msgBuf,'(4A)') 'DIC_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_ERROR( msgBuf, myThid )
        ENDIF
       ENDIF
       IF ( StopFlag ) THEN
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R DIC_READ_PICKUP'
       ENDIF

       _BEGIN_MASTER( myThid )
       IF ( .NOT.pH_isLoaded(1) ) THEN
         WRITE(msgBuf,'(2A)') 'WARNING >> DIC_READ_PICKUP: ',
     &      'will restart from approximated 2-D pH'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       ENDIF
       IF ( useCalciteSaturation .AND. .NOT.pH_isLoaded(2) ) THEN
         WRITE(msgBuf,'(2A)') 'WARNING >> DIC_READ_PICKUP: ',
     &      'will restart from approximated 3-D pH'
c        CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
       ENDIF
       _END_MASTER( myThid )

C--    Update overlap regions:
       CALL EXCH_XY_RL( pH, myThid )
#ifdef DIC_CALCITE_SAT
       IF ( useCalciteSaturation ) THEN
c        CALL EXCH_3D_RL( pH3D, Nr, myThid )
       ENDIF
#endif

#endif /* DIC_BIOTIC */
#endif /* ALLOW_DIC  */

      RETURN
      END
