#include "MYPACKAGE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MYPACKAGE_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE MYPACKAGE_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of MYPACKAGE from a pickup file

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MYPACKAGE.h"

C     !INPUT PARAMETERS:
C     myIter            :: time-step number
C     myThid            :: thread number
      INTEGER myIter
      INTEGER myThid

#if (defined MYPACKAGE_3D_STATE) || (defined MYPACKAGE_2D_STATE)

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
          WRITE(fn,'(A,I10.10)') 'pickup_mypackage.', myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
          WRITE(fn,'(A,A)') 'pickup_mypackage.', suff
        ENDIF
       ELSE
        WRITE(fn,'(A,A10)') 'pickup_mypackage.', pickupSuff
       ENDIF
       fp = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )
       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.fp ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'MYPACKAGE_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'MYPACKAGE_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R MYPACKAGE_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'MYPACKAGE_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'MYPACKAGE_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MYPACKAGE_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> MYPACKAGE_READ_PICKUP: ',
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
          WRITE(msgBuf,'(4A)') 'MYPACKAGE_READ_PICKUP: ',
     &      'no field-list found in meta-file'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R MYPACKAGE_READ_PICKUP'
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.EQ.0 ) THEN
C---   Old way to read pickup:

       ELSE
C---   New way to read MYPACKAGE pickup:
        nj = 0
C---    read MYPACKAGE 3-D fields for restart
#ifdef MYPACKAGE_3D_STATE
        CALL READ_MFLDS_3D_RL( 'myPaSta1', myPa_StatScal1,
     &                                 nj, fp, Nr, myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'myPaSta2', myPa_StatScal2,
     &                                 nj, fp, Nr, myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'myPaStaU', myPa_StatVelU,
     &                                 nj, fp, Nr, myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'myPaStaV', myPa_StatVelV,
     &                                 nj, fp, Nr, myIter, myThid )
#endif /* MYPACKAGE_3D_STATE */
        nj = nj*Nr
C---    read MYPACKAGE 2-D fields for restart
#ifdef MYPACKAGE_2D_STATE
        CALL READ_MFLDS_3D_RL( 'myPaSur1', myPa_Surf1,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'myPaSur2', myPa_Surf2,
     &                                 nj, fp, 1 , myIter, myThid )
#endif /* MYPACKAGE_2D_STATE */

C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'MYPACKAGE_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R MYPACKAGE_READ_PICKUP (list-size Pb)'
       ENDIF
       IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        DO j=1,nMissing
         WRITE(msgBuf,'(4A)') 'MYPACKAGE_READ_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDDO
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R MYPACKAGE_READ_PICKUP'
       ENDIF

C--    Update overlap regions:
#ifdef MYPACKAGE_3D_STATE
        CALL EXCH_3D_RL( myPa_StatScal1, Nr, myThid )
        CALL EXCH_3D_RL( myPa_StatScal2, Nr, myThid )
        IF ( myPa_StaV_Cgrid ) THEN
         CALL EXCH_UV_3D_RL( myPa_StatVelU, myPa_StatVelV,
     &                       .TRUE., Nr, myThid )
        ELSE
C-      Assume Agrid position:
         CALL EXCH_UV_AGRID_3D_RL( myPa_StatVelU, myPa_StatVelV,
     &                       .TRUE., Nr, myThid )
        ENDIF
#endif /* MYPACKAGE_3D_STATE */
#ifdef MYPACKAGE_2D_STATE
        CALL EXCH_XY_RL( myPa_Surf1, myThid )
        CALL EXCH_XY_RL( myPa_Surf2, myThid )
#endif /* MYPACKAGE_2D_STATE */

#endif /* MYPACKAGE_3D_STATE or MYPACKAGE_2D_STATE */

      RETURN
      END
