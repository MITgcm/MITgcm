#include "GMREDI_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: MYPACKAGE_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE GMREDI_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of MYPACKAGE from a pickup file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT PARAMETERS:
C     myIter            :: time-step number
C     myThid            :: thread number
      INTEGER myIter
      INTEGER myThid

#ifdef GM_BATES_K3D

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
      INTEGER i,j,k,n,nm,ioUnit,bi,bj
      PARAMETER( missFldDim = 12 )
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(8) missFldList(missFldDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL vec(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      CHARACTER*(8) fieldname
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( pickupSuff.EQ.' ' ) THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_gmredi.', myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
          WRITE(fn,'(A,A)') 'pickup_gmredi.', suff
        ENDIF
       ELSE
        WRITE(fn,'(A,A10)') 'pickup_gmredi.', pickupSuff
       ENDIF
       fp = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )
       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.fp ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'GMREDI_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'GMREDI_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R GMREDI_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'GMREDI_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'GMREDI_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R GMREDI_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> GMREDI_READ_PICKUP: ',
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
          WRITE(msgBuf,'(4A)') 'GMREDI_READ_PICKUP: ',
     &      'no field-list found in meta-file'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R GMREDI_READ_PICKUP'
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.EQ.0 ) THEN
C---   Old way to read pickup:

       ELSE

        nm = 0
C---    read GMREDI fields for restart

C       Centre mode
        fieldname='mode01C'
        CALL READ_MFLDS_3D_RL( fieldname, vec,
     &                         nm, fp, Nr, myIter, myThid )
        CALL EXCH_3D_RL(vec, Nr, myThid)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO k=1,Nr
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             modesC(1,i,j,k,bi,bj) = vec(i,j,k,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C      Western Mode
        DO n=1,GM_Bates_NModes
         WRITE(fieldname, '(A,I2.2,A)') 'mode',n,'W'
         CALL READ_MFLDS_3D_RL( fieldname, vec,
     &        nm, fp, Nr, myIter, myThid )
         CALL EXCH_3D_RL(vec, Nr, myThid)
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              modesW(n,i,j,k,bi,bj) = vec(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C      Southern Mode
        DO n=1,GM_Bates_NModes
         WRITE(fieldname, '(A,I2.2,A)') 'mode',n,'S'
         CALL READ_MFLDS_3D_RL( fieldname, vec,
     &        nm, fp, Nr, myIter, myThid )
         CALL EXCH_3D_RL(vec, Nr, myThid)
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO k=1,Nr
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              modesS(n,i,j,k,bi,bj) = vec(i,j,k,bi,bj)
             ENDDO
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO

        nm = nm*Nr

C       ---2D fields---
C       Deformation radius
        fieldname='Rdef'
        CALL READ_MFLDS_3D_RL( fieldname, Rdef,
     &       nm, fp, 1, myIter, myThid )
        CALL EXCH_XY_RL(Rdef, myThid)

C--   end: new way to read pickup file
      ENDIF

C--    Check for missing fields:
      nMissing = missFldDim
      CALL READ_MFLDS_CHECK(
     O     missFldList,
     U     nMissing,
     I     myIter, myThid )
      IF ( nMissing.GT.missFldDim ) THEN
        WRITE(msgBuf,'(2A,I4)') 'GMREDI_READ_PICKUP: ',
     &       'missing fields list has been truncated to', missFldDim
        CALL PRINT_ERROR( msgBuf, myThid )
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R GMREDI_READ_PICKUP (list-size Pb)'
      ENDIF
      IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        DO j=1,nMissing
         WRITE(msgBuf,'(4A)') 'GMREDI_READ_PICKUP: ',
     &        'cannot restart without field "',missFldList(nm),'"'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDDO
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R GMREDI_READ_PICKUP'
      ENDIF

C--    Update overlap regions:
C        CALL EXCH_3D_RL( myPa_StatScal1, Nr, myThid )
#endif /* GM_BATES_K3D */

      RETURN
      END
