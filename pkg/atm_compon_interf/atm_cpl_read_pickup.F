#include "ATM_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: ATM_CPL_READ_PICKUP
C     !INTERFACE:
      SUBROUTINE ATM_CPL_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE ATM_CPL_READ_PICKUP
C     | o Reads fields from a pickup file for a restart
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "ATMCPL.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myIter  :: Current time-step number
C     myThid  :: my Thread Id number
      INTEGER myIter
      INTEGER myThid

#ifdef COMPONENT_MODULE
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
      PARAMETER( missFldDim = 18 )
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(8) missFldList(missFldDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER i, bi, bj
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( pickupSuff.EQ.' ' ) THEN
        IF ( rwSuffixType.EQ.0 ) THEN
          WRITE(fn,'(A,I10.10)') 'pickup_cpl.', myIter
        ELSE
          CALL RW_GET_SUFFIX( suff, startTime, myIter, myThid )
          WRITE(fn,'(A,A)') 'pickup_cpl.', suff
        ENDIF
       ELSE
        WRITE(fn,'(A,A10)') 'pickup_cpl.', pickupSuff
       ENDIF
       fp = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )
       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.fp ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'ATM_CPL_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'ATM_CPL_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R ATM_CPL_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'ATM_CPL_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'ATM_CPL_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R ATM_CPL_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> ATM_CPL_READ_PICKUP: ',
     &      ' no field-list found'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         IF ( nbFields.EQ.-1 ) THEN
C-      No meta-file
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as currently written'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ELSE
C-      Old meta-file without List of Fields
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' try to read pickup as it used to be written'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          WRITE(msgBuf,'(4A)') 'WARNING >> ',
     &      ' until checkpoint65r (2015 Dec 21)'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
c         WRITE(msgBuf,'(4A)') 'ATM_CPL_READ_PICKUP: ',
c    &      'no field-list found in meta-file'
c         CALL PRINT_ERROR( msgBuf, myThid )
c         CALL ALL_PROC_DIE( myThid )
c         STOP 'ABNORMAL END: S/R ATM_CPL_READ_PICKUP'
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.EQ.0 ) THEN
C---   Old way to read pickup:

c       CALL READ_REC_3D_RL( fn, fp, 1, ocMxlD    ,  1, myIter,myThid )
c       CALL READ_REC_3D_RL( fn, fp, 1, SSTocn    ,  2, myIter,myThid )
c       CALL READ_REC_3D_RL( fn, fp, 1, SSSocn    ,  3, myIter,myThid )
c       CALL READ_REC_3D_RL( fn, fp, 1, vSqocn    ,  4, myIter,myThid )
c       CALL READ_REC_3D_RL( fn, fp, 1, atmSLPr   ,  5, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, HeatFlux  ,  6, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, qShortWave,  7, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, tauX      ,  8, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, tauY      ,  9, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, EvMPrFlux , 10, myIter,myThid )
#ifdef ALLOW_LAND
        CALL READ_REC_3D_RL( fn, fp, 1, RunOffFlux, 11, myIter,myThid )
        CALL READ_REC_3D_RL( fn, fp, 1, RunOffEnFx, 12, myIter,myThid )
#endif /* ALLOW_LAND */
#ifdef ALLOW_THSICE
        CALL READ_REC_3D_RL( fn, fp, 1, iceSaltFlx, 13, myIter,myThid )
c       CALL READ_REC_3D_RL( fn, fp, 1, seaIceMass, 14, myIter,myThid )
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AIM
        IF ( atm_cplExch_DIC ) THEN
c        CALL READ_REC_3D_RL( fn,fp, 1, flxCO2ocn , 15, myIter,myThid )
         CALL READ_REC_3D_RL( fn,fp, 1, airCO2    , 16, myIter,myThid )
         CALL READ_REC_3D_RL( fn,fp, 1, sWSpeed   , 17, myIter,myThid )
# ifdef ALLOW_THSICE
c        CALL READ_REC_3D_RL( fn,fp,1,sIceFrac_cpl, 18, myIter,myThid )
# endif /* ALLOW_THSICE */
        ENDIF
#endif /* ALLOW_AIM */

       ELSE
C---   New way to read ATM_CPL pickup:
        nj = 0
C---    read ATM_CPL 3-D fields for restart
        nj = nj*Nr

C---    read ATM_CPL 2-D fields for restart
        CALL READ_MFLDS_3D_RL( 'qHeatFlx', HeatFlux,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'qShortW ', qShortWave,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'surfTauX', tauX,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'surfTauY', tauY,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'Evp-Prec', EvMPrFlux,
     &                                 nj, fp, 1 , myIter, myThid )
#ifdef ALLOW_LAND
        IF ( atm_cplExch_RunOff ) THEN
         CALL READ_MFLDS_3D_RL('RunOffFx', RunOffFlux,
     &                                 nj, fp, 1 , myIter, myThid )
         CALL READ_MFLDS_3D_RL('RnOfEnFx', RunOffEnFx,
     &                                 nj, fp, 1 , myIter, myThid )
        ENDIF
#endif /* ALLOW_LAND */
#ifdef ALLOW_THSICE
        IF ( atm_cplExch1W_sIce ) THEN
         CALL READ_MFLDS_3D_RL('saltFlux', iceSaltFlx,
     &                                 nj, fp, 1 , myIter, myThid )
        ENDIF
        IF ( atm_cplExch_SaltPl ) THEN
         CALL READ_MFLDS_3D_RL('sltPlmFx', saltPlmFlx_cpl,
     &                                 nj, fp, 1 , myIter, myThid )
        ENDIF
#endif /* ALLOW_THSICE */
#ifdef ALLOW_AIM
        IF ( atm_cplExch_DIC ) THEN
         CALL READ_MFLDS_3D_RL('atm-CO2 ', airCO2,
     &                                 nj, fp, 1 , myIter, myThid )
         CALL READ_MFLDS_3D_RL('wndSpeed', sWSpeed,
     &                                 nj, fp, 1 , myIter, myThid )
        ENDIF
#endif /* ALLOW_AIM */

C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'ATM_CPL_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R ATM_CPL_READ_PICKUP (list-size Pb)'
       ENDIF
       IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        DO j=1,nMissing
         WRITE(msgBuf,'(4A)') 'ATM_CPL_READ_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDDO
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R ATM_CPL_READ_PICKUP'
       ENDIF

C--    Update overlap regions:

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( cpl_oldPickup ) THEN
        _BARRIER
C-    EmP & RunOff were (before checkpoint59h) in m/s , but are now in kg/m2/s:
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            EvMPrFlux (i,j,bi,bj) = EvMPrFlux (i,j,bi,bj)*rhoConstFresh
#ifdef ALLOW_LAND
            RunOffFlux(i,j,bi,bj) = RunOffFlux(i,j,bi,bj)*rhoConstFresh
#endif /* ALLOW_LAND */
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

#endif /* COMPONENT_MODULE */

      RETURN
      END
