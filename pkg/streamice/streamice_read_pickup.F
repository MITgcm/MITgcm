C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_read_pickup.F,v 1.3 2014/03/30 18:00:23 jmc Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: STREAMICE_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE STREAMICE_READ_PICKUP( myThid )

C     !DESCRIPTION:
C     Reads current state of STREAMICE from a pickup file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"

C     !INPUT PARAMETERS:
C     myIter            :: time-step number
C     myThid            :: thread number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_STREAMICE
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
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(8) missFldList(missFldDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( pickupSuff.EQ.' ' ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_streamice.',nIter0
       ELSE
        WRITE(fn,'(A,A10)')    'pickup_streamice.',pickupSuff
       ENDIF
       fp = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, nIter0, myThid )

       _BEGIN_MASTER( myThid )
       IF ( nbFields.GE.0 .AND. filePrec.NE.fp ) THEN
         WRITE(msgBuf,'(2A,I4)') 'STREAMICE_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'STREAMICE_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', fp
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( 0 )
         STOP 'ABNORMAL END: S/R STREAMICE_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'STREAMICE_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'STREAMICE_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R STREAMICE_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> STREAMICE_READ_PICKUP: ',
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
          WRITE(msgBuf,'(4A)') 'STREAMICE_READ_PICKUP: ',
     &      'no field-list found in meta-file'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R STREAMICE_READ_PICKUP'
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF ( nbFields.EQ.0 ) THEN
C---   Old way to read pickup:

       ELSE
C---   New way to read STREAMICE pickup:
        nj = 0
C---    read STREAMICE 3-D fields for restart
#ifdef STREAMICE_HYBRID_STRESS
        CALL READ_MFLDS_3D_RL( 'visc3d  ', visc_streamice_full,
     &                                 nj, fp, Nr, myIter, myThid )
#endif /* STREAMICE_HYBRID_STRESS */
        nj = nj*Nr
C---    read STREAMICE 2-D fields for restart

        CALL READ_MFLDS_3D_RL( 'SI_area ', area_shelf_streamice,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_LEV_RS('SI_hmask', STREAMICE_hmask,
     &                            nj, fp, 1, 1, 1, myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_uvel ', U_streamice,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_vvel ', V_streamice,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_thick', H_streamice,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_betaF', tau_beta_eff_streamice,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_visc ', visc_streamice,
     &                                 nj, fp, 1 , myIter, myThid )

#ifdef STREAMICE_HYBRID_STRESS
        CALL READ_MFLDS_3D_RL( 'SI_taubx', streamice_taubx,
     &                                 nj, fp, 1 , myIter, myThid )
        CALL READ_MFLDS_3D_RL( 'SI_tauby', streamice_tauby,
     &                                 nj, fp, 1 , myIter, myThid )
#endif

C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'STREAMICE_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R STREAMICE_READ_PICKUP (list-size Pb)'
       ENDIF
       IF ( nMissing.GE.1 ) THEN
        ioUnit = errorMessageUnit
        DO j=1,nMissing
         WRITE(msgBuf,'(4A)') 'STREAMICE_READ_PICKUP: ',
     &       'cannot restart without field "',missFldList(nj),'"'
         CALL PRINT_ERROR( msgBuf, myThid )
        ENDDO
        CALL ALL_PROC_DIE( myThid )
        STOP 'ABNORMAL END: S/R STREAMICE_READ_PICKUP'
       ENDIF

C--    Update overlap regions:
#ifdef STREAMICE_HYBRID_STRESS
        CALL EXCH_3D_RL( visc_streamice_full, Nr, myThid )
#endif /* STREAMICE_HYBRID_STRESS */
        CALL EXCH_XY_RL( area_shelf_streamice, myThid )
        CALL EXCH_XY_RL( h_streamice, myThid )
        CALL EXCH_XY_RL( u_streamice, myThid )
        CALL EXCH_XY_RL( v_streamice, myThid )
        CALL EXCH_XY_RS( streamice_hmask, myThid )
        CALL EXCH_XY_RL( tau_beta_eff_streamice, myThid )
        CALL EXCH_XY_RL( visc_streamice, myThid )

c       CALL EXCH_XY_RL( myPa_Surf2, myThid )

#endif /* ALLOW_STREAMICE */

      RETURN
      END
