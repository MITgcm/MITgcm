C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_read_pickup.F,v 1.17 2014/08/18 14:34:27 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
C     !ROUTINE: PTRACERS_READ_PICKUP

C     !INTERFACE:
      SUBROUTINE PTRACERS_READ_PICKUP( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of passive tracers from a pickup file

C     !USES:
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_START.h"
#include "PTRACERS_FIELDS.h"

C     !INPUT PARAMETERS:
C     myIter            :: time-step number
C     myThid            :: thread number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer     :: tracer index
C     iRec        :: record number
C     fn          :: character buffer for creating filename
C     prec        :: precision of pickup files
C     filePrec    :: pickup-file precision (read from meta file)
C     nbFields    :: number of fields in pickup file (read from meta file)
C     fldName     :: Name of the field to read
C     missFldList :: List of missing fields   (attempted to read but not found)
C     missFldDim  :: Dimension of missing fields list array: missFldList
C     nMissing    :: Number of missing fields (attempted to read but not found)
C     j           :: loop index
C     nj          :: record number
C     ioUnit      :: temp for writing msg unit
C     msgBuf      :: Informational/error message buffer
      INTEGER iTracer, iRec, prec
      INTEGER filePrec, nbFields
      INTEGER missFldDim, nMissing
      INTEGER nj, ioUnit
      PARAMETER( missFldDim = 2*PTRACERS_num )
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(8) fldName, missFldList(missFldDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef PTRACERS_ALLOW_DYN_STATE
      CHARACTER*(MAX_LEN_FNAM) filNam
      LOGICAL useCurrentDir, fileExist
      INTEGER n
#endif
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_MNC
      IF ( PTRACERS_pickup_read_mnc ) THEN
C       Read variables from the pickup file
        WRITE(fn,'(a)') 'pickup_ptracers'
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
        DO iTracer = 1, PTRACERS_numInUse
          CALL MNC_CW_RL_R('D',fn,0,0, PTRACERS_names(iTracer),
     &         gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),myThid)
          CALL EXCH_3D_RL( gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO
        CALL MNC_CW_SET_UDIM(fn, 2, myThid)
        DO iTracer = 1, PTRACERS_numInUse
          CALL MNC_CW_RL_R('D',fn,0,0, PTRACERS_names(iTracer),
     &         pTracer(1-OLx,1-OLy,1,1,1,iTracer),myThid)
          CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO
      ENDIF
      IF ( useMNC .AND. PTRACERS_pickup_read_mnc ) THEN
       DO iTracer = 1, PTRACERS_numInUse
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
         WRITE(msgBuf,'(3A)')'PTRACERS_READ_PICKUP: MNC not yet coded',
     &                       ' for SOM advection',
     &                       ' => read bin file instead'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid)
        ENDIF
       ENDDO
      ENDIF
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( PTRACERS_pickup_read_mdsio ) THEN

       IF ( pickupSuff.EQ.' ' ) THEN
        WRITE(fn,'(A,I10.10)') 'pickup_ptracers.',myIter
       ELSE
        WRITE(fn,'(A,A10)')    'pickup_ptracers.',pickupSuff
       ENDIF
       prec = precFloat64

       CALL READ_MFLDS_SET(
     I                      fn,
     O                      nbFields, filePrec,
     I                      Nr, myIter, myThid )
       _BEGIN_MASTER( myThid )
c      IF ( filePrec.NE.0 .AND. filePrec.NE.prec ) THEN
       IF ( nbFields.GE.0 .AND. filePrec.NE.prec ) THEN
         WRITE(msgBuf,'(2A,I4)') 'PTRACERS_READ_PICKUP: ',
     &    'pickup-file binary precision do not match !'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(A,2(A,I4))') 'PTRACERS_READ_PICKUP: ',
     &    'file prec.=', filePrec, ' but expecting prec.=', prec
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R PTRACERS_READ_PICKUP (data-prec Pb)'
       ENDIF
       _END_MASTER( myThid )

       IF ( nbFields.LE.0 ) THEN
C-      No meta-file or old meta-file without List of Fields
        ioUnit = errorMessageUnit
        IF ( pickupStrictlyMatch ) THEN
          WRITE(msgBuf,'(4A)') 'PTRACERS_READ_PICKUP: ',
     &      'no field-list found in meta-file',
     &      ' => cannot check for strick-matching'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(4A)') 'PTRACERS_READ_PICKUP: ',
     &      'try with " pickupStrictlyMatch=.FALSE.,"',
     &      ' in file: "data", NameList: "PARM03"'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          STOP 'ABNORMAL END: S/R PTRACERS_READ_PICKUP'
        ELSE
          WRITE(msgBuf,'(4A)') 'WARNING >> PTRACERS_READ_PICKUP: ',
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
     &      ' until checkpoint59l (2007 Dec 17)'
          CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         ENDIF
        ENDIF
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C---   Very Old way to read ptracer pickup:
       IF ( nbFields.EQ.0 .AND. usePickupBeforeC54 ) THEN
C       Read fields as consecutive records
        DO iTracer = 1, PTRACERS_numInUse
          iRec = iTracer
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O         pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     I         iRec, myIter, myThid )
          CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO

C       Read historical tendencies as consecutive records
c       DO iTracer = 1,PTRACERS_numInUse
c         iRec = iTracer + PTRACERS_num
c         CALL READ_REC_3D_RL( fn, prec, Nr,
c    O         gPtr(1-OLx,1-OLy,1,1,1,iTracer),
c    I         iRec, myIter, myThid )
c         CALL EXCH_3D_RL( gPtr(1-OLx,1-OLy,1,1,1,iTracer),
c    &                     Nr, myThid )
c       ENDDO
        DO iTracer = 1, PTRACERS_numInUse
          iRec = iTracer + PTRACERS_num*2
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O         gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     I         iRec, myIter, myThid )
          CALL EXCH_3D_RL( gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO

       ELSEIF ( nbFields.EQ.0 ) THEN
C---   Old way to read ptracer pickup:
C       Read fields & tendencies (needed for AB) as consecutive records,
C       one tracer after the other, only for tracers "InUse".  Note:
C       this allow to restart from a pickup with a different number of
C       tracers, with write_pickup dumping all of them (PTRACERS_num).
        DO iTracer = 1, PTRACERS_numInUse
          iRec = 2*iTracer -1
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O         pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     I         iRec, myIter, myThid )
          iRec = 2*iTracer
          CALL READ_REC_3D_RL( fn, prec, Nr,
     O         gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     I         iRec, myIter, myThid )
          CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
          CALL EXCH_3D_RL( gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO

       ELSE
C---   New way to read ptracer pickup:
        nj = 0
        DO iTracer = 1, PTRACERS_numInUse
C---    read pTracer 3-D fields for restart
          fldName = 'pTr'//PTRACERS_ioLabel(iTracer)//'   '
          CALL READ_MFLDS_3D_RL( fldName,
     O                     pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                     nj, prec, Nr, myIter, myThid )
          CALL EXCH_3D_RL( pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
        ENDDO
        DO iTracer = 1, PTRACERS_numInUse
C---    read pTracer 3-D tendencies for AB-restart
         IF ( PTRACERS_AdamsBashGtr(iTracer) .OR.
     &        PTRACERS_AdamsBash_Tr(iTracer) ) THEN
          IF ( PTRACERS_AdamsBashGtr(iTracer) )
     &      fldName = 'gPtr'//PTRACERS_ioLabel(iTracer)//'m1'
          IF ( PTRACERS_AdamsBash_Tr(iTracer) )
     &      fldName = 'pTr'//PTRACERS_ioLabel(iTracer)//'Nm1'
          CALL READ_MFLDS_3D_RL( fldName,
     O                     gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                     nj, prec, Nr, myIter, myThid )
          CALL EXCH_3D_RL( gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &                     Nr, myThid )
         ENDIF
        ENDDO

C--    end: new way to read pickup file
       ENDIF

C--    Check for missing fields:
       nMissing = missFldDim
       CALL READ_MFLDS_CHECK(
     O                     missFldList,
     U                     nMissing,
     I                     myIter, myThid )
       IF ( nMissing.GT.missFldDim ) THEN
         WRITE(msgBuf,'(2A,I4)') 'PTRACERS_READ_PICKUP: ',
     &     'missing fields list has been truncated to', missFldDim
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R PTRACERS_READ_PICKUP (list-size Pb)'
       ENDIF
       CALL PTRACERS_CHECK_PICKUP(
     I                     missFldList,
     I                     nMissing, nbFields,
     I                     myIter, myThid )

C--   end: pickup_read_mdsio
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef PTRACERS_ALLOW_DYN_STATE
c     IF ( PTRACERS_pickup_read_mdsio ) THEN

C--   Read pickup file with 2nd.Order moment fields
       prec = precFloat64
       DO iTracer = 1, PTRACERS_numInUse
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN

         IF (pickupSuff .EQ. ' ') THEN
           WRITE(fn,'(3A,I10.10)') 'pickup_somTRAC',
     &                     PTRACERS_ioLabel(iTracer),'.', myIter
         ELSE
           WRITE(fn,'(3A,A10)') 'pickup_somTRAC',
     &                     PTRACERS_ioLabel(iTracer),'.', pickupSuff
         ENDIF
         ioUnit = standardMessageUnit
         WRITE(msgBuf,'(A,I3,A)')'PTRACERS_READ_PICKUP: iTracer = ',
     &            iTracer, ' : reading 2nd-order moments from file:'
         CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
         CALL PRINT_MESSAGE( fn,     ioUnit, SQUEEZE_RIGHT, myThid )

C-      First check if pickup file exist
#ifdef ALLOW_MDSIO
         useCurrentDir = .FALSE.
         CALL MDS_CHECK4FILE(
     I                       fn, '.data', 'PTRACERS_READ_PICKUP',
     O                       filNam, fileExist,
     I                       useCurrentDir, myThid )
#else
         STOP 'ABNORMAL END: S/R PTRACERS_READ_PICKUP: Needs MDSIO pkg'
#endif

         IF ( fileExist ) THEN
C-       Read 2nd Order moments as consecutive records
          DO n=1,nSOM
           iRec = n
           CALL READ_REC_3D_RL( fn, prec, Nr,
     O               _Ptracers_som(:,:,:,:,:,n,iTracer),
     I               iRec, myIter, myThid )
          ENDDO
          CALL GAD_EXCH_SOM( _Ptracers_som(:,:,:,:,:,:,iTracer),
     &                       Nr, myThid )
         ELSE
          ioUnit = errorMessageUnit
          IF ( pickupStrictlyMatch ) THEN
            WRITE(msgBuf,'(4A)') 'PTRACERS_READ_PICKUP: ',
     &        'try with " pickupStrictlyMatch=.FALSE.,"',
     &        ' in file: "data", NameList: "PARM03"'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
            STOP 'ABNORMAL END: S/R PTRACERS_READ_PICKUP'
          ELSE
            WRITE(msgBuf,'(2A)') 'WARNING >> PTRACERS_READ_PICKUP: ',
     &        'approximated restart: reset Ptr_SOM to zero'
            CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
          ENDIF
         ENDIF
        ENDIF
       ENDDO

C--   end: pickup_read_mdsio, SOM pickups
c     ENDIF
#endif /* PTRACERS_ALLOW_DYN_STATE */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
