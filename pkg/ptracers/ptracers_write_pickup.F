C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_pickup.F,v 1.16 2014/08/18 14:34:27 jmc Exp $
C $Name:  $

#include "GAD_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_WRITE_PICKUP( permCheckPoint,
     &                    suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
#include "PTRACERS_MOD.h"
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GAD.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C     permCheckPoint  :: permanent or a rolling checkpoint
C     suff            :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime          :: model time
C     myIter          :: time-step number
C     myThid          :: thread number
      LOGICAL permCheckPoint
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C     iTracer     :: tracer index
C     j           :: loop index / field number
C     prec        :: pickup-file precision
C     glf         :: local flag for "globalFiles"
C     fn          :: character buffer for creating filename
C     nWrFlds     :: number of fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     msgBuf      :: Informational/error message buffer
      INTEGER iTracer, j, prec, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 3*PTRACERS_num )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef PTRACERS_ALLOW_DYN_STATE
      INTEGER n, iRec
#endif
CEOP

#ifdef ALLOW_MNC
      IF ( PTRACERS_pickup_write_mnc ) THEN
        IF ( permCheckPoint ) THEN
          WRITE(fn,'(A)') 'pickup_ptracers'
        ELSE
          lChar = ILNBLNK(suff)
          WRITE(fn,'(2A)') 'pickup_ptracers.', suff(1:lChar)
        ENDIF
        CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
C       First ***define*** the file group name
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        IF ( permCheckPoint ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, -1, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
C       Then set the actual unlimited dimension
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
C       The following two values should probably be for the n-1 time
C       step since we are saving the gpTrNm1 variable first
        CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        DO iTracer = 1,PTRACERS_numInUse
          CALL MNC_CW_RL_W('D',fn,0,0, PTRACERS_names(iTracer),
     &         gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),myThid)
        ENDDO
        CALL MNC_CW_SET_UDIM(fn, 2, myThid)
        CALL MNC_CW_RL_W_S('D',fn,0,0,'T', myTime, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        DO iTracer = 1,PTRACERS_numInUse
          CALL MNC_CW_RL_W('D',fn,0,0, PTRACERS_names(iTracer),
     &         pTracer(1-OLx,1-OLy,1,1,1,iTracer),myThid)
        ENDDO
      ENDIF
      IF ( useMNC .AND. PTRACERS_pickup_write_mnc ) THEN
       DO iTracer = 1, PTRACERS_numInUse
        IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
         WRITE(msgBuf,'(3A)')'PTRACERS_WRITE_PICKUP: MNC not yet coded',
     &                       ' for SOM advection',
     &                       ' => write bin file instead'
         CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                       SQUEEZE_RIGHT, myThid)
        ENDIF
       ENDDO
      ENDIF
#endif /*  ALLOW_MNC  */

      lChar = ILNBLNK(suff)
      IF ( PTRACERS_pickup_write_mdsio ) THEN

        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_ptracers'
        ELSE
          WRITE(fn,'(2A)') 'pickup_ptracers.',suff(1:lChar)
        ENDIF
        prec = precFloat64

C       Firstly, write ptracer fields as consecutive records,
C       one tracer after the other, for all tracers "InUse".

        j  = 0
C     record number < 0 : a hack not to write meta files now:
        DO iTracer = 1, PTRACERS_numInUse
          j = j + 1
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     &         pTracer(1-OLx,1-OLy,1,1,1,iTracer),
     &         -j, myIter, myThid )
          IF (j.LE.listDim)
     &      wrFldList(j) = 'pTr'//PTRACERS_ioLabel(iTracer)//'   '
        ENDDO

C       Then write ptracer tendencies (if this tracer is using AB time-stepping)
        DO iTracer = 1, PTRACERS_numInUse
         IF ( PTRACERS_AdamsBashGtr(iTracer) .OR.
     &        PTRACERS_AdamsBash_Tr(iTracer) ) THEN
          j = j + 1
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     &         gpTrNm1(1-OLx,1-OLy,1,1,1,iTracer),
     &         -j, myIter, myThid )
          IF ( j.LE.listDim .AND. PTRACERS_AdamsBashGtr(iTracer) )
     &      wrFldList(j) = 'gPtr'//PTRACERS_ioLabel(iTracer)//'m1'
          IF ( j.LE.listDim .AND. PTRACERS_AdamsBash_Tr(iTracer) )
     &      wrFldList(j) = 'pTr'//PTRACERS_ioLabel(iTracer)//'Nm1'
         ENDIF

        ENDDO

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'PTRACERS_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'PTRACERS_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R PTRACERS_WRITE_PICKUP (list-size Pb)'
        ENDIF
#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta files
        glf  = globalFiles
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, prec, glf, .FALSE.,
     &                         0, 0, Nr, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         j, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------
      ENDIF

#ifdef PTRACERS_ALLOW_DYN_STATE
C     write pickup for 2nd-order moment fields
C     we write a separate file for each Ptracer that uses SOM advection
        DO iTracer = 1, PTRACERS_numInUse
         IF ( PTRACERS_SOM_Advection(iTracer) ) THEN
          IF ( lChar.EQ.0 ) THEN
            WRITE(fn,'(2A)') 'pickup_somTRAC',PTRACERS_ioLabel(iTracer)
          ELSE
            WRITE(fn,'(4A)') 'pickup_somTRAC',PTRACERS_ioLabel(iTracer),
     &                       '.',suff(1:lChar)
          ENDIF
          _BEGIN_MASTER(myThid)
          WRITE(msgBuf,'(A,I4,A)')'PTRACERS_WRITE_PICKUP: iTracer =',
     &                       iTracer, ' : writing 2nd-order moments'
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          j = ILNBLNK(fn)
          WRITE(msgBuf,'(A,A)') ' to file: ',fn(1:j)
          CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                        SQUEEZE_RIGHT, myThid )
          _END_MASTER(myThid)
          prec = precFloat64
C     Write 2nd Order moments as consecutive records
          DO n=1,nSOM
            iRec = n
            CALL WRITE_REC_3D_RL( fn, prec, Nr,
     I               _Ptracers_som(:,:,:,:,:,n,iTracer),
     I               iRec, myIter, myThid )
          ENDDO
         ENDIF
        ENDDO
#endif /* PTRACERS_ALLOW_DYN_STATE */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
