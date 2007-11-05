C $Header: /u/gcmpack/MITgcm/pkg/ptracers/ptracers_write_pickup.F,v 1.4 2007/11/05 18:48:04 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_WRITE_PICKUP( permCheckPoint,
     &                    suff, myIter, myTime, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"

C !INPUT PARAMETERS: ===================================================
C     permCheckPoint  :: permanent or a rolling checkpoint
C     suff            :: suffix for pickup file (eg. ckptA or 0000000010)
C     myIter          :: time-step number
C     myTime          :: model time
C     myThid          :: thread number
      LOGICAL permCheckPoint
      CHARACTER*(*) suff
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C  iTracer		:: loop indices
C  iRec    		:: record number
C  fn   		:: character buffer for creating filename
C  prec 		:: precision of pickup files
      INTEGER iTracer,prec,iRec,lChar
      CHARACTER*(MAX_LEN_FNAM) fn
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
C       step since we're saving the gpTrNm1 variable first
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
#endif /*  ALLOW_MNC  */

      IF ( PTRACERS_pickup_write_mdsio ) THEN

        lChar = ILNBLNK(suff)
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_ptracers'
        ELSE
          WRITE(fn,'(2A)') 'pickup_ptracers.',suff(1:lChar)
        ENDIF
        prec = precFloat64

C       Write fields & tendencies (needed for AB) as consecutive
C       records, one tracer after the other, for all available tracers.
C       note: this allow to restart from a pickup with a different
C       number of tracers, with read_pickup reading only the tracers
C       "InUse".
        DO iTracer=1,PTRACERS_num
          iRec = 2*iTracer - 1
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     &         pTracer(1-Olx,1-Oly,1,1,1,iTracer),
     &         iRec, myIter, myThid )
          iRec = 2*iTracer
          CALL WRITE_REC_3D_RL( fn, prec, Nr,
     &         gpTrNm1(1-Olx,1-Oly,1,1,1,iTracer),
     &         iRec, myIter, myThid )
        ENDDO

      ENDIF

#endif /* ALLOW_PTRACERS */

      RETURN
      END
