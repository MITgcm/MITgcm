C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_read_checkpoint.F,v 1.8 2004/09/03 20:10:47 edhill Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: PTRACERS_READ_CHECKPOINT

C     !INTERFACE:
      SUBROUTINE PTRACERS_READ_CHECKPOINT( myIter, myThid )

C     !DESCRIPTION:
C     Reads current state of passive tracers from a pickup file

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"

C     !INPUT PARAMETERS:
C     myIter            :: time-step number
C     myThid            :: thread number
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_PTRACERS

C     !LOCAL VARIABLES:
C     iTracer		:: loop indices
C     iRec    		:: record number
C     fn   		:: character buffer for creating filename
C     prec 		:: precision of pickup files
C     fn                :: suffix for pickup file (eg. 0000000010)
      INTEGER iTracer,prec,iRec
      CHARACTER*(MAX_LEN_MBUF) fn
CEOP

      WRITE(fn,'(A,I10.10)') 'pickup_ptracers.',myIter
      prec = precFloat64
      
#ifdef ALLOW_MNC
C       IF (useMNC .AND. PTRACERS_mnc_read) THEN
C C       Read variables from the pickup file
C         CALL MNC_FILE_CLOSE_ALL_MATCHING(fn, myThid)
C         CALL MNC_CW_SET_UDIM(fn, 1, myThid)
C         CALL MNC_CW_RL_R('D',fn,0,0,'uVelD',uVelD, myThid)
C         CALL MNC_CW_RL_R('D',fn,0,0,'vVelD',vVelD, myThid)
C         CALL MNC_CW_RL_R('D',fn,0,0,'Unm1',Unm1, myThid)
C         CALL MNC_CW_RL_R('D',fn,0,0,'Vnm1',Vnm1, myThid)
C         CALL MNC_CW_RL_R('D',fn,0,0,'etaNm1', etaNm1, myThid)
C       ELSE
#endif /*  ALLOW_MNC  */

      IF ( usePickupBeforeC54 ) THEN
C       Read fields as consecutive records
        DO iTracer=1,PTRACERS_num
          iRec = iTracer
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &         pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
      _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDDO

C       Read historical tendancies as consecutive records
        DO iTracer=1,PTRACERS_num
          iRec = iTracer + PTRACERS_num
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &         gPtr(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
      _EXCH_XYZ_R8(gPtr(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDDO
        DO iTracer=1,PTRACERS_num
          iRec = iTracer + PTRACERS_num*2
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &         gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
      _EXCH_XYZ_R8(gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDDO

      ELSE
C       Read fields & tendancies (needed for AB) as consecutive records,
C       one tracer after the other, only for tracers "InUse".  Note:
C       this allow to restart from a pickup with a different number of
C       tracers, with write_pickup dumping all of them (PTRACERS_num).
        DO iTracer=1,PTRACERS_numInUse
          iRec = 2*iTracer -1 
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &         pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
          iRec = 2*iTracer
          CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &         gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
      _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
      _EXCH_XYZ_R8(gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),myThid)
        ENDDO
        
      ENDIF

#ifdef ALLOW_MNC
C     ENDIF
#endif /*  ALLOW_MNC  */

#endif /* ALLOW_PTRACERS */

      RETURN
      END
