C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_write_checkpoint.F,v 1.2 2003/05/13 13:31:45 mlosch Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_WRITE_CHECKPOINT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_WRITE_CHECKPOINT( suff,myIter,myTime,myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  suff                 :: suffix for pickup file (eg. ckptA or 0000000010)
C  myIter               :: time-step number
C  myTime               :: model time
C  myThid               :: thread number
      CHARACTER*(*) suff
      INTEGER myIter
      _RL myTime
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer		:: loop indices
C  iRec    		:: record number
C  fn   		:: character buffer for creating filename
C  prec 		:: precision of pickup files
C  lgf  		:: flag to write "global" files
      INTEGER iTracer,prec,iRec,iChar,lChar
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL lgf
CEOP

      lChar = 0
      DO iChar = 1,len(suff)
       IF ( suff(iChar:iChar) .NE. ' ') lChar=iChar
      ENDDO
      WRITE(fn,'(A,A)') 'pickup_ptracers.',suff(1:lChar)
      prec = precFloat64
      lgf = globalFiles

C Write fields as consecutive records
      DO iTracer=1,PTRACERS_num
       iRec = iTracer
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &      pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
      ENDDO
      print *, 'hallo 1'

C Write historical tendancies as consecutive records
      DO iTracer=1,PTRACERS_num
       iRec = iTracer + PTRACERS_num
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &      gPtr(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
      ENDDO
      DO iTracer=1,PTRACERS_num
       iRec = iTracer + PTRACERS_num*2
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &      gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
