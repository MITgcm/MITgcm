C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_write_checkpoint.F,v 1.5 2004/07/26 17:06:57 jmc Exp $
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
#include "PTRACERS_SIZE.h"
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

C Write fields & tendancies (needed for AB) as consecutive records,
C  one tracer after the other, for all available tracers.
C  note: this allow to restart from a pickup with a different number of
C      tracers, with read_pickup reading only the tracers "InUse".
      DO iTracer=1,PTRACERS_num
       iRec = 2*iTracer - 1 
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &      pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)

       iRec = 2*iTracer
       CALL MDSWRITEFIELD(fn,prec,lgf,'RL',Nr,
     &      gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),iRec,myIter,myThid)
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
