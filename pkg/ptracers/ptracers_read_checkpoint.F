C $Header: /u/gcmpack/MITgcm/pkg/ptracers/Attic/ptracers_read_checkpoint.F,v 1.3 2003/06/12 18:41:46 jmc Exp $
C $Name:  $

#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: PTRACERS_READ_CHECKPOINT

C !INTERFACE: ==========================================================
      SUBROUTINE PTRACERS_READ_CHECKPOINT( myIter,suff,myThid )

C !DESCRIPTION:
C     Reads current state of passive tracers from a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "PTRACERS.h"

C !INPUT PARAMETERS: ===================================================
C  myIter               :: time-step number
C  suff                 :: filename suffix
C  myThid               :: thread number
      INTEGER myIter
      CHARACTER*(10) suff
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
C  iTracer		:: loop indices
C  iRec    		:: record number
C  fn   		:: character buffer for creating filename
C  prec 		:: precision of pickup files
C  fn                   :: suffix for pickup file (eg. 0000000010)
      INTEGER iTracer,prec,iRec
      CHARACTER*(MAX_LEN_MBUF) fn
CEOP

      WRITE(fn,'(A,A10)') 'pickup_ptracers.',suff
      prec = precFloat64

C Write fields as consecutive records
      DO iTracer=1,PTRACERS_num
       iRec = iTracer
       CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &      pTracer(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
       _EXCH_XYZ_R8(pTracer(1-Olx,1-Oly,1,1,1,iTracer),myThid)
      ENDDO

C Write historical tendancies as consecutive records
      DO iTracer=1,PTRACERS_num
       iRec = iTracer + PTRACERS_num
       CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &      gPtr(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
       _EXCH_XYZ_R8(gPtr(1-Olx,1-Oly,1,1,1,iTracer),myThid)
      ENDDO
      DO iTracer=1,PTRACERS_num
       iRec = iTracer + PTRACERS_num*2
       CALL MDSREADFIELD(fn,prec,'RL',Nr,
     &      gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),iRec,myThid)
       _EXCH_XYZ_R8(gPtrNm1(1-Olx,1-Oly,1,1,1,iTracer),myThid)
      ENDDO

#endif /* ALLOW_PTRACERS */

      RETURN
      END
