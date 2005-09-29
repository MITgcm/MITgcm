C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_write_tave.F,v 1.11 2005/09/29 12:19:51 edhill Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE AIM_WRITE_TAVE( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE AIM_WRITE_TAVE
C     | o Write out AIM time-average output
C     *==========================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"
#include "AIM_DIAGS.h"
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid - Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_AIM

C     == Local variables ==
      INTEGER bi, bj, K
      CHARACTER*(MAX_LEN_MBUF) msgBuf, suff, fn
      LOGICAL gf
#ifdef ALLOW_MNC
      CHARACTER*(8) mncfn
      CHARACTER*(1) pf
#endif
      
#ifdef ALLOW_AIM_TAVE
      
      IF (aim_taveFreq.LE.0.) RETURN
      
      IF ( myIter.EQ.nIter0 ) THEN
C     Initialize average arrays to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(USTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(VSTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(TSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(OLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SHFtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(EVAPtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECNVtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECLStave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CLOUDCtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CLTOPtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CBMFtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(DRAGtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimV0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimT0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimQ0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(EnFxPrtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(albedotave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(dTsurftave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimRHtave,Nr, bi, bj, myThid)
         DO k=1,Nr
           aim_timeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO
      ENDIF

C     Dump files and restart average computation if needed
      IF ( myIter.NE.nIter0 .AND.
     &     DIFFERENT_MULTIPLE(aim_taveFreq,myTime,deltaTClock)
     &   ) THEN

C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(USTRtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(VSTRtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(TSRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(OLRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SSRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SLRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(SHFtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(EVAPtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PRECNVtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(PRECLStave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(CLOUDCtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(CLTOPtave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(CBMFtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(DRAGtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(aimV0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(aimT0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(aimQ0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(EnFxPrtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(albedotave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(dTsurftave,aim_timeAve,1,bi,bj,myThid)

         CALL TIMEAVE_NORMALIZ(aimRHtave,aim_timeAve,Nr,bi,bj,myThid)
        ENDDO
       ENDDO

       _BARRIER
       _BEGIN_MASTER( myThid )

#ifdef ALLOW_MNC
       IF (useMNC .AND. aim_timeave_mnc) THEN
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
C        C             12345678
         mncfn(1:8) = 'aim_tave'
         CALL MNC_CW_SET_UDIM(mncfn, -1, myThid)
         CALL MNC_CW_RL_W_S('D',mncfn,0,0,'T',myTime,myThid)
         CALL MNC_CW_SET_UDIM(mncfn, 0, myThid)
         CALL MNC_CW_I_W_S('I',mncfn,0,0,'iter',myIter,myThid)
         
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'USTRtave',  USTRtave,  myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'VSTRtave',  VSTRtave,  myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'TSRtave',   TSRtave,   myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'OLRtave',   OLRtave,   myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'SSRtave',   SSRtave,   myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'SLRtave',   SLRtave,   myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'SHFtave',   SHFtave,   myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'EVAPtave',  EVAPtave,  myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'PRECNVtave',PRECNVtave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'PRECLStave',PRECLStave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'CLOUDCtave',CLOUDCtave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'CLTOPtave', CLTOPtave, myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'CBMFtave',  CBMFtave,  myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'DRAGtave',  DRAGtave,  myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'aimV0tave', aimV0tave, myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'aimT0tave', aimT0tave, myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'aimQ0tave', aimQ0tave, myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'EnFxPrtave',EnFxPrtave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'albedotave',albedotave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'dTsurftave',dTsurftave,myThid)
         CALL MNC_CW_RL_W(pf,mncfn,0,0,'aimRHtave', aimRHtave, myThid)
       ENDIF
#endif /*  ALLOW_MNC  */

       IF (aim_timeave_mdsio) THEN

C        find wether we are writing globalFile or tile-files:
         CALL GET_WRITE_GLOBAL_FLD( gf )
         
         WRITE(fn,'(A,I10.10)') 'aimPhytave.',myIter
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,USTRtave,   1,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,VSTRtave,   2,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,TSRtave ,   3,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,OLRtave ,   4,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,SSRtave ,   5,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,SLRtave ,   6,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,SHFtave ,   7,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,EVAPtave,   8,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,PRECNVtave, 9,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,PRECLStave,10,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,CLOUDCtave,11,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,CLTOPtave, 12,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,CBMFtave,  13,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,DRAGtave,  14,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,aimV0tave, 15,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,aimT0tave, 16,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,aimQ0tave, 17,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,EnFxPrtave,18,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,albedotave,19,myIter,myThid)
         CALL MDSWRITEFIELD(fn,writeBinaryPrec,gf,
     &        'RL',1,dTsurftave,20,myIter,myThid)
         
         WRITE(suff,'(I10.10)') myIter
         CALL WRITE_FLD_XYZ_RL('aim_RHtave.',suff,aimRHtave,
     &        myIter,myThid)
         
         WRITE(msgBuf,'(A,I10)')
     &        '// AIM Time-average  written, t-step', myIter
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)') ' '
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &        SQUEEZE_RIGHT, myThid )
         
       ENDIF

       _END_MASTER( myThid )
       _BARRIER

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(USTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(VSTRtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(TSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(OLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SSRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SLRtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(SHFtave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(EVAPtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECNVtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(PRECLStave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CLOUDCtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CLTOPtave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(CBMFtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(DRAGtave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimV0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimT0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimQ0tave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(EnFxPrtave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(albedotave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(dTsurftave,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(aimRHtave,Nr, bi, bj, myThid)
         DO k=1,Nr
           aim_timeAve(k,bi,bj)=0.
         ENDDO                     
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_AIM_TAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
