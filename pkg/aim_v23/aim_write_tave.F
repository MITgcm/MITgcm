C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_write_tave.F,v 1.13 2009/12/29 23:02:21 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

CBOP
C     !ROUTINE: AIM_WRITE_TAVE
C     !INTERFACE:
      SUBROUTINE AIM_WRITE_TAVE( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE AIM_WRITE_TAVE
C     | o Write out AIM time-average output
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "AIM_PARAMS.h"
#include "AIM_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: Number of this instance of the routine
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP
#ifdef ALLOW_AIM

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi, bj
      CHARACTER*(10) suff
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_MBUF) msgBuf
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
         aim_timeAve(bi,bj) = 0.
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
         CALL TIMEAVE_NORMALIZE(USTRtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(VSTRtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(TSRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(OLRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(SSRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(SLRtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(SHFtave,   aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(EVAPtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(PRECNVtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(PRECLStave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(CLOUDCtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(CLTOPtave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(CBMFtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(DRAGtave,  aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(aimV0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(aimT0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(aimQ0tave, aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(EnFxPrtave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(albedotave,aim_timeAve,1,bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(dTsurftave,aim_timeAve,1,bi,bj,myThid)

         CALL TIMEAVE_NORMALIZE(aimRHtave,aim_timeAve,Nr,bi,bj,myThid)
        ENDDO
       ENDDO

#ifdef ALLOW_MNC
       IF (useMNC .AND. aim_timeave_mnc) THEN
       _BARRIER
c      _BEGIN_MASTER( myThid )
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
c      _END_MASTER( myThid )
       _BARRIER
       ENDIF
#endif /*  ALLOW_MNC  */

       IF (aim_timeave_mdsio) THEN

         WRITE(fn,'(A,I10.10)') 'aimPhytave.',myIter
         CALL WRITE_REC_XY_RL( fn, USTRtave,    1, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, VSTRtave,    2, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, TSRtave ,    3, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, OLRtave ,    4, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, SSRtave ,    5, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, SLRtave ,    6, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, SHFtave ,    7, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, EVAPtave,    8, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, PRECNVtave,  9, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, PRECLStave, 10, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, CLOUDCtave, 11, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, CLTOPtave,  12, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, CBMFtave,   13, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, DRAGtave,   14, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, aimV0tave,  15, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, aimT0tave,  16, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, aimQ0tave,  17, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, EnFxPrtave, 18, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, albedotave, 19, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, dTsurftave, 20, myIter, myThid )

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
         aim_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_AIM_TAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
