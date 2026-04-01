#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_OUTPUT
C     !INTERFACE:
      SUBROUTINE THSICE_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_OUTPUT
C     | o general routine for ThSIce output
C     *==========================================================*
C     | - write time-average & snap-shot output
C     | - call monitor to write global quantities
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     !FUNCTIONS:
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
      CHARACTER*(10) suff
#ifdef ALLOW_MNC
      INTEGER i
      CHARACTER*(MAX_LEN_MBUF) mncf
      CHARACTER*(1) pf
#endif

C     Dump a snap-shot of main state variables:
      IF (
     &     DIFFERENT_MULTIPLE( thSIce_diagFreq, myTime, deltaTClock )
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN

        IF ( thSIce_snapshot_mdsio .AND.
     &       ( myTime.NE.startTime .OR. .NOT.thSIce_skipThermo
     &                             .OR. .NOT.useCoupler )
     &     ) THEN

          IF ( rwSuffixType.EQ.0 ) THEN
            WRITE(suff,'(I10.10)') myIter
          ELSE
            CALL RW_GET_SUFFIX( suff, myTime, myIter, myThid )
          ENDIF

          CALL WRITE_FLD_XY_RL('ice_fract.',  suff,iceMask,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_iceH.',   suff,iceHeight,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_snowH.',  suff,snowHeight,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_Tsrf.',   suff,Tsrf,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_Tice1.',  suff,Tice1,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_Tice2.',  suff,Tice2,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_Qice1.',  suff,Qice1,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_Qice2.',  suff,Qice2,
     &         myIter,myThid)
          CALL WRITE_FLD_XY_RL('ice_snowAge.',suff,snowAge,
     &         myIter,myThid)
          IF ( myTime.NE.startTime ) THEN
            CALL WRITE_FLD_XY_RL('ice_flxAtm.',suff,icFlxAtm,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('ice_frwAtm.',suff,icFrwAtm,
     &           myIter,myThid)
          ENDIF
          IF ( stepFwd_oceMxL ) THEN
            CALL WRITE_FLD_XY_RL('ice_tOceMxL.', suff, tOceMxL,
     &           myIter,myThid)
            CALL WRITE_FLD_XY_RL('ice_sOceMxL.', suff, sOceMxL,
     &           myIter,myThid)
          ENDIF

        ENDIF

#ifdef ALLOW_MNC
        IF ( thSIce_snapshot_mnc ) THEN
          _BARRIER
          IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
            pf(1:1) = 'D'
          ELSE
            pf(1:1) = 'R'
          ENDIF
          DO i = 1,MAX_LEN_MBUF
            mncf(i:i) = ' '
          ENDDO
C         C             123456789 12345
          mncf(1:15) = 'thsice_snapshot'
          CALL MNC_CW_SET_UDIM(mncf, -1, myThid)
          CALL MNC_CW_I_W_S('I',mncf,0,0,'iter', myIter, myThid)
          CALL MNC_CW_SET_UDIM(mncf, 0, myThid)
          CALL MNC_CW_RL_W_S('D',mncf,0,0,'T', myTime, myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'iceMask',   iceMask,   myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'iceHeight', iceHeight, myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'snowHeight',snowHeight,myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'Tsrf',      Tsrf,      myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'Tice1',     Tice1,     myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'Tice2',     Tice1,     myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'Qice1',     Qice1,     myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'Qice2',     Qice2,     myThid)
          CALL MNC_CW_RL_W(pf,mncf,0,0,'snowAge',   snowAge,   myThid)
          IF ( stepFwd_oceMxL ) THEN
            CALL MNC_CW_RL_W(pf,mncf,0,0,'tOceMxL',tOceMxL,myThid)
            CALL MNC_CW_RL_W(pf,mncf,0,0,'sOceMxL',sOceMxL,myThid)
          ENDIF
          _BARRIER
        ENDIF
#endif  /* ALLOW_MNC */

      ENDIF

      IF ( thSIce_monFreq.GT. 0. _d 0 )
     &    CALL THSICE_MONITOR( myTime, myIter, myThid )

#endif /* ALLOW_THSICE */

      RETURN
      END
