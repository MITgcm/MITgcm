C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_output.F,v 1.14 2013/05/02 20:01:21 jmc Exp $
C $Name:  $

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
#include "THSICE_TAVE.h"

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
      CHARACTER*(MAX_LEN_MBUF) suff
#ifdef ALLOW_TIMEAVE
      CHARACTER*(MAX_LEN_MBUF) fn
      INTEGER bi, bj, kl
#endif
#ifdef ALLOW_MNC
      INTEGER i
      CHARACTER*(MAX_LEN_MBUF) mncf
      CHARACTER*(1) pf
#endif

#ifdef ALLOW_TIMEAVE

c set arrays to zero if first timestep
      IF ( myIter .EQ. nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(ice_fract_Ave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_iceH_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_snowH_Ave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_Tsrf_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice1_Ave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice2_Ave,  1, bi, bj, myThid)
c        CALL TIMEAVE_RESET(ice_snowPr_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_flx2oc_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_frw2oc_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_salFx_Ave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_flxAtm_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_frwAtm_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_albedo_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_tMxL_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_sMxL_Ave,   1, bi, bj, myThid)
         ice_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
      ELSEIF (
     &  DIFFERENT_MULTIPLE(thSIce_taveFreq,myTime,deltaTClock)
     &     ) THEN

C--    Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C-     area weighted average (with ice-fraction)
         CALL TIMEAVE_NORMA_2V( ice_iceH_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_snowH_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_Tsrf_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_Tice1_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_Tice2_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
c        CALL TIMEAVE_NORMA_2V( ice_snowPr_Ave,
c    &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_albedo_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )

C-     simple time average :
         CALL TIMEAVE_NORMALIZE(ice_fract_Ave,ice_timeAve,1 ,
     &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_iceH_Ave,   ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_snowH_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_Tsrf_Ave,   ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_Tice1_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_Tice2_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZE(ice_snowPr_Ave, ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(ice_flx2oc_Ave, ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(ice_frw2oc_Ave, ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZE(ice_salFx_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         IF ( fluidIsWater ) THEN
          CALL TIMEAVE_NORMALIZE(ice_flxAtm_Ave,ice_timeAve, 1 ,
     &                 bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(ice_frwAtm_Ave,ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         ENDIF
         IF ( stepFwd_oceMxL ) THEN
          CALL TIMEAVE_NORMALIZE(ice_tMxL_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
          CALL TIMEAVE_NORMALIZE(ice_sMxL_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         ENDIF
        ENDDO
       ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--    Write all time-average variables in 1 file :

       IF ( thSIce_tave_mdsio ) THEN

         WRITE(fn,'(A,I10.10)') 'thSIce_tave.', myIter

         CALL WRITE_REC_XY_RL( fn, ice_fract_Ave,  1, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_iceH_Ave,   2, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_snowH_Ave,  3, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_Tsrf_Ave,   4, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_Tice1_Ave,  5, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_Tice2_Ave,  6, myIter, myThid )
c        CALL WRITE_REC_XY_RL( fn, ice_snowPr_Ave, 7, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_albedo_Ave, 7, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_flx2oc_Ave, 8, myIter, myThid )
         CALL WRITE_REC_XY_RL( fn, ice_frw2oc_Ave, 9, myIter, myThid )
         kl = 10
         CALL WRITE_REC_XY_RL( fn, ice_salFx_Ave, kl, myIter, myThid )
         IF ( fluidIsWater ) THEN
          kl = kl+1
          CALL WRITE_REC_XY_RL(fn, ice_flxAtm_Ave,kl, myIter, myThid )
          kl = kl+1
          CALL WRITE_REC_XY_RL(fn, ice_frwAtm_Ave,kl, myIter, myThid )
         ENDIF
         IF ( stepFwd_oceMxL ) THEN
          kl = kl+1
          CALL WRITE_REC_XY_RL(fn, ice_tMxL_Ave,  kl, myIter, myThid )
          kl = kl+1
          CALL WRITE_REC_XY_RL(fn, ice_sMxL_Ave,  kl, myIter, myThid )
         ENDIF

       ENDIF

#ifdef ALLOW_MNC
       IF ( useMNC .AND. thSIce_tave_mnc ) THEN
         _BARRIER
         IF ( writeBinaryPrec .EQ. precFloat64 ) THEN
           pf(1:1) = 'D'
         ELSE
           pf(1:1) = 'R'
         ENDIF
         DO i = 1,MAX_LEN_MBUF
           mncf(i:i) = ' '
         ENDDO
C        C             123456789 1
         mncf(1:11) = 'thsice_tave'
         CALL MNC_CW_SET_UDIM(mncf, -1, myThid)
         CALL MNC_CW_RL_W_S('D',mncf,0,0,'T', myTime, myThid)
         CALL MNC_CW_SET_UDIM(mncf, 0, myThid)
         CALL MNC_CW_I_W_S('I',mncf,0,0,'iter', myIter, myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'fract_ave', ice_fract_Ave, myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'iceH_ave',  ice_iceH_Ave,  myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'snowH_ave', ice_snowH_Ave, myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'Tsrf_ave',  ice_Tsrf_Ave,  myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'Tice1_ave', ice_Tice1_Ave, myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'Tice2_ave', ice_Tice2_Ave, myThid)
c        CALL MNC_CW_RL_W(pf,mncf,0,0,
c    &                    'snowPr_ave',ice_snowPr_Ave,myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'albedo_ave',ice_albedo_Ave,myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'flx2oc_ave',ice_flx2oc_Ave,myThid)
         CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'frw2oc_ave',ice_frw2oc_Ave,myThid)
         IF ( fluidIsWater ) THEN
           CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'flxAtm_ave',ice_flxAtm_Ave,myThid)
           CALL MNC_CW_RL_W(pf,mncf,0,0,
     &                    'frwAtm_ave',ice_frwAtm_Ave,myThid)
         ENDIF
         IF ( stepFwd_oceMxL ) THEN
           CALL MNC_CW_RL_W(pf,mncf,0,0,'tMxL_ave',ice_tMxL_Ave,myThid)
           CALL MNC_CW_RL_W(pf,mncf,0,0,'sMxL_ave',ice_sMxL_Ave,myThid)
         ENDIF
         _BARRIER
       ENDIF
#endif


C--    Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(ice_fract_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_iceH_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_snowH_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tsrf_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice1_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice2_Ave,  1, bi,bj, myThid)
c        CALL TIMEAVE_RESET(ice_snowPr_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_flx2oc_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_frw2oc_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_salFx_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_flxAtm_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_frwAtm_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_albedo_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_tMxL_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_sMxL_Ave,   1, bi, bj, myThid)
         ice_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */

C     Dump a snap-shot of main state variables:
      IF (
     &     DIFFERENT_MULTIPLE( thSIce_diagFreq, myTime, deltaTClock )
     &  .OR. dumpInitAndLast.AND.( myTime.EQ.endTime .OR.
     &                             myTime.EQ.startTime  )
     &   ) THEN

        IF ( thSIce_snapshot_mdsio ) THEN

          WRITE(suff,'(I10.10)') myIter

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
