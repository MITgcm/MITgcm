C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_diags.F,v 1.5 2004/04/08 18:54:26 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DIAGS
C     !INTERFACE:
      SUBROUTINE THSICE_DIAGS( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R THSICE_DIAGS
C     | o Do ICE diagnostic output
C     *==========================================================*
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
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_THSICE

C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) suff

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
         CALL TIMEAVE_RESET(ice_snowPr_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_flx2oc_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_frw2oc_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_salFx_Ave,  1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_flxAtm_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_frwAtm_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_albedo_Ave, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_tMxL_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_sMxL_Ave,   1, bi, bj, myThid)
         ice_timeAve(1,bi,bj)=0.
        ENDDO
       ENDDO

C     Dump files and restart average computation if needed
      ELSEIF ( 
     &  DIFFERENT_MULTIPLE(thSIce_taveFreq,myTime,myTime-deltaTClock)
     &     ) THEN
         
C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
C-- area weighted average (with ice-fraction)
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
         CALL TIMEAVE_NORMA_2V( ice_snowPr_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ice_albedo_Ave,
     &                          ice_fract_Ave, 1, bi, bj, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--  simple time average :
         CALL TIMEAVE_NORMALIZ(ice_fract_Ave,ice_timeAve,1 ,
     &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_iceH_Ave,   ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_snowH_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_Tsrf_Ave,   ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_Tice1_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_Tice2_Ave,  ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ice_snowPr_Ave, ice_timeAve, 1 ,
c    &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ice_flx2oc_Ave, ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ice_frw2oc_Ave, ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ice_salFx_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         IF ( buoyancyRelation(1:7) .EQ. 'OCEANIC' ) THEN
          CALL TIMEAVE_NORMALIZ(ice_flxAtm_Ave,ice_timeAve, 1 ,
     &                 bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(ice_frwAtm_Ave,ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         ENDIF
         IF ( stepFwd_oceMxL ) THEN
          CALL TIMEAVE_NORMALIZ(ice_tMxL_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
          CALL TIMEAVE_NORMALIZ(ice_sMxL_Ave,  ice_timeAve, 1 ,
     &                 bi,bj,myThid)
         ENDIF
        ENDDO
       ENDDO
         
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XY_RL('ICE_fract-T.',  suff,
     &                       ice_fract_Ave,  myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_iceH-T.',   suff,
     &                       ice_iceH_Ave,   myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_snowH-T.',  suff,
     &                       ice_snowH_Ave,  myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tsrf-T.',   suff,
     &                       ice_Tsrf_Ave,   myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tice1-T.',  suff,
     &                       ice_Tice1_Ave,  myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tice2-T.',  suff,
     &                       ice_Tice2_Ave,  myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_snowPr-T.', suff,
     &                       ice_snowPr_Ave, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_flx2oc-T.', suff,
     &                       ice_flx2oc_Ave, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_frw2oc-T.', suff,
     &                       ice_frw2oc_Ave, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_salFx-T.',  suff,
     &                       ice_salFx_Ave,  myIter,myThid)
       IF ( buoyancyRelation(1:7) .EQ. 'OCEANIC' ) THEN
        CALL WRITE_FLD_XY_RL('ICE_flxAtm-T.', suff,
     &                       ice_flxAtm_Ave, myIter,myThid)
        CALL WRITE_FLD_XY_RL('ICE_frwAtm-T.', suff,
     &                       ice_frwAtm_Ave, myIter,myThid)
       ENDIF
       CALL WRITE_FLD_XY_RL('ICE_albedo-T.', suff,
     &                       ice_albedo_Ave, myIter,myThid)
       IF ( stepFwd_oceMxL ) THEN
        CALL WRITE_FLD_XY_RL('ICE_tMxL-T.', suff,
     &                       ice_tMxL_Ave, myIter,myThid)
        CALL WRITE_FLD_XY_RL('ICE_sMxL-T.', suff,
     &                       ice_sMxL_Ave, myIter,myThid)
       ENDIF

         
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(ice_fract_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_iceH_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_snowH_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tsrf_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice1_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_Tice2_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_snowPr_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_flx2oc_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_frw2oc_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_salFx_Ave,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_flxAtm_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_frwAtm_Ave,   1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_albedo_Ave, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ice_tMxL_Ave,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ice_sMxL_Ave,   1, bi, bj, myThid)
         ice_timeAve(1,bi,bj) = 0.
        ENDDO
       ENDDO
         
      ENDIF

#endif /* ALLOW_TIMEAVE */

C     Dump a snap-shot of main state variables:
      IF ( DIFFERENT_MULTIPLE(thSIce_diagFreq,myTime,myTime-deltaTClock)
     &     .OR. myTime.EQ.startTime
     &     .OR. myTime.EQ.endTime ) THEN

       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XY_RL('ice_fract.',  suff,iceMask,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_iceH.',   suff,iceheight,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_snowH.',  suff,snowheight,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tsrf.',   suff,Tsrf,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tice1.',  suff,Tice1,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tice2.',  suff,Tice2,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Qice1.',  suff,Qice1,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Qice2.',  suff,Qice2,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_snowPrc.',suff,snowPrc,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_snowAge.',suff,snowAge,
     &     myIter,myThid)
       IF ( stepFwd_oceMxL ) THEN
        CALL WRITE_FLD_XY_RL('ice_tOceMxL.', suff, tOceMxL,
     &     myIter,myThid)
        CALL WRITE_FLD_XY_RL('ice_sOceMxL.', suff, sOceMxL,
     &     myIter,myThid)
       ENDIF

      ENDIF
         
      IF ( thSIce_monFreq.GT. 0. _d 0 )
     &    CALL THSICE_MONITOR( myTime, myIter, myThid )

#endif /* ALLOW_THSICE */
      
      RETURN
      END
