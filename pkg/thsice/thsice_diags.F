C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_diags.F,v 1.1 2003/11/23 01:20:13 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CStartOfInterface
      SUBROUTINE THSICE_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE THSICE_DIAGS                                     |
C     | o Do ICE diagnostic output                               |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "THSICE_PARAMS.h"
#include "THSICE.h"
#include "THSICE_DIAGS.h"

C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance of INI_FORCING
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

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
         CALL TIMEAVE_RESET(ICE_iceMask_AVE,   1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_iceH_AVE, 1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_snowH_AVE,1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tsrf_AVE,      1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tice1_AVE,     1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tice2_AVE,     1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_snow_AVE,      1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_qleft_AVE,     1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_fresh_AVE,     1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_salFx_AVE,     1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_Qnet_AVE,      1, bi, bj, myThid)
         CALL TIMEAVE_RESET(ICE_FWfx_AVE,      1, bi, bj, myThid)
         ICE_TimeAve(1,bi,bj)=0.
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
         CALL TIMEAVE_NORMA_2V( ICE_iceH_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ICE_snowH_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ICE_Tsrf_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ICE_Tice1_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ICE_Tice2_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
         CALL TIMEAVE_NORMA_2V( ICE_snow_AVE,
     &                          ICE_iceMask_AVE, 1, bi, bj, myThid )
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--  simple time average :
         CALL TIMEAVE_NORMALIZ(ICE_iceMask_AVE,ICE_timeave,1 ,
     &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_iceH_AVE,  ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_snowH_AVE, ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_Tsrf_AVE,  ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_Tice1_AVE, ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_Tice2_AVE, ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
c        CALL TIMEAVE_NORMALIZ(ICE_snow_AVE,  ICE_timeave, 1 ,
c    &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ICE_qleft_AVE, ICE_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ICE_fresh_AVE, ICE_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ICE_salFx_AVE, ICE_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ICE_Qnet_AVE,  ICE_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(ICE_FWfx_AVE,  ICE_timeave, 1 ,
     &                 bi,bj,myThid)
        ENDDO
       ENDDO
         
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XY_RL('ICE_icemask-T.',suff,
     &      ICE_iceMask_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_iceheight-T.',suff,
     &     ICE_iceH_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_snowheight-T.',suff,
     &     ICE_snowH_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tsrf-T.',suff,
     &     ICE_Tsrf_AVE, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tice1-T.',suff,
     &     ICE_Tice1_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Tice2-T.',suff,
     &     ICE_Tice2_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_snow-T.',suff,
     &     ICE_snow_AVE, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Qleft-T.',suff,
     &     ICE_qleft_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_fresh-T.',suff,
     &     ICE_fresh_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_salFx-T.',suff,
     &     ICE_salFx_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_Qnet-T.',suff,
     &     ICE_Qnet_AVE, myIter,myThid)
       CALL WRITE_FLD_XY_RL('ICE_FWfx-T.',suff,
     &     ICE_FWfx_AVE, myIter,myThid)

         
C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(ICE_iceMask_AVE,1,bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_iceH_AVE,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_snowH_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tsrf_AVE,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tice1_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_Tice2_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_snow_AVE,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_qleft_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_fresh_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_salFx_AVE, 1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_Qnet_AVE,  1, bi,bj, myThid)
         CALL TIMEAVE_RESET(ICE_FWfx_AVE,  1, bi,bj, myThid)
         ICE_TimeAve(1,bi,bj) = 0.
        ENDDO
       ENDDO
         
      ENDIF

#endif /* ALLOW_TIMEAVE */

C     Dump a snap-shot of main state variables:
      IF ( DIFFERENT_MULTIPLE(thSIce_diagFreq,myTime,myTime-deltaTClock)
     &     .OR. myIter.EQ.nIter0 ) THEN
       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XY_RL('ice_icemask.',  suff,iceMask,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_iceheight.',suff,iceheight,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_snowheight.',suff,snowheight,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tsrf.', suff,Tsrf,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tice1.',suff,Tice1,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Tice2.',suff,Tice2,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Qice1.',suff,Qice1,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_Qice2.',suff,Qice2,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_snow.',suff,snow,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('ice_sage.',suff,sage,
     &     myIter,myThid)

      ENDIF
         
      IF ( thSIce_monFreq.GT. 0. _d 0 )
     &    CALL THSICE_MONITOR( myTime, myIter, myThid )

#endif /* ALLOW_THSICE */
      
      RETURN
      END
