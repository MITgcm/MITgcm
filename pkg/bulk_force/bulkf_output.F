C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_output.F,v 1.2 2008/09/09 19:58:26 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"

CBOP
C     !ROUTINE: BULKF_OUTPUT
C     !INTERFACE:
      SUBROUTINE BULKF_OUTPUT( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE BULKF_OUTPUT
C     | o Do BULKF time-average output
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "BULKF.h"
#include "BULKF_PARAMS.h"
#include "BULKF_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime :: Current time of simulation ( s )
C     myIter :: Iteration number
C     myThid ::  Number of this instance
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_BULK_FORCE
#ifdef ALLOW_TIMEAVE

C     == Function
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
C     == Local variables ==
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) suff

C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(blk_taveFreq,myTime,deltaTClock)
     &     ) THEN


C      Normalize by integrated time
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_NORMALIZ(BULK_Qnet_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_EmPmR_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_fu_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_fv_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_latent_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_sensible_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_evap_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_flwup_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_flwupnet_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_solar_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_ssq_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)

        ENDDO
       ENDDO

C--    wait for all threads to finish normalization
       _BARRIER

       WRITE(suff,'(I10.10)') myIter

       CALL WRITE_FLD_XY_RL('BULK_Qnet.',suff,BULK_Qnet_AVE,
     &     myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_EmPmR.',suff,
     &     BULK_EmPmR_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_fu.',suff,
     &     BULK_fu_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_fv.',suff,
     &     BULK_fv_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_latent.',suff,
     &     BULK_latent_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_sensible.',suff,
     &     BULK_sensible_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_evap.',suff,
     &     BULK_evap_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_flwup.',suff,
     &     BULK_flwup_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_flwupnet.',suff,
     &     BULK_flwupnet_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_solar.',suff,
     &     BULK_solar_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_ssq.',suff,
     &     BULK_ssq_AVE,myIter,myThid)

C--    all threads wait for writing to finish before resetting array
       _BARRIER

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET(BULK_Qnet_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_EmPmR_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_fu_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_fv_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_latent_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_sensible_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_evap_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_flwup_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_flwupnet_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_solar_AVE,1,bi,bj,myThid)
         CALL TIMEAVE_RESET(BULK_ssq_AVE,1,bi,bj,myThid)
         BULKF_TimeAve(1,bi,bj)=0.
        ENDDO
       ENDDO


      ENDIF

#endif /* ALLOW_TIMEAVE */

#endif /* ALLOW_BULKF */

      RETURN
      END
