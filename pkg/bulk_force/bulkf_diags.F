C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/Attic/bulkf_diags.F,v 1.2 2002/12/04 14:53:39 cheisey Exp $
C $Name:  $
cswdblk modified for bulk formula -- Dec 01

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE BULKF_DIAGS( myTime, myIter, myThid )
C     /==========================================================\
C     | SUBROUTINE BULKF_DIAGS                                  |
C     | o Do BULKF diagnostic output                           |
C     |==========================================================|
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
cswdblk --- add --
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#include "BULKF_DIAG.h" 
#endif 
cswdblk -- end add ---
C     == Routine arguments ==
C     myTime - Current time of simulation ( s )
C     myIter - Iteration number
C     myThid -  Number of this instance 
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

CEndOfInterface


C     == Local variables ==
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE
      INTEGER bi, bj
      CHARACTER*(MAX_LEN_MBUF) suff

#ifdef ALLOW_BULK_FORCE


#ifdef ALLOW_TIMEAVE


C     Dump files and restart average computation if needed
      IF ( DIFFERENT_MULTIPLE(taveFreq,myTime,myTime-deltaTClock)
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
         CALL TIMEAVE_NORMALIZ(BULK_flw_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_solar_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)
         CALL TIMEAVE_NORMALIZ(BULK_ssq_AVE,BULKF_timeave, 1 ,
     &                 bi,bj,myThid)

        ENDDO
       ENDDO
         

       _BEGIN_MASTER(myThid)
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
       CALL WRITE_FLD_XY_RL('BULK_flw.',suff,
     &     BULK_flw_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_solar.',suff,
     &     BULK_solar_AVE,myIter,myThid)
       CALL WRITE_FLD_XY_RL('BULK_ssq.',suff,
     &     BULK_ssq_AVE,myIter,myThid)
       _END_MASTER(myThid)


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
         CALL TIMEAVE_RESET(BULK_flw_AVE,1,bi,bj,myThid)
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
