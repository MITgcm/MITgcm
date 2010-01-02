C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_output.F,v 1.3 2010/01/02 22:46:29 jmc Exp $
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
         CALL TIMEAVE_NORMALIZE( BULK_Qnet_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_EmPmR_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_fu_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_fv_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_latent_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_sensible_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_evap_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_flwup_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_flwupnet_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_solar_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )
         CALL TIMEAVE_NORMALIZE( BULK_ssq_Ave,
     &                           BULK_timeAve, 1 , bi, bj, myThid )

        ENDDO
       ENDDO

       WRITE(suff,'(I10.10)') myIter
       CALL WRITE_FLD_XY_RL( 'BULK_Qnet.', suff,
     &                        BULK_Qnet_Ave,     myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_EmPmR.', suff,
     &                        BULK_EmPmR_Ave,    myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_fu.', suff,
     &                        BULK_fu_Ave,       myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_fv.', suff,
     &                        BULK_fv_Ave,       myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_latent.', suff,
     &                        BULK_latent_Ave,   myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_sensible.', suff,
     &                        BULK_sensible_Ave, myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_evap.', suff,
     &                        BULK_evap_Ave,     myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_flwup.', suff,
     &                        BULK_flwup_Ave,    myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_flwupnet.', suff,
     &                        BULK_flwupnet_Ave, myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_solar.', suff,
     &                        BULK_solar_Ave,    myIter, myThid )
       CALL WRITE_FLD_XY_RL( 'BULK_ssq.', suff,
     &                        BULK_ssq_Ave,      myIter, myThid )

C      Reset averages to zero
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         CALL TIMEAVE_RESET( BULK_Qnet_Ave,     1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_EmPmR_Ave,    1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_fu_Ave,       1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_fv_Ave,       1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_latent_Ave,   1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_sensible_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_evap_Ave,     1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_flwup_Ave,    1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_flwupnet_Ave, 1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_solar_Ave,    1, bi, bj, myThid )
         CALL TIMEAVE_RESET( BULK_ssq_Ave,      1, bi, bj, myThid )
         BULK_timeAve(bi,bj) = 0.
        ENDDO
       ENDDO

      ENDIF

#endif /* ALLOW_TIMEAVE */
#endif /* ALLOW_BULKF */

      RETURN
      END
