#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_COST
# include "COST_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

      SUBROUTINE SEAICE_COST_DRIVER( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE seaice_cost_driver
C     ==================================================================
C
C     o Evaluate cost function contributions of
C       seaice observations
C
C     ==================================================================
C     SUBROUTINE seaice_cost_driver
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
#include "EEPARAMS.h"
#if (defined ALLOW_COST && defined ALLOW_CAL && \
     defined ALLOW_SEAICE_COST_SMR_AREA)
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl_dummy.h"
#endif
#include "SEAICE_COST.h"
#endif /* ALLOW_COST and ALLOW_CAL and ALLOW_SEAICE_COST_SMR_AREA */

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#if (defined ALLOW_COST && defined ALLOW_CAL && \
     defined ALLOW_SEAICE_COST_SMR_AREA)

C     == external functions ==
      INTEGER  cal_IntDays
      EXTERNAL cal_IntDays

C     == local variables ==
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER nnzbar
      INTEGER nnzobs
      INTEGER nrecloc
      _RL spminloc
      _RL spmaxloc
      _RL spzeroloc
      _RL localperiod
C     == end of interface ==

      IF ( useSEAICE .AND. useCAL ) THEN

        WRITE(msgBuf,'(a)') 'ph-cost CALL cost_smrarea'
        CALL print_message( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        nnzbar = 1
        nnzobs = 1

C     copied from ecco_cost_init_fixed.F
        nrecloc = cal_IntDays( myThid )
        spminloc = 0.0
        spmaxloc = 1.0
        spzeroloc = -1.
        localperiod = 86400.

        CALL SEAICE_COST_CONCENTRATION(
     &     nnzbar, smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrarea,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrarea, num_smrarea,
     &     myTime, myIter, myThid )

C read the area dat file and compare against the averaged sst file
        CALL SEAICE_COST_SST(
     &     nnzbar, smrsstbarfile,  smrsstbar, xx_smrsstbar_mean_dummy,
     &             smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrsst,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrsst, num_smrsst,
     &     myTime, myIter, myThid )

C read the area dat file and compare against the averaged salinity file
        CALL SEAICE_COST_SSS(
     &     nnzbar, smrsssbarfile, smrsssbar, xx_smrsssbar_mean_dummy,
     &             smrareabarfile, smrareabar, xx_smrareabar_mean_dummy,
     &     nnzobs, smrareadatfile, smrareadat, mult_smrsss,
     &     nrecloc, modelstartdate, localperiod,
     &     maskC, wsmrarea,
     &     spminloc, spmaxloc, spzeroloc,
     &     objf_smrsss, num_smrsss,
     &     myTime, myIter, myThid )

        print *,' SICD cost smrarea/sst/sss ',objf_smrarea,
     &            objf_smrsst,objf_smrsss
        print *,' SICD num points smrarea/sst/sss ',num_smrarea,
     &            num_smrsst,num_smrsss

C-    End if useSEAICE & useCAL block
      ENDIF

#endif /* ALLOW_COST and ALLOW_CAL and ALLOW_SEAICE_COST_SMR_AREA */
      RETURN
      END
