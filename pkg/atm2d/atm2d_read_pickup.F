#include "ctrparam.h"
#include "ATM2D_OPTIONS.h"

C     !INTERFACE:
      SUBROUTINE ATM2D_READ_PICKUP( myThid )
C     *==========================================================*
C     | Read couple flux pickup, which contains fluxes for the   |
C     | initial ocean step and seaice growth/melt step. Called   |
C     | from the "normal" place (atm2d_init_vars).               |
C     *==========================================================*
        IMPLICIT NONE

#include "ATMSIZE.h"
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     === Global SeaIce Variables ===
#include "THSICE_VARS.h"

C     === Atmos/Ocean/Seaice Interface Variables ===
#include "ATM2D_VARS.h"


C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     LOCAL VARIABLES:
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec

      IF (startTime.NE.baseTime .OR. nIter0.NE.0) THEN

        prec=precFloat64

        WRITE(fn,'(A,I10.10)') 'pickup_atm2d.',nIter0

        CALL MDSREADFIELD(fn,prec,'RL',1,pass_slp, 1, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_qnet, 2, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_solarnet, 3, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_fu, 4, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_fv, 5, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_precip, 6, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_evap, 7, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_runoff, 8, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_wspeed, 9, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_pCO2, 10, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_sIceLoad, 11, myThid)

        CALL MDSREADFIELD(fn,prec,'RL',1,sHeating, 12, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,flxCndBt, 13, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,pass_prcAtm, 14, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,snowPrc, 15, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,icFrwAtm, 16, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,icFlxSw   , 17, myThid)
        CALL MDSREADFIELD(fn,prec,'RL',1,siceAlb  , 18, myThid)

      ENDIF

      RETURN
      END

