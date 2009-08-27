C $Header: /u/gcmpack/MITgcm/pkg/atm2d/atm2d_read_pickup.F,v 1.3 2009/08/27 18:00:01 jmc Exp $
C $Name:  $

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

        CALL READ_REC_3D_RL(fn,prec,1,pass_slp,      1, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_qnet,     2, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_solarnet, 3, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_fu,       4, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_fv,       5, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_precip,   6, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_evap,     7, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_runoff,   8, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_wspeed,   9, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_pCO2,    10, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_sIceLoad,11, nIter0, myThid)

        CALL READ_REC_3D_RL(fn,prec,1,sHeating,     12, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,flxCndBt,     13, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,pass_prcAtm,  14, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,snowPrc,      15, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,icFrwAtm,     16, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,icFlxSw,      17, nIter0, myThid)
        CALL READ_REC_3D_RL(fn,prec,1,siceAlb,      18, nIter0, myThid)

      ENDIF

      RETURN
      END

