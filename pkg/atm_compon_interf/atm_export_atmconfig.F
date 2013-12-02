C $Header: /u/gcmpack/MITgcm/pkg/atm_compon_interf/atm_export_atmconfig.F,v 1.4 2013/12/02 22:13:23 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: ATM_EXPORT_ATMCONFIG

C !INTERFACE:
      SUBROUTINE ATM_EXPORT_ATMCONFIG(
     U                      errFlag,
     I                      landMask, myThid )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE ATM_EXPORT_ATMCONFIG
C     | o Routine for exporting atmos. config to coupling level.
C     *==========================================================*
C     | This version talks to the MIT Coupler. It uses the MIT
C     | Coupler "checkpoint1" library calls.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "ATMIDS.h"
c#include "ATMCPL.h"
#include "CPL_PARAMS.h"

C !INPUT/OUTPUT PARAMETERS:
C     errFlag  :: logical flag to report an error
C     landMask :: land / sea mask (=1 : full land; =0 : full ocean grid cell)
C     myThid   :: Thread number for this instance of the routine
      LOGICAL errFlag
      _RL landMask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

C !LOCAL VARIABLES:
C     msgBuf     :: Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER tmpFld(1)
CEOP

C     Send my configuration information to the coupler
      _BARRIER
      _BEGIN_MASTER( myThid )

C     o Send number of coupler time-steps to do for this run
      IF ( MOD( nTimeSteps, cplSendFrq_iter ).NE.0 ) THEN
        errFlag = .TRUE.
        WRITE(msgBuf,'(2A)') 'ATM_EXPORT_ATMCONFIG: ',
     &   '=> Length of run not a multiple of cpl_atmSendFrq'
        CALL PRINT_ERROR( msgBuf, myThid )
      ENDIF
      tmpFld(1) = nTimeSteps/cplSendFrq_iter
      CALL COMPSEND_I4VEC( 'nCouplingSteps', 1, tmpFld )

C     o Send atmos. land/sea mask
      CALL COMPSEND_R8TILES(
     I              atmLandName, sNx, OLx, sNy, OLy, 1, nSx, nSy,
     I              landMask )

      _END_MASTER( myThid )
      _BARRIER

      RETURN
      END
