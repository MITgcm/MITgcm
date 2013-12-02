C $Header: /u/gcmpack/MITgcm/pkg/atm_ocn_coupler/exch_component_configs.F,v 1.1 2013/12/02 21:55:06 jmc Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP 0
C !ROUTINE: EXCH_COMPONENT_CONFIGS

C !INTERFACE:
      SUBROUTINE EXCH_COMPONENT_CONFIGS( msgUnit )

C !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE EXCH_COMPONENT_CONFIGS
C     | o Routine which controls exchange of component model
C     |   configuration information to other components.
C     *==========================================================*

C !USES:
      IMPLICIT NONE
C     == Global variables ==
C     Data on ocean grid
c#include "OCNSIZE.h"
c#include "OCNVARS.h"
C     Data on atmos grid
c#include "ATMSIZE.h"
c#include "ATMVARS.h"

C !INPUT/OUTPUT PARAMETERS:
C     msgUnit   :: log-file I/O unit
      INTEGER msgUnit

C !LOCAL VARIABLES:
      LOGICAL errFlag
      CHARACTER*70 errMsg
CEOP

      errFlag = .FALSE.
      errMsg  = ' '

C--   1) collect configs from all components

C     Receive ocean config (OCNCONFIG) from ocean component (OCN).
      CALL CPL_RECV_OCN_OCNCONFIG

C     Receive atmos config (ATCONFIG) from atmos.  component (ATM).
      CALL CPL_RECV_ATM_ATMCONFIG

C--   2) send to all components the configs of the other component

C     Send atmos. config (ATMCONFIG) to ocean component (OCN).
      CALL CPL_SEND_OCN_ATMCONFIG

C     Send ocean config (OCNCONFIG) to atmos. component (ATM).
      CALL CPL_SEND_ATM_OCNCONFIG

C--   3) define length of the run (number of coupler time-step)
C        and check for consistent Coupler config (vs components)
      CALL CPL_CHECK_CPLCONFIG(
     I                          msgUnit,
     U                          errFlag, errMsg )

C--   All procs in World check for error and stop if any
      CALL MITCPLR_ALL_CHECK( errFlag, errMsg )

      RETURN
      END
