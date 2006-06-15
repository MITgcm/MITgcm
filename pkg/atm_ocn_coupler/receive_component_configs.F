C $ Header: $
C $ Name:   $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE RECEIVE_COMPONENT_CONFIGS
C     /==========================================================\
C     | SUBROUTINE RECEIVE_COMPONENT_CONFIGS                     |
C     | o Rotuine which controls receiving the configurations    |
C     |   of each of the component models.                       |
C     \==========================================================/
      IMPLICIT NONE

C     Data on ocean grid
#include "OCNSIZE.h"
#include "OCNVARS.h"
C     Data on atmos grid
#include "ATMSIZE.h"
#include "ATMVARS.h"

C     Receive ocean config (OCNCONFIG) from ocean
C     component (OCN).
      CALL CPL_RECV_OCN_OCNCONFIG

C     Receive atmos config (ATCONFIG) from atmos.
C     component (ATM).
      CALL CPL_RECV_ATM_ATMCONFIG

      RETURN
      END
