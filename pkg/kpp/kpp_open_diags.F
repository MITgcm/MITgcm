C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_open_diags.F,v 1.4 2001/03/06 17:58:15 jmc Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_OPEN_DIAGS( myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_OPEN_DIAGS                                |
C     | o Initialize I/O variables for diagnostics               |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "KPP_PARAMS.h"
#include "KPP_DIAGS.h"

C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid

#ifdef ALLOW_KPP

C     == Local variables ==

      _BEGIN_MASTER( myThid )

C     Initialize record numbers in KPP_DIAGS
      kpp_drctrec=1
#ifdef ALLOW_TIMEAVE
      kpp_drctrecTave=1
#endif

      _END_MASTER( myThid )
      _BARRIER

#endif /* ALLOW_KPP */

      RETURN
      END
