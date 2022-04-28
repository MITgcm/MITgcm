#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ctrl_map_ini
C     !INTERFACE:
      subroutine ctrl_map_ini( mythid )

C     !DESCRIPTION: \bv
c     *=================================================================
c     | SUBROUTINE ctrl_map_ini
c     | Add the temperature, salinity, and diffusivity parts of the
c     | control vector to the model state and update the tile halos.
c     | The control vector is defined in the header file "ctrl.h".
c     *=================================================================
C     \ev

C     !USES:
      implicit none

C     !INPUT/OUTPUT PARAMETERS:
c     == routine arguments ==
      integer mythid

      RETURN
      END
