#include "CTRL_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine ctrl_map_ini_ecco( mythid )

c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================
c
c     o Add the temperature and salinity etc. parts of the control vector to
c       the model state and params and update the tile edges.
c     o The control vector is defined in the header file "ctrl.h".
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 23-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     o rewritten: Gael Forget gforget@mit.edu 09-Feb-2008
c              - Introduce a generic routine (ctrl_map_ini_gen.F)
c                handling individual ctrl vector terms
c
c     ==================================================================
c     SUBROUTINE ctrl_map_ini_ecco
c     ==================================================================

      implicit none

c     == routine arguments ==

      integer mythid

      return
      end
