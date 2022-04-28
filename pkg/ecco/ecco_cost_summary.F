#include "ECCO_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif


      subroutine ecco_cost_summary( mythid )

c     ==================================================================
c     SUBROUTINE ecco_cost_summary
c     ==================================================================
c
c     o Summarize the cost function related parts of the ECCO release.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE ecco_cost_summary
c     ==================================================================

      implicit none

c     == global variables ==

c     == routine arguments ==

      integer mythid

      RETURN
      END
