#include "CTRL_OPTIONS.h"

      subroutine ctrl_cost_driver( myThid )

c     ==================================================================
c     SUBROUTINE ctrl_cost_driver
c     ==================================================================
c
c     ==================================================================
c     SUBROUTINE ctrl_cost_driver
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "ctrl.h"
# include "ctrl_dummy.h"
# include "CTRL_GENARR.h"
#endif

c     == routine arguments ==

      integer myThid

c     == local variables ==

#ifdef ALLOW_CTRL

#ifdef ALLOW_GENTIM2D_CONTROL
      integer startrec
      integer endrec
#endif

#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      integer iarr
      logical dodimensionalcost
      integer k2
      _RS mask2D(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RS mask3D(1-olx:snx+olx,1-oly:sny+oly,Nr,nsx,nsy)
#endif

c     == end of interface ==

c--   Evaluate the individual cost function contributions.


#ifdef ALLOW_GENTIM2D_CONTROL
      do iarr = 1, maxCtrlTim2D

      dodimensionalcost=.FALSE.
      do k2 = 1, maxCtrlProc
       if (xx_gentim2d_preproc(k2,iarr).EQ.'noscaling') then
         dodimensionalcost=.TRUE.
       endif
      enddo

      if (xx_gentim2d_weight(iarr).NE.' ') then
      startrec = ncvarrecstart(300+iarr)
      endrec   = ncvarrecsend(300+iarr)
      do k2 = 1, maxCtrlProc
       if (xx_gentim2d_preproc(k2,iarr).EQ.'replicate') then
         if (xx_gentim2d_preproc_i(k2,iarr).NE.0) then
          endrec=min(endrec,xx_gentim2d_preproc_i(k2,iarr))
         endif
       endif
      enddo

C --- Get appropriate mask for field
      call ctrl_get_mask2D(xx_gentim2d_file(iarr),mask2D,myThid)

      call ctrl_cost_gen2d (
     &     startrec, endrec,
     &     xx_gentim2d_file(iarr),xx_gentim2d_dummy(iarr),
     &     xx_gentim2d_period(iarr),
     &     wgentim2d(1-Olx,1-Oly,1,1,iarr),
     &     dodimensionalcost,
     &     num_gentim2d(1,1,iarr),
     &     objf_gentim2d(1,1,iarr),
#ifdef ECCO_CTRL_DEPRECATED
     &     zeroRL, num_gentim2dm(1,1,iarr), objf_gentim2dm(1,1,iarr),
     &     objf_gentim2dsmoo(1,1,iarr), zeroRL, zeroRL,
#endif /* ECCO_CTRL_DEPRECATED */
     &     mask2D, myThid )
      endif

      enddo
#endif

#ifdef ALLOW_GENARR2D_CONTROL
      do iarr = 1, maxCtrlArr2D

      dodimensionalcost=.FALSE.
      do k2 = 1, maxCtrlProc
       if (xx_genarr2d_preproc(k2,iarr).EQ.'noscaling') then
         dodimensionalcost=.TRUE.
       endif
      enddo

C --- Get appropriate mask for field
      call ctrl_get_mask2D(xx_genarr2d_file(iarr),mask2D,myThid)

      if (xx_genarr2d_weight(iarr).NE.' ') then
      call ctrl_cost_gen2d (
     &     1,1,
     &     xx_genarr2d_file(iarr),xx_genarr2d_dummy(iarr),
     &     zeroRL, wgenarr2d(1-Olx,1-Oly,1,1,iarr),
     &     dodimensionalcost,
     &     num_genarr2d(1,1,iarr), objf_genarr2d(1,1,iarr),
#ifdef ECCO_CTRL_DEPRECATED
     &     zeroRL, num_zero_mean, objf_zero_mean,
     &     objf_zero_smoo, zeroRL, zeroRL,
#endif /* ECCO_CTRL_DEPRECATED */
     &     mask2D, myThid )

      endif

      enddo
#endif


#ifdef ALLOW_GENARR3D_CONTROL
      do iarr = 1, maxCtrlArr3D

      dodimensionalcost=.FALSE.
      do k2 = 1, maxCtrlProc
       if (xx_genarr3d_preproc(k2,iarr).EQ.'noscaling') then
         dodimensionalcost=.TRUE.
       endif
      enddo

C --- Get appropriate mask for field
      call ctrl_get_mask3D(xx_genarr3d_file(iarr),mask3D,myThid)

      if (xx_genarr3d_weight(iarr).NE.' ') then
      call ctrl_cost_gen3d (
     &     xx_genarr3d_file(iarr),xx_genarr3d_dummy(iarr),
     &     wgenarr3d(1-Olx,1-Oly,1,1,1,iarr),
     &     dodimensionalcost,
     &     num_genarr3d(1,1,iarr), objf_genarr3d(1,1,iarr),
     &     mask3D, myThid )
      endif

      enddo
#endif

#endif


      return
      end
