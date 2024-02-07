#include "CTRL_OPTIONS.h"

      subroutine ctrl_cost_driver( myThid )

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
# include "CTRL.h"
# include "CTRL_DUMMY.h"
# include "CTRL_GENARR.h"
#endif

c     == routine arguments ==

      integer myThid

c     == local variables ==

#ifdef ALLOW_CTRL

      integer ivar
#ifdef ALLOW_GENTIM2D_CONTROL
      integer startrec
      integer endrec
#endif

#if ( defined ALLOW_GENTIM2D_CONTROL \
   || defined ALLOW_GENARR2D_CONTROL \
   || defined ALLOW_GENARR3D_CONTROL )
      integer iarr
      logical dodimensionalcost
      integer k2
#endif
#if ( defined ALLOW_GENTIM2D_CONTROL \
   || defined ALLOW_GENARR2D_CONTROL )
      _RS mask2D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_GENARR3D_CONTROL
      _RS mask3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#endif

c     == end of interface ==

c--   Evaluate the individual cost function contributions.

      do ivar = 1, maxcvars
#ifdef ALLOW_GENTIM2D_CONTROL
       if ( ncvartype(ivar) .EQ. 'Tim2D' ) then
        iarr = ncvarindex(ivar)

        dodimensionalcost=.FALSE.
        do k2 = 1, maxCtrlProc
         if (xx_gentim2d_preproc(k2,iarr).EQ.'noscaling') then
          dodimensionalcost=.TRUE.
         endif
        enddo

        if (xx_gentim2d_weight(iarr).NE.' ') then
         startrec = ncvarrecstart(ivar)
         endrec   = ncvarrecsend(ivar)
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
     &     wgentim2d(1-OLx,1-OLy,1,1,iarr),
     &     dodimensionalcost,
     &     num_gentim2d(1,1,iarr),
     &     objf_gentim2d(1,1,iarr),
     &     mask2D, myThid )
        endif

       endif
#endif

#ifdef ALLOW_GENARR2D_CONTROL
       if ( ncvartype(ivar) .EQ. 'Arr2D' ) then
        iarr = ncvarindex(ivar)

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
     &     zeroRL, wgenarr2d(1-OLx,1-OLy,1,1,iarr),
     &     dodimensionalcost,
     &     num_genarr2d(1,1,iarr), objf_genarr2d(1,1,iarr),
     &     mask2D, myThid )
        endif

       endif
#endif

#ifdef ALLOW_GENARR3D_CONTROL
       if ( ncvartype(ivar) .EQ. 'Arr3D' ) then
        iarr = ncvarindex(ivar)

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
     &     wgenarr3d(1-OLx,1-OLy,1,1,1,iarr),
     &     dodimensionalcost,
     &     num_genarr3d(1,1,iarr), objf_genarr3d(1,1,iarr),
     &     mask3D, myThid )
        endif

       endif
#endif
      enddo
#endif /* ALLOW_CTRL */

      return
      end
