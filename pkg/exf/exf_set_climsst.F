c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_climsst.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_set_climsst( 
     O                            climsst
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_set_climsst
c     ==================================================================
c
c     o  Get the current climatological sea surface salinity field.
c
c     started: Christian Eckert eckert@mit.edu 27-Aug-1999
c
c     changed: Christian Eckert eckert@mit.edu 11-Jan-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c              Christian Eckert eckert@mit.edu 12-Feb-2000
c
c              - Changed Routine names (package prefix: exf_)
c
c     ==================================================================
c     SUBROUTINE exf_set_climsst
c     ==================================================================

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_clim_param.h"

c     == routine arguments ==

      _RL     climsst(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_CLIMSST_RELAXATION

c     == local variables ==

      common /exf_csst_r/ climsst0, climsst1
      _RL climsst0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsst1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j

c     == end of interface ==


cph(
cph   Routine exf_GetSSTClimRec has standard cyclic forcing
cph   assuming 12 monthly climatological records on file
cph   with irec=1: Jan, ... irec=12: Dec
cph   Will be included as optional choice of to the above.
cph
cph      call exf_GetSSTClimRec(
cph     O                        fac, first, changed,
cph     O                        count0,count1,
cph     I                        mytime, myiter, mythid
cph     &                            )
cph)


c     get record numbers and interpolation factor for climsst
      call exf_GetFFieldRec(
     I                       climsststartdate, climsstperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        call mdsreadfield( climsstfile, exf_clim_iprec
     &                   , exf_clim_yftype, 1
     &                   , climsst1, count0, mythid
     &                   )
        if (exf_clim_yftype .eq. 'RL') then
           call exf_filter_rl( climsst1, climsstmask, mythid )
        else
           call exf_filter_rs( climsst1, climsstmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields( climsst0, climsst1, mythid )

        call mdsreadfield( climsstfile, exf_clim_iprec
     &                   , exf_clim_yftype, 1
     &                   , climsst1, count1, mythid
     &                   )
        if (exf_clim_yftype .eq. 'RL') then
           call exf_filter_rl( climsst1, climsstmask, mythid )
        else
           call exf_filter_rs( climsst1, climsstmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx

c             Set to freezing temperature if less
              if (climsst0(i,j,bi,bj) .lt. climtempfreeze) then
                climsst0(i,j,bi,bj) = climtempfreeze
              endif
              if (climsst1(i,j,bi,bj) .lt. climtempfreeze) then
                climsst1(i,j,bi,bj) = climtempfreeze
              endif

c             Interpolate linearly onto the current time.
              climsst(i,j,bi,bj) = fac          *climsst0(i,j,bi,bj)+
     &                          (exf_one - fac) *climsst1(i,j,bi,bj)

            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMSST_RELAXATION */

      end


      subroutine exf_init_climsst(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_climsst
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_climsst
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_fields.h"
#include "exf_param.h"
#include "exf_clim_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_CLIMSST_RELAXATION

c     == local variables ==

      common /exf_csst_r/ climsst0, climsst1
      _RL climsst0(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL climsst1(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)

      integer bi, bj
      integer i, j

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do j = 1, sny
            do i = 1, snx
              climsst (i,j,bi,bj) = 0. _d 0
              climsst0(i,j,bi,bj) = 0. _d 0
              climsst1(i,j,bi,bj) = 0. _d 0
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMSSST_RELAXATION */

      end
