c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_climsalt.F,v 1.1 2001/05/14 22:08:41 heimbach Exp $

#include "EXF_CPPOPTIONS.h"


      subroutine exf_set_climsalt( 
     O                            climsalt
     I                          , mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_set_climsalt
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
c     SUBROUTINE exf_set_climsalt
c     ==================================================================

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_clim_param.h"

c     == routine arguments ==

      _RL     climsalt(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_CLIMSALT_RELAXATION

c     == local variables ==

      common /exf_csalt_r/ climsalt0, climsalt1
      _RL climsalt0(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climsalt1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j, k

c     == end of interface ==

c     get record numbers and interpolation factor for climsalt
      call exf_GetFFieldRec(
     I                       climsaltstartdate, climsaltperiod
     O                     , fac, first, changed
     O                     , count0, count1
     I                     , mycurrenttime, mycurrentiter, mythid
     &                     )

      if ( first ) then
        call mdsreadfield( climsaltfile, exf_clim_iprec
     &                   , exf_clim_yftype, nr
     &                   , climsalt1, count0, mythid
     &                   )
        if (exf_clim_yftype .eq. 'RL') then
           call exf_filter_rl( climsalt1, climsaltmask, mythid )
        else
           call exf_filter_rs( climsalt1, climsaltmask, mythid )
        end if
      endif

      if (( first ) .or. ( changed )) then
        call exf_SwapFFields_3d( climsalt0, climsalt1, mythid )

        call mdsreadfield( climsaltfile, exf_clim_iprec
     &                   , exf_clim_yftype, nr
     &                   , climsalt1, count1, mythid
     &                   )
        if (exf_clim_yftype .eq. 'RL') then
           call exf_filter_rl( climsalt1, climsaltmask, mythid )
        else
           call exf_filter_rs( climsalt1, climsaltmask, mythid )
        end if
      endif

c     Loop over tiles.
      do bj = mybylo(mythid),mybyhi(mythid)
        do bi = mybxlo(mythid),mybxhi(mythid)
          do k = 1,nr
            do j = 1-oly,sny+oly
              do i = 1-olx,snx+olx

c             Set to freezing salterature if less
              if (climsalt0(i,j,bi,bj) .lt. climsaltfreeze) then
                climsalt0(i,j,bi,bj) = climsaltfreeze
              endif
              if (climsalt1(i,j,bi,bj) .lt. climsaltfreeze) then
                climsalt1(i,j,bi,bj) = climsaltfreeze
              endif

c             Interpolate linearly onto the current time.
              climsalt(i,j,k,bi,bj) = 
     &             fac             *climsalt0(i,j,k,bi,bj)+
     &             (exf_one - fac) *climsalt1(i,j,k,bi,bj)

              enddo
            enddo
          enddo
        enddo
      enddo
        
#endif /* ALLOW_CLIMSALT_RELAXATION */

      end


      subroutine exf_init_climsalt(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_climsalt
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_climsalt
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "exf_fields.h"
#include "exf_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_CLIMSALT_RELAXATION

c     == local variables ==

      common /exf_csalt_r/ climsalt0, climsalt1
      _RL climsalt0(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      _RL climsalt1(1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)

      integer bi, bj
      integer i, j, k

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do k=1,nr
            do j = 1, sny
              do i = 1, snx
                 climsalt (i,j,k,bi,bj) = 0. _d 0
                 climsalt0(i,j,k,bi,bj) = 0. _d 0
                 climsalt1(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMSALT_RELAXATION */

      end
