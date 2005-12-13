c $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_set_climtemp.F,v 1.10 2005/12/13 19:46:46 heimbach Exp $

#include "EXF_OPTIONS.h"


      subroutine exf_set_climtemp( 
     O                            mycurrenttime
     I                          , mycurrentiter
     I                          , mythid
     &                         )

c     ==================================================================
c     SUBROUTINE exf_set_climtemp
c     ==================================================================
c
c     o  Get the current climatological sea surface salinity field.
c
c     started: Christian Eckert eckert@mit.edu 27-Aug-1999
c     changed: Christian Eckert eckert@mit.edu 11-Jan-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c              Christian Eckert eckert@mit.edu 12-Feb-2000
c              - Changed Routine names (package prefix: exf_)
c     changed: heimbach@mit.edu 08-Feb-2002
c     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
c
c     ==================================================================
c     SUBROUTINE exf_set_climtemp
c     ==================================================================

      implicit none

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_clim_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      _RL     mycurrenttime
      integer mycurrentiter
      integer mythid

#ifdef ALLOW_CLIMTEMP_RELAXATION

c     == local variables ==

      logical first, changed
      integer count0, count1
      _RL     fac

      integer bi, bj
      integer i, j, k
      integer year0, year1

c     == end of interface ==

      if ( climtempfile .NE. ' ' ) then

         if ( climtempperiod .EQ. 0 ) then

c     record numbers are assumed 1 to 12 corresponding to
c     Jan. through Dec.
            call cal_GetMonthsRec(
     O           fac, first, changed,
     O           count0, count1,
     I           mycurrenttime, mycurrentiter, mythid
     &           )

         else

c     get record numbers and interpolation factor for climtemp
            call exf_GetFFieldRec(
     I           climtempstartdate, climtempperiod
     I           , climtempstartdate1, climtempstartdate2
     I           , .false.
     O           , fac, first, changed
     O           , count0, count1, year0, year1
     I           , mycurrenttime, mycurrentiter, mythid
     &           )

         endif

         if ( first ) then
            call mdsreadfield( climtempfile, exf_clim_iprec
     &           , exf_clim_yftype, nr
     &           , climtemp1, count0, mythid
     &           )
            do k = 1, Nr
               if (exf_clim_yftype .eq. 'RL') then
                  call exf_filter_rl( 
     &                 climtemp1(1-Olx,1-Oly,k,1,1),
     &                 climtempmask, mythid )
               else
                  call exf_filter_rs( 
     &                 climtemp1(1-Olx,1-Oly,k,1,1), 
     &                 climtempmask, mythid )
               end if
            enddo
         endif

         if (( first ) .or. ( changed )) then
            call exf_SwapFFields_3d( climtemp0, climtemp1, mythid )

            call mdsreadfield( climtempfile, exf_clim_iprec
     &           , exf_clim_yftype, nr
     &           , climtemp1, count1, mythid
     &           )
            do k = 1, Nr
               if (exf_clim_yftype .eq. 'RL') then
                  call exf_filter_rl( 
     &                 climtemp1(1-Olx,1-Oly,k,1,1), 
     &                 climtempmask, mythid )
               else
                  call exf_filter_rs( 
     &                 climtemp1(1-Olx,1-Oly,k,1,1), 
     &                 climtempmask, mythid )
               end if
            enddo
         endif

c     Loop over tiles.
         do bj = mybylo(mythid),mybyhi(mythid)
            do bi = mybxlo(mythid),mybxhi(mythid)
               do k = 1,nr
                  do j = 1,sny
                     do i = 1,snx

c     Set to freezing temperature if less
                        if (climtemp0(i,j,k,bi,bj) .lt. climtempfreeze)
     &                       climtemp0(i,j,k,bi,bj) = climtempfreeze
                        if (climtemp1(i,j,k,bi,bj) .lt. climtempfreeze)
     &                       climtemp1(i,j,k,bi,bj) = climtempfreeze

c     Interpolate linearly onto the current time.
                        climtemp(i,j,k,bi,bj) = 
     &                       fac * climtemp0(i,j,k,bi,bj) +
     &                       (exf_one - fac) * climtemp1(i,j,k,bi,bj)

                     enddo
                  enddo
               enddo
            enddo
         enddo

      endif
        
#endif /* ALLOW_CLIMTEMP_RELAXATION */

      end


      subroutine exf_init_climtemp(
     I                             mythid
     &                           )

c     ==================================================================
c     SUBROUTINE exf_init_climtemp
c     ==================================================================
c
c     o 
c
c     started: Ralf.Giering@FastOpt.de 25-Mai-2000
c
c     ==================================================================
c     SUBROUTINE exf_init_climtemp
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "exf_fields.h"
#include "exf_param.h"
#include "exf_clim_fields.h"

c     == routine arguments ==

      integer mythid

#ifdef ALLOW_CLIMTEMP_RELAXATION

c     == local variables ==

      integer bi, bj
      integer i, j, k

c     == end of interface ==

      do bj = mybylo(mythid), mybyhi(mythid)
        do bi = mybxlo(mythid), mybxhi(mythid)
          do k=1,nr
            do j = 1, sny
              do i = 1, snx
                 climtemp (i,j,k,bi,bj) = tRef(k)
                 climtemp0(i,j,k,bi,bj) = 0. _d 0
                 climtemp1(i,j,k,bi,bj) = 0. _d 0
              enddo
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_CLIMTEMP_RELAXATION */

      end
