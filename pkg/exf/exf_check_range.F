c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check_range.F,v 1.1 2004/02/26 19:34:34 heimbach Exp $

#include "EXF_OPTIONS.h"

      subroutine exf_check_range( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE exf_check_range
c     ==================================================================
c
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FFIELDS.h"
#include "GRID.h"

#include "exf_param.h"
#include "exf_constants.h"
#include "exf_fields.h"
#include "exf_clim_fields.h"
c     == routine arguments ==

c     mythid - thread number for this instance of the routine.

      integer mytime, myiter, mythid

c     == local variables ==

      integer bi,bj
      integer i,j
      integer jtlo
      integer jthi
      integer itlo
      integer ithi
      integer jmin
      integer jmax
      integer imin
      integer imax

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1-oly
      jmax = sny+oly
      imin = 1-olx
      imax = snx+olx

      do bj = jtlo,jthi
        do bi = itlo,ithi

          do j = jmin,jmax
            do i = imin,imax
c
c             Heat flux.
              if ( ABS(hflux(i,j,bi,bj)) .GT. 1000. )
     &           print *, 'EXF WARNING: hflux out of range for i,j= ',
     &           hflux(i,j,bi,bj)
c
c             Salt flux.
              if ( ABS(sflux(i,j,bi,bj)) .GT. 1.E-6 )
     &           print *, 'EXF WARNING: sflux out of range for i,j= ',
     &           sflux(i,j,bi,bj)
c
c             Zonal wind stress.
              if ( ABS(ustress(i,j,bi,bj)) .GT. 2. )
     &           print *, 'EXF WARNING: ustress out of range for i,j= ',
     &           ustress(i,j,bi,bj)
c
c             Meridional wind stress.
              if ( ABS(vstress(i,j,bi,bj)) .GT. 2. )
     &           print *, 'EXF WARNING: vstress out of range for i,j= ',
     &           vstress(i,j,bi,bj)
c
#ifdef ALLOW_ATM_WIND
c             zonal wind speed
              if ( ABS(uwind(i,j,bi,bj)) .GT. 40. )
     &           print *, 'EXF WARNING: uwind out of range for i,j= ',
     &           uwind(i,j,bi,bj)
c
c             zonal wind speed
              if ( ABS(vwind(i,j,bi,bj)) .GT. 40. )
     &           print *, 'EXF WARNING: vwind out of range for i,j= ',
     &           vwind(i,j,bi,bj)
#endif

#ifdef ALLOW_ATM_TEMP
c             2-m air temperature
              if ( atemp(i,j,bi,bj) .LT. 223 .OR.
     &             atemp(i,j,bi,bj) .GT. 323 )
     &           print *, 'EXF WARNING: atemp out of range for i,j= ',
     &           atemp(i,j,bi,bj)
c
c             2-m specific humidity
              if ( aqh(i,j,bi,bj) .LT. 0. .OR.
     &             aqh(i,j,bi,bj) .GT. 1.E-6 )
     &           print *, 'EXF WARNING: aqh out of range for i,j= ',
     &           aqh(i,j,bi,bj)
c
c             precipitation rate
              if ( precip(i,j,bi,bj) .LT. 0. .OR.
     &             precip(i,j,bi,bj) .GT. 0.1 )
     &           print *, 'EXF WARNING: precip out of range for i,j= ',
     &           precip(i,j,bi,bj)
#endif

#ifdef SHORTWAVE_HEATING
c             Short wave radiative flux.
              if ( swflux(i,j,bi,bj) .GT. 0. .OR.
     &             swflux(i,j,bi,bj) .LT. -500. )
     &           print *, 'EXF WARNING: swflux out of range for i,j= ',
     &           swflux(i,j,bi,bj)
#endif

            enddo
          enddo
c
        enddo
      enddo

      end
