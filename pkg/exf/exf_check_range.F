c $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check_range.F,v 1.22 2005/06/14 23:21:05 heimbach Exp $

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

      _RL mytime
      integer myiter, mythid

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
      integer exferr

c     == end of interface ==

      exferr = 0

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
              if ( ( hflux(i,j,bi,bj) .GT. 1600. .OR.
     &               hflux(i,j,bi,bj) .LT. -500. ) .AND.
     &              hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: hflux out of range for i,j= ',
     &                i, j, hflux(i,j,bi,bj)
                 exferr = 1
              endif
c
c             Salt flux.
              if ( ABS(sflux(i,j,bi,bj)) .GT. 1.E-6 .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: sflux out of range for i,j= ',
     &                i, j, sflux(i,j,bi,bj)
                 exferr = 1
              endif
c
c             Zonal wind stress.
              if ( ABS(ustress(i,j,bi,bj)) .GT. 2.7 .AND.
     &             hFacW(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: ustress out of range for i,j= ',
     &                i, j, ustress(i,j,bi,bj)
                 exferr = 1
              endif
c
c             Meridional wind stress.
              if ( ABS(vstress(i,j,bi,bj)) .GT. 2.3 .AND.
     &             hFacS(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: vstress out of range for i,j= ',
     &                i, j, vstress(i,j,bi,bj)
                 exferr = 1
              endif
c
#ifdef ALLOW_ATM_WIND
c             zonal wind speed
              if ( ABS(uwind(i,j,bi,bj)) .GT. 100. .AND.
     &             hFacW(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: uwind out of range for i,j= ',
     &                i, j, uwind(i,j,bi,bj)
                 exferr = 1
              endif
c
c             zonal wind speed
              if ( ABS(vwind(i,j,bi,bj)) .GT. 100. .AND.
     &             hFacS(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: vwind out of range for i,j= ',
     &                i, j, vwind(i,j,bi,bj)
                 exferr = 1
              endif
#endif

#ifdef ALLOW_ATM_TEMP
c             2-m air temperature
              if ( (atemp(i,j,bi,bj) .LT. 183 .OR.
     &             atemp(i,j,bi,bj) .GT. 343 ) .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(2A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: atemp + exf_offset_atemp ',
     &                'out of range for i,j= ',
     &                i, j, atemp(i,j,bi,bj)
                 exferr = 1
              endif
c
c             2-m specific humidity
              if ( (aqh(i,j,bi,bj) .LT. 0. .OR.
     &             aqh(i,j,bi,bj) .GT. 0.1 ) .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: aqh out of range for i,j= ',
     &                i, j, aqh(i,j,bi,bj)
                 exferr = 1
              endif
c
c             precipitation rate
              if ( (precip(i,j,bi,bj) .LT. 0. .OR.
     &             precip(i,j,bi,bj) .GT. 2.E-6 ) .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: precip out of range for i,j= ',
     &                i, j, precip(i,j,bi,bj)
                 exferr = 1
              endif
#endif

#ifdef SHORTWAVE_HEATING
c             Short wave radiative flux.
              if ( (swflux(i,j,bi,bj) .GT. 1. .OR.
     &             swflux(i,j,bi,bj) .LT. -1000. ) .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: swflux out of range for i,j= ',
     &                i, j, swflux(i,j,bi,bj)
                 exferr = 1
              endif
#endif

#ifdef ALLOW_RUNOFF
c             Runoff.
              if ( (runoff(i,j,bi,bj) .LT. 0. .OR.
     &             runoff(i,j,bi,bj) .GT. 1.E-6 ) .AND.
     &             hFacC(i,j,1,bi,bj) .NE. 0. ) then
                 write(standardmessageunit,'(A,2(2X,I5),2X,D22.15)')
     &                'EXF WARNING: runoff out of range for i,j= ',
     &                i, j, runoff(i,j,bi,bj)
                 write(standardmessageunit,'(A)')
     &                'Please note that input units for runoff are'
                 write(standardmessageunit,'(A)')
     &                'm/s not m/yr.  If input file is in m/yr, set'
                 write(standardmessageunit,'(A)')
     &                'exf_inscal_runoff=3.170979198E-8'
                 write(standardmessageunit,'(A)')
     &                'in the data.exf input file.'
                 exferr = 1
              endif
#endif

              if ( exferr .NE. 0 ) then
               write(standardmessageunit,'(A)')
     &                'EXF WARNING: If you think these values are OK '
               write(standardmessageunit,'(A)')
     &                'EXF WARNING: then set useExfCheckRange=.FALSE.'
               STOP 'in S/R exf_check_range'
              endif

            enddo
          enddo
c
        enddo
      enddo

      end
