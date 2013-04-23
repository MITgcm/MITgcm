C $Header: /u/gcmpack/MITgcm/pkg/exf/exf_check_range.F,v 1.31 2013/04/23 19:04:33 dimitri Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

      SUBROUTINE EXF_CHECK_RANGE( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE EXF_CHECK_RANGE
c     ==================================================================
c
      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "FFIELDS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_CONSTANTS.h"
#include "EXF_FIELDS.h"
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

c     jtlo = mybylo(mythid)
c     jthi = mybyhi(mythid)
c     itlo = mybxlo(mythid)
c     ithi = mybxhi(mythid)
C--   Only master thread can safely write directly to standard output:
      _BARRIER
      _BEGIN_MASTER( myThid )
      jtlo = 1
      jthi = nSy
      itlo = 1
      ithi = nSx

C Change checking range because some atmospheric fields will
C not always have valid values in the tile edges.
C      jmin = 1-oly
C      jmax = sny+oly
C      imin = 1-olx
C      imax = snx+olx
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      do bj = jtlo,jthi
       do bi = itlo,ithi

        do j = jmin,jmax
         do i = imin,imax
c
c     Heat flux.
          if ( ( hflux(i,j,bi,bj) .GT. 1600. .OR.
     &         hflux(i,j,bi,bj) .LT. -500. ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: hflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, hflux(i,j,bi,bj)
           exferr = 1
          endif
c
c     Salt flux.
          if ( ABS(sflux(i,j,bi,bj)) .GT. 1.E-6 .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: sflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, sflux(i,j,bi,bj)
           exferr = 1
          endif
c
c     Zonal wind stress.
          if ( ABS(ustress(i,j,bi,bj)) .GT. 2.7 .AND.
     &         maskW(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: ustress out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, ustress(i,j,bi,bj)
           exferr = 1
          endif
c
c     Meridional wind stress.
          if ( ABS(vstress(i,j,bi,bj)) .GT. 2.3 .AND.
     &         maskS(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: vstress out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, vstress(i,j,bi,bj)
           exferr = 1
          endif
c
      IF ( useAtmWind ) THEN
c     zonal wind speed
          if ( ABS(uwind(i,j,bi,bj)) .GT. 100. .AND.
     &         maskW(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: uwind out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, uwind(i,j,bi,bj)
           exferr = 1
          endif
c
c     zonal wind speed
          if ( ABS(vwind(i,j,bi,bj)) .GT. 100. .AND.
     &         maskS(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: vwind out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, vwind(i,j,bi,bj)
           exferr = 1
          endif
      ENDIF
c
c     wind speed modulus
          if ( ( wspeed(i,j,bi,bj) .LT. 0. .OR.
     &         wspeed(i,j,bi,bj) .GT. 100. ) .AND.
     &         maskS(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: wspeed out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, wspeed(i,j,bi,bj)
           exferr = 1
          endif
          
#ifdef ALLOW_ATM_TEMP
c     2-m air temperature
          if ( (atemp(i,j,bi,bj) .LT. 183 .OR.
     &         atemp(i,j,bi,bj) .GT. 343 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(2A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: atemp + exf_offset_atemp ',
     &          'out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, atemp(i,j,bi,bj)
           exferr = 1
          endif
c
c     2-m specific humidity
          if ( (aqh(i,j,bi,bj) .LT. 0. .OR.
     &         aqh(i,j,bi,bj) .GT. 0.1 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: aqh out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, aqh(i,j,bi,bj)
           exferr = 1
          endif
c     
c     precipitation rate
          if ( (precip(i,j,bi,bj) .LT. 0. .OR.
     &         precip(i,j,bi,bj) .GT. 2.E-6 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: precip out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, precip(i,j,bi,bj)
           exferr = 1
          endif
c     
c     snow
          if ( (snowprecip(i,j,bi,bj) .LT. 0. .OR.
     &         snowprecip(i,j,bi,bj) .GT. 2.E-6 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(2A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: snowprecip out of range ', 
     &          'for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, snowprecip(i,j,bi,bj)
           exferr = 1
          endif
#endif

#ifdef SHORTWAVE_HEATING
c     Short wave radiative flux.
          if ( (swflux(i,j,bi,bj) .GT. 1. .OR.
     &         swflux(i,j,bi,bj) .LT. -1000. ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: swflux out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, swflux(i,j,bi,bj)
           exferr = 1
          endif
#endif

#ifdef ALLOW_RUNOFF
c     Runoff.
          if ( (runoff(i,j,bi,bj) .LT. 0. .OR.
     &         runoff(i,j,bi,bj) .GT. 1.E-6 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: runoff out of range for bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, runoff(i,j,bi,bj)
           write(standardmessageunit,'(A)')
     &          'Please note that input units for runoff are'
           write(standardmessageunit,'(A)')
     &          'm/s not m/yr.  If input file is in m/yr, set'
           write(standardmessageunit,'(A)')
     &          'exf_inscal_runoff=3.170979198E-8'
           write(standardmessageunit,'(A)')
     &          'in the data.exf input file.'
           exferr = 1
          endif
# ifdef ALLOW_RUNOFTEMP
c     Runoff temperature.
          if ( (runoftemp(i,j,bi,bj) .LT. -2. .OR.
     &         runoff(i,j,bi,bj) .GT. 36 ) .AND.
     &         maskC(i,j,1,bi,bj) .NE. 0. ) then
           write(standardmessageunit,'(A,5(1X,I6),2X,D22.15)')
     &          'EXF WARNING: runoftemp out of range at bi,bj,i,j,it= ',
     &          bi, bj, i, j, myiter, runoff(i,j,bi,bj)
           exferr = 1
          endif
# endif /* ALLOW_RUNOFTEMP */
#endif /* ALLOW_RUNOFF */
         enddo
        enddo
c     
       enddo
      enddo

      if ( exferr .NE. 0 ) then
       write(standardmessageunit,'(A)')
     &      'EXF WARNING: If you think these values are OK '
       write(standardmessageunit,'(A)')
     &      'EXF WARNING: then set useExfCheckRange=.FALSE.'
       STOP 'ABNORMAL END: S/R EXF_CHECK_RANGE'
      endif
      _END_MASTER( myThid )

      RETURN
      END
