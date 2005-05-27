C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_trans_zonal.F,v 1.1 2005/05/27 22:10:27 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_trans_zonal( mythid )

c     ==================================================================
c     SUBROUTINE cost_trans_zonal
c     ==================================================================
c
c     o Compute zonal transports. 
c
c     ==================================================================
c     SUBROUTINE cost_trans_zonal
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "optim.h"
#include "cost.h"
#include "ecco_cost.h"
#include "ctrl_dummy.h"

c     == routine arguments ==

      integer mythid

cph#ifdef ALLOW_COST_TRANSPORT

c     == local variables ==

      integer nsect
      parameter ( nsect = 5 )

      integer isect
      integer kmin(nsect),kmax(nsect)
c
      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer irec
      integer il
      integer funit

      character*(80) fnameout
      character*(80) fnametheta
      character*(80) fnamesalt
      character*(80) fnameuvel
      character*(80) fnamevvel
      character*(MAX_LEN_MBUF) msgbuf

      _RL        p5
      parameter( p5 = 0.5 )

      _RL dummy
      _RL del_x
c--   tv: heat transport       --- [Watt] (order of 1.E15 = PW)
c--   sv: freshwater transport --- [kg/sec] (order 1.E9 equiv. 1 Sv in vol.)
c--       convert from [ppt*m^3/sec] via rhoConst/1000.
c--       ( 1ppt = 1000*[mass(salt)]/[mass(seawater)] )
c--   mv: volume flux          --- [m^3/sec] (order of 10^6 = 1 Sv)
      _RL tu(nsect), su(nsect), mu(nsect)
      _RL musum(nsect), mumin(nsect), mumax(nsect), mulev(nsect,Nr)
      _RL xlon(nsect),beglat(nsect),endlat(nsect)
c--   1: A21 - Drake Passage 67W
c--   2: J89 - Indonesian Throughflow 125E
c--   3:  I6 - South Africa 30E
c--   4: I9S - Western Australia 115E
c--   5: P12 - Tasmania 145E
      DATA xlon   / 293.0, 125.0,  30.0, 115.0, 145.0 /
      DATA beglat / -67.0, -14.5, -70.0, -67.0, -67.0 /
      DATA endlat / -55.0,  -8.5, -31.0, -32.0, -42.0 /
c
c      _RL ylat2,beglon2,endlon2      
c      _RL ylat3,beglon3,endlon3      
c      parameter(ylat= 29., beglon=-42., endlon =-2.)
c      parameter(ylat= 29., beglon=282., endlon =352.)
c      parameter(ylat= 29., beglon=-82., endlon =-2.)
cc      parameter(ylat= 66.75,beglon=-34.5,endlon =-22.5)
cc      parameter(ylat2= 63.8,beglon2=-20,endlon2 =-5.)
cc      parameter(ylat3= 63.8,beglon3=-98.5,endlon3 =-80.0)

      logical doglobalread
      logical ladinit

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      doglobalread = .false.
      ladinit = .false.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      il=ilnblnk( tbarfile )
      write(fnametheta(1:80),'(2a,i10.10)')
     &      tbarfile(1:il),'.',optimcycle
c
      il=ilnblnk( sbarfile )
      write(fnamesalt(1:80),'(2a,i10.10)')
     &      sbarfile(1:il),'.',optimcycle
c
      il=ilnblnk( ubarfile )
      write(fnameuvel(1:80),'(2a,i10.10)')
     &      ubarfile(1:il),'.',optimcycle
c
      il=ilnblnk( vbarfile )
      write(fnamevvel(1:80),'(2a,i10.10)')
     &      vbarfile(1:il),'.',optimcycle

      do isect = 1, nsect

      call mdsfindunit( funit, mythid )
      write(fnameout(1:80),'(a,i2.2,a,i4.4)')
     &        'cost_trans_zon',isect,'.',optimcycle
      open(unit=funit,file=fnameout)

      write(msgbuf,'(a,1(X,D22.15))')
     &     'ECCO_TRANS_ZON_XLON: section at lon: ', xlon(isect)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )

      musum(isect) = 0.0

      do irec = 1, nmonsrec

        call active_read_xyz( fnametheta, tbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )
c
        call active_read_xyz( fnamesalt, sbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )
c
        call active_read_xyz( fnameuvel, ubar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )
c
        call active_read_xyz( fnamevvel, vbar, irec,
     &                        doglobalread, ladinit,
     &                        optimcycle, mythid,
     &                        dummy )

        tu(isect) = 0.0
        su(isect) = 0.0
        mu(isect) = 0.0
        mumin(isect) = 0.0
        mumax(isect) = 0.0
        kmin(isect) = 0
        kmax(isect) = 0

c--     Next, do the monthly average for temperature.
c--     Assign the first value to the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                   del_x = xc(i,j,bi,bj)-xc(i-1,j,bi,bj)
                   if (  xc(i,j,bi,bj) .ge. xlon(isect) .and.
     $                   xc(i,j,bi,bj) .lt. xlon(isect)+del_x .and.
     $                   yc(i,j,bi,bj) .ge. beglat(isect) .and.
     $                   yc(i,j,bi,bj) .le. endlat(isect) ) then
                       tu(isect) = tu(isect) + p5*(tbar(i,j,k,bi,bj)
     $                     + tbar(i-1,j,k,bi,bj))*ubar(i,j,k,bi,bj)
     $                     * _dyG(i,j,bi,bj)
     &                     * drF(k)*_hFacW(i,j,k,bi,bj)
     $                     *HeatCapacity_Cp*rhoNil
                       su(isect) = su(isect) + p5*(sbar(i,j,k,bi,bj)
     $                     + sbar(i-1,j,k,bi,bj))*ubar(i,j,k,bi,bj)
     $                     * _dyG(i,j,bi,bj)
     &                     *  drF(k)*_hFacW(i,j,k,bi,bj)
     &                     *  rhoNil/1000.
                       mu(isect) = mu(isect) + p5*(hFacC(i,j,k,bi,bj)
     $                     + hFacC(i-1,j,k,bi,bj))*ubar(i,j,k,bi,bj)
     $                     * _dyG(i,j,bi,bj)
     &                     *  drF(k)*_hFacW(i,j,k,bi,bj)
		   endif
                enddo
              enddo
	      mulev(isect,k) = mu(isect)
            enddo
          enddo
        enddo

        _GLOBAL_SUM_R8( tu(isect), mythid )
        _GLOBAL_SUM_R8( su(isect), mythid )
        _GLOBAL_SUM_R8( mu(isect), mythid )
c
	do k =1,nr
           _GLOBAL_SUM_R8( mulev(isect,k), mythid )
        enddo
        mumin(isect) = mulev(isect,1)
	mumax(isect) = mulev(isect,1)
	do k = 2,nr
	   if ( mulev(isect,k) .GT. mulev(isect,k-1) ) then
	      mumax(isect) = mulev(isect,k)
	      kmax(isect) = k
           endif
	   if ( mulev(isect,k) .LT. mulev(isect,k-1) ) then
	      mumin(isect) = mulev(isect,k)
	      kmin(isect) = k
           endif
        enddo

        musum(isect) = musum(isect) + mu(isect)

	write(msgbuf,'(a,i3,i5,2i3,5(X,D15.8))')
     &       'ECCO_TRANS_ZON ', isect, irec, kmin(isect), kmax(isect), 
     &       tu(isect), su(isect), mu(isect), mumin(isect), mumax(isect)
        call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )
c
	write(msgbuf,'(a,i3,1(X,D22.15))')
     &     'ECCO_TRANS_ZON_SUM  musum ', isect, musum(isect)
        call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )

        write(funit,'(i3,i5,2i3,5(X,D22.15))') 
     &       isect, irec, kmin(isect), kmax(isect), 
     &       tu(isect), su(isect), mu(isect), mumin(isect), mumax(isect)

c-- end loop over irec
      enddo

      close(funit)

c-- end loop over isect
      enddo

cph#endif

      end
