C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_trans_merid.F,v 1.2 2005/07/28 13:51:36 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_trans_merid( mythid )

c     ==================================================================
c     SUBROUTINE cost_trans_merid
c     ==================================================================
c
c     o Compute meridional transports. 
c
c     ==================================================================
c     SUBROUTINE cost_trans_merid
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

#ifdef ALLOW_COST_TRANSPORT

c     == local variables ==

      integer nsect
      parameter ( nsect = 11 )

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
      _RL del_y
c--   tv: heat transport       --- [Watt] (order of 1.E15 = PW)
c--   sv: freshwater transport --- [kg/sec] (order 1.E9 equiv. 1 Sv in vol.)
c--       convert from [ppt*m^3/sec] via rhoConst/1000.
c--       ( 1ppt = 1000*[mass(salt)]/[mass(seawater)] )
c--   mv: volume flux          --- [m^3/sec] (order of 10^6 = 1 Sv)
      _RL tv(nsect), sv(nsect), mv(nsect)
      _RL mvsum(nsect), mvmin(nsect), mvmax(nsect), mvlev(nsect,Nr)
      _RL ylat(nsect),beglon(nsect),endlon(nsect)
c--
c--   1:  A5 - Atlantic 26.5N
c--   2:  A2 - Atlantic 48N
c--   3:     - Atlantic 65N Denmark Strait
c--   4:     - Atlantic 26.5N Florida Strait
c--   5:  I5 - Indian: 30S
c--   6:  I4 - Indian: Mozambique Channel 24S
c--   7:  I2 - Indian: 8S
c--   8:  P1 - Pacific: 48N
c--   9:  P3 - Pacific: 26.5N
c--  10: P21 - Pacific: 17S
c--  11:  P6 - Pacific: 30S
c--
      DATA ylat   /  26.5,  48.0,  65.0,  26.5, -30.0, -24.5,  -7.5,
     &               48.0,  26.5, -17.0, -30.0  /
      DATA beglon / 279.5, 307.5, 317.0, 280.5,  30.0,  32.0,  38.0,
     &              142.0, 121.0, 147.0, 153.0 /
      DATA endlon / 347.5, 357.5, 340.0, 285.5, 116.0,  45.0, 115.0,
     &              236.0, 251.0, 290.0, 290.0 /
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
     &        'cost_trans_mer',isect,'.',optimcycle
      open(unit=funit,file=fnameout)

      write(msgbuf,'(a,1(X,D22.15))')
     &     'ECCO_TRANS_MER_YLAT: section at lat: ', ylat(isect)
      call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )

      mvsum(isect) = 0.0

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

        tv(isect) = 0.0
        sv(isect) = 0.0
        mv(isect) = 0.0
        mvmin(isect) = 0.0
        mvmax(isect) = 0.0
        kmin(isect) = 0
        kmax(isect) = 0

c--     Next, do the monthly average for temperature.
c--     Assign the first value to the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                   del_y = yc(i,j,bi,bj)-yc(i,j-1,bi,bj)
                   if (  yc(i,j,bi,bj) .ge. ylat(isect) .and.
     $                   yc(i,j,bi,bj) .lt. ylat(isect)+del_y .and.
     $                   xc(i,j,bi,bj) .ge. beglon(isect) .and.
     $                   xc(i,j,bi,bj) .le. endlon(isect) ) then
                       tv(isect) = tv(isect) + p5*(tbar(i,j,k,bi,bj)
     $                     + tbar(i,j-1,k,bi,bj))*vbar(i,j,k,bi,bj)
     $                     * _dxG(i,j,bi,bj)
     &                     * drF(k)*_hFacS(i,j,k,bi,bj)
     $                     *HeatCapacity_Cp*rhoNil
                       sv(isect) = sv(isect) + p5*(sbar(i,j,k,bi,bj)
     $                     + sbar(i,j-1,k,bi,bj))*vbar(i,j,k,bi,bj)
     $                     * _dxG(i,j,bi,bj)
     &                     *  drF(k)*_hFacS(i,j,k,bi,bj)
     &                     *  rhoNil/1000.
                       mv(isect) = mv(isect) + p5*(hFacC(i,j,k,bi,bj)
     $                     + hFacC(i,j-1,k,bi,bj))*vbar(i,j,k,bi,bj)
     $                     * _dxG(i,j,bi,bj)
     &                     *  drF(k)*_hFacS(i,j,k,bi,bj)
		   endif
                enddo
              enddo
	      mvlev(isect,k) = mv(isect)
            enddo
          enddo
        enddo

        _GLOBAL_SUM_R8( tv(isect), mythid )
        _GLOBAL_SUM_R8( sv(isect), mythid )
        _GLOBAL_SUM_R8( mv(isect), mythid )
c
	do k =1,nr
           _GLOBAL_SUM_R8( mvlev(isect,k), mythid )
        enddo
        mvmin(isect) = mvlev(isect,1)
	mvmax(isect) = mvlev(isect,1)
	do k = 2,nr
	   if ( mvlev(isect,k) .GT. mvlev(isect,k-1) ) then
	      mvmax(isect) = mvlev(isect,k)
	      kmax(isect) = k
           endif
	   if ( mvlev(isect,k) .LT. mvlev(isect,k-1) ) then
	      mvmin(isect) = mvlev(isect,k)
	      kmin(isect) = k
           endif
        enddo

        mvsum(isect) = mvsum(isect) + mv(isect)

	write(msgbuf,'(a,i3,i5,2i3,5(X,D15.8))')
     &       'ECCO_TRANS_MER ', isect, irec, kmin(isect), kmax(isect), 
     &       tv(isect), sv(isect), mv(isect), mvmin(isect), mvmax(isect)
        call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )
c
	write(msgbuf,'(a,i3,1(X,D22.15))')
     &     'ECCO_TRANS_MER_SUM  mvsum ', isect, mvsum(isect)
        call print_message( msgbuf, standardmessageunit,
     &     SQUEEZE_RIGHT, mythid )

        write(funit,'(i3,i5,2i3,5(X,D22.15))') 
     &       isect, irec, kmin(isect), kmax(isect), 
     &       tv(isect), sv(isect), mv(isect), mvmin(isect), mvmax(isect)

c-- end loop over irec
      enddo

      close(funit)

c-- end loop over isect
      enddo

#endif

      end
