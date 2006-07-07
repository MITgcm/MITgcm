C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_atlantic.F,v 1.4 2006/07/07 22:53:35 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_atlantic(
     I                                mytime,
     I                                myiter,
     I                                mythid
     &                              )

c     ==================================================================
c     SUBROUTINE cost_atlantic
c     ==================================================================
c
c     o Compute meridional heat transport. The counters
c       are explicitly calculated instead of being incremented. This
c       reduces dependencies. The latter is useful for the adjoint code
c       generation.
c
c     started: Armin Koehl akoehl@ucsd.edu 22-Sep-2000
c
c     ==================================================================
c     SUBROUTINE cost_atlantic
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_COST_ATLANTIC

#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#include "CG2D.h"

#include "optim.h"
#include "cost.h"
#include "ecco_cost.h"
#include "ctrl_dummy.h"

#endif

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

#ifdef ALLOW_COST_ATLANTIC

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax

      logical first
      logical startofday
      logical startofmonth
      logical startofyear
      logical inday
      logical inmonth
      logical inyear
      logical last
      logical endofday
      logical endofmonth
      logical endofyear

      _RL        p5
      parameter( p5 = 0.5 )

      _RL del_y
      _RL tv
      _RL ylat,beglon,endlon      
      _RL ylat2,beglon2,endlon2      
      _RL ylat3,beglon3,endlon3      
c      parameter(ylat= 29., beglon=-42., endlon =-2.)
c      parameter(ylat= 29., beglon=282., endlon =352.)
c      parameter(ylat= 29., beglon=-82., endlon =-2.)
      parameter(ylat= 66.75,beglon=-34.5,endlon =-22.5)
      parameter(ylat2= 63.8,beglon2=-20,endlon2 =-5.)
      parameter(ylat3= 63.8,beglon3=-98.5,endlon3 =-80.0)
c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx



c--   Get the time flags and record numbers for the time averaging.

      call cost_AveragesFlags(
     I                    myiter,     mytime,       mythid,
     O                    first,      last, 
     O                    startofday, startofmonth, startofyear,
     O                    inday,      inmonth,      inyear,
     O                    endofday,   endofmonth,   endofyear,
     O                    sum1day,    dayrec,
     O                    sum1mon,    monrec,
     O                    sum1year,   yearrec
     &                  )

ce      print*,' cost_AveragesFields:       myiter = ', myiter
ce      print*,' cost_AveragesFields:       mytime = ', mytime
ce      print*,' cost_AveragesFields:        first = ', first
ce      print*,' cost_AveragesFields:   startofday = ', startofday
ce      print*,' cost_AveragesFields: startofmonth = ', startofmonth
ce      print*,' cost_AveragesFields:        inday = ', inday
ce      print*,' cost_AveragesFields:      inmonth = ', inmonth
ce      print*,' cost_AveragesFields:         last = ', last
ce      print*,' cost_AveragesFields:     endofday = ', endofday
ce      print*,' cost_AveragesFields:   endofmonth = ', endofmonth
ce      print*,' cost_AveragesFields:      sum1day = ', sum1day
ce      print*,' cost_AveragesFields:       dayrec = ', dayrec
ce      print*,' cost_AveragesFields:      sum1mon = ', sum1mon
ce      print*,' cost_AveragesFields:       monrec = ', monrec

ce      stop '... cost_AveragesFields stopped after ecco_TimeAverageFlags.'

c--   Next, do the monthly average for temperature.
      if (first) then
c--     Assign the first value to the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
             tv=0.0
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                   del_y=yc(i,j,bi,bj)-yc(i,j-1,bi,bj)
                   if(yc(i,j,bi,bj) .ge.ylat .and.
     $                  yc(i,j,bi,bj).lt.ylat+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon.and.
     $                  xc(i,j,bi,bj).le.endlon.or.
     $                  (yc(i,j,bi,bj) .ge.ylat2 .and.
     $                  yc(i,j,bi,bj).lt.ylat2+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon2.and.
     $                  xc(i,j,bi,bj).le.endlon2).or.
     $                  (yc(i,j,bi,bj) .ge.ylat3 .and.
     $                  yc(i,j,bi,bj).lt.ylat3+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon3.and.
     $                  xc(i,j,bi,bj).le.endlon3)) then
                       tv = tv+p5*(theta(i,j,k,bi,bj)
     $                     + theta(i,j-1,k,bi,bj))*vVel(i,j,k,bi,bj)
     $                     * _dxG(i,j,bi,bj)
     &                     *  drF(k)*_hFacS(i,j,k,bi,bj)
     $                     *HeatCapacity_Cp*rhoNil
                      endif
                enddo
              enddo
            enddo
            objf_atl(bi,bj) = tv
          enddo
        enddo
      else if (last ) then
         print*,"cost_atlantic last"
c--     Add the last value and devide by the number of accumulated
c--     records.
        do bj = jtlo,jthi
          do bi = itlo,ithi
                    objf_atl(bi,bj) = (objf_atl(bi,bj)
     &                                 )/float(nTimeSteps)
          enddo
        enddo
      else 
c--     Accumulate the array holding the average.
        do bj = jtlo,jthi
          do bi = itlo,ithi
             tv=0
            do k = 1,nr
              do j = jmin,jmax
                do i =  imin,imax
                   del_y=yc(i,j,bi,bj)-yc(i,j-1,bi,bj)
                   if(yc(i,j,bi,bj) .ge.ylat .and.
     $                  yc(i,j,bi,bj).lt.ylat+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon.and.
     $                  xc(i,j,bi,bj).le.endlon.or.
     $                  (yc(i,j,bi,bj) .ge.ylat2 .and.
     $                  yc(i,j,bi,bj).lt.ylat2+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon2.and.
     $                  xc(i,j,bi,bj).le.endlon2).or.
     $                  (yc(i,j,bi,bj) .ge.ylat3 .and.
     $                  yc(i,j,bi,bj).lt.ylat3+del_y.and.
     $                  xc(i,j,bi,bj).ge.beglon3.and.
     $                  xc(i,j,bi,bj).le.endlon3)) then
                      tv =  tv
     $                     +p5*(theta(i,j,k,bi,bj)
     $                     + theta(i,j-1,k,bi,bj))*vVel(i,j,k,bi,bj)
     $                     * _dxG(i,j,bi,bj)
     &                     *  drF(k)*_hFacS(i,j,k,bi,bj)
     $                     *HeatCapacity_Cp*rhoNil
                   endif
                enddo
              enddo
            enddo
            objf_atl(bi,bj)  =  objf_atl(bi,bj) +tv
          enddo
        enddo
      endif

#endif

      end
