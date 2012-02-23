C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readscatyfields.F,v 1.7 2012/02/23 23:31:48 gforget Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadscatyFields(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ReadscatyFields
c     ==================================================================
c
c     o Read a given record of the SST data.
c
c     started: Christian Eckert eckert@mit.edu 25-May-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_ReadscatyFields
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"

#include "cal.h"
#include "ecco_cost.h"

c     == routine arguments ==

      integer irec
      integer mythid

c     == local variables ==

      integer bi,bj
      integer i,j,k
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      integer nobs
      integer scatyrec
      integer beginscaty
      integer beginrun

      _RL spval
      _RL vartile

cnew(
      integer  il
      integer mody, modm
      integer obsrec, localrec
      _RL daytime
      _RL diffsecs
      integer dayiter
      integer daydate(4)
      integer difftime(4)
      integer middate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer beginmodel, beginlocal
      character*(128) fnametmp
      logical exst
cnew)

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      parameter (spval = -1.8 )
ce    --> there is certainly a better place for this.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c      beginscaty = scatystartdate(1)/10000
c      beginrun = modelstartdate(1)/10000
c      if ( beginscaty .eq. beginrun ) then
c        scatyrec = mod(modelstartdate(1)/100,100) -
c     &           mod(scatystartdate(1)/100,100) + irec
c      else
c        scatyrec = ( beginrun - beginscaty - 1)*nmonthyear +
c     &           (nmonthyear - mod(scatystartdate(1)/100,100) +
c     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
c      endif

#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c-- assume monthly fields
           beginlocal = scatystartdate(1)/10000
           beginmodel = modelstartdate(1)/10000
           obsrec =
     &           ( beginmodel - beginlocal )*nmonthyear
     &         + ( mod(modelstartdate(1)/100,100)
     &            -mod(scatystartdate(1)/100,100) )
     &         + irec
           mody   = modelstartdate(1)/10000
           modm   = modelstartdate(1)/100 - mody*100
           yday   = mody + INT((modm-1+irec-1)/12)
           localrec = 1 + MOD(modm-1+irec-1,12)
#else
c-- assume daily fields
           obsrec = irec
           daytime = FLOAT(secondsperday*(irec-1)) +modelstart
           dayiter = hoursperday*(irec-1) +modeliter0
           call cal_getdate( dayiter, daytime, daydate, mythid )
           call cal_convdate( daydate,yday,md,dd,sd,ld,wd,mythid )
           ymod = scatystartdate(1)/10000
           if ( ymod .EQ. yday ) then
              middate(1) = scatystartdate(1)
           else
              middate(1) = yday*10000+100+1
           endif
           middate(2) = 0
           middate(3) = modelstartdate(3)
           middate(4) = modelstartdate(4)
           call cal_TimePassed( middate, daydate, difftime, mythid )
           call cal_ToSeconds( difftime, diffsecs, mythid )
           localrec = int(diffsecs/86400. _d 0) + 1
#endif  
        il=ilnblnk(scatydatfile)
        write(fnametmp(1:128),'(2a,i4)')
     &       scatydatfile(1:il), '_', yday
        inquire( file=fnametmp, exist=exst )
        if (.NOT. exst) then
           write(fnametmp(1:128),'(a)') scatydatfile(1:il)
           localrec = obsrec
        endif

      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1,
     &                   scatydat, localrec, mythid )
cnew)

      nobs = 0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          k = 1
          do j = jmin,jmax
            do i = imin,imax
              if (maskS(i,j,k,bi,bj) .eq. 0.) then
                scatymask(i,j,bi,bj) = 0. _d 0
              else
                scatymask(i,j,bi,bj) = 1. _d 0
              endif
              if (scatydat(i,j,bi,bj) .lt. spval) then
                scatymask(i,j,bi,bj) = 0. _d 0
              endif
              if (scatydat(i,j,bi,bj) .eq. 0. _d 0 ) then
                 scatymask(i,j,bi,bj) = 0. _d 0
              endif
              scatymask(i,j,bi,bj) = scatymask(i,j,bi,bj)*frame(i,j)
              scatydat(i,j,bi,bj) = scatydat(i,j,bi,bj)*
     &                              scatymask(i,j,bi,bj)
              nobs = nobs + int(scatymask(i,j,bi,bj))
            enddo
          enddo
        enddo
      enddo

c--   Calculate the field variance for present subdomain.
c--   One could of course do a global sum here.
      vartile = 0. _d 0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do j = jmin,jmax
            do i = imin,imax
            vartile=vartile+scatydat(i,j,bi,bj)*scatydat(i,j,bi,bj)
            enddo
          enddo
        enddo
      enddo

      if (nobs .gt. 0) then
        vartile = vartile/float(nobs)
      else
        vartile = spval
      endif

      return
      end

