C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readscatyfields.F,v 1.10 2012/08/10 19:45:26 jmc Exp $
C $Name:  $

#include "ECCO_OPTIONS.h"


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
      integer tempDate_1
      integer middate(4)
      integer yday, ymod
      integer md, dd, sd, ld, wd
      integer beginmodel, beginlocal
      character*(128) fnametmp
      logical exst
cnew)

#ifdef ALLOW_ECCO_DEBUG
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ioUnit
#endif

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

#ifdef ALLOW_ECCO_DEBUG
      ioUnit=standardMessageUnit
#endif

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
           if ( ymod .GE. yday ) then
              call cal_FullDate( scatystartdate(1), 0, middate, mythid)
           else
              tempDate_1 = yday*10000+100+1
              call cal_FullDate( tempDate_1, 0, middate, mythid)
           endif
           call cal_TimePassed( middate, daydate, difftime, mythid )
           call cal_ToSeconds( difftime, diffsecs, mythid )
c           localrec = floor(diffsecs/86400. _d 0) + 1
           localrec = int(diffsecs/86400. _d 0) + 1
#endif
        il=ilnblnk(scatydatfile)
        write(fnametmp(1:128),'(2a,i4)')
     &       scatydatfile(1:il), '_', yday
        inquire( file=fnametmp, exist=exst )
        if (.NOT. exst) then
#ifdef ALLOW_ECCO_DEBUG
           WRITE(msgBuf,'(2A)') 'cost_readscatyfields : did not find',
     &                           fnametmp(1:128)
           CALL PRINT_MESSAGE( msgBuf, ioUnit, SQUEEZE_RIGHT, myThid )
#endif
c          now assume one big file
           write(fnametmp(1:128),'(a)') scatydatfile(1:il)
#ifdef ALLOW_SCAT_COST_CONTRIBUTION
c-- assume monthly fields
           localrec = obsrec
#else
c-- assume daily fields
           call cal_TimePassed(scatystartdate,daydate,difftime,mythid)
           call cal_ToSeconds( difftime, diffsecs, mythid )
c           localrec = floor(diffsecs/86400. _d 0) + 1
           localrec = int(diffsecs/86400. _d 0) + 1
#endif
        endif

        if ( (localrec .GT. 0).AND.(obsrec .GT. 0) ) then
             call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1,
     &                   scatydat, localrec, mythid )
        else
          do bj = jtlo,jthi
            do bi = itlo,ithi
              do j = jmin,jmax
                do i = imin,imax
                   scatydat(i,j,bi,bj) = spval
                enddo
              enddo
            enddo
          enddo
        endif
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
              if (scatydat(i,j,bi,bj) .le. spval) then
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

