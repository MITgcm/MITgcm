C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_readsssfields.F,v 1.3 2005/03/28 23:49:49 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_ReadSSSFields(
     I                               irec,
     I                               mythid
     &                             )

c     ==================================================================
c     SUBROUTINE cost_ReadSSSFields
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
c     SUBROUTINE cost_ReadSSSFields
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
      integer sssrec
      integer beginsss
      integer beginrun

      _RL spval
      _RL vartile

cnew(
      integer  il
      integer mody, modm
      integer iyear, imonth
      character*(80) fnametmp
      logical exst
cnew)

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      parameter (spval = 25.0 ) 
ce    --> there is certainly a better place for this.

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid) 
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

c      beginsss = sssstartdate(1)/10000
c      beginrun = modelstartdate(1)/10000
c      if ( beginsss .eq. beginrun ) then
c        sssrec = mod(modelstartdate(1)/100,100) -
c     &           mod(sssstartdate(1)/100,100) + irec
c      else
c        sssrec = ( beginrun - beginsss - 1)*nmonthyear +
c     &           (nmonthyear - mod(sssstartdate(1)/100,100) +
c     &            1) + mod(modelstartdate(1)/100,100) - 1 + irec
c      endif

cnew(
      mody = modelstartdate(1)/10000
      modm = modelstartdate(1)/100 - mody*100
      iyear = mody + INT((modm-1+irec-1)/12)
      imonth = 1 + MOD(modm-1+irec-1,12)

      il=ilnblnk(sssdatfile)
      write(fnametmp(1:80),'(2a,i4)') 
     &     sssdatfile(1:il), '_', iyear
      inquire( file=fnametmp, exist=exst )
      if (.NOT. exst) then
         write(fnametmp(1:80),'(a)') sssdatfile(1:il)
         imonth = irec
      endif

      call mdsreadfield( fnametmp, cost_iprec, cost_yftype, 1, sssdat,
     &                   imonth, mythid )
cnew)

      return
      end

