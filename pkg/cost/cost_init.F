C $Header: /u/gcmpack/MITgcm/pkg/cost/Attic/cost_init.F,v 1.2 2001/07/13 13:37:45 heimbach Exp $

#include "COST_CPPOPTIONS.h"


      subroutine cost_init(
     I                           mythid
     &                         )

c     ==================================================================
c     SUBROUTINE cost_init
c     ==================================================================
c
c     o Initialise the variable cost function part.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 18-Apr-2000
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     ==================================================================
c     SUBROUTINE cost_init
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer bi,bj
      integer itlo,ithi
      integer jtlo,jthi

c     == external functions ==

c     == end of interface ==
      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c--   Initialize the tiled cost function contributions.
      do bj = jtlo,jthi
        do bi = itlo,ithi
          objf_hflux(bi,bj)   = 0. _d 0
          objf_sflux(bi,bj)   = 0. _d 0
          objf_tauu(bi,bj) = 0. _d 0
          objf_tauv(bi,bj) = 0. _d 0
          objf_temp(bi,bj) = 0. _d 0
          objf_salt(bi,bj) = 0. _d 0
          objf_sst(bi,bj)  = 0. _d 0
          objf_h(bi,bj)    = 0. _d 0
          objf_atl(bi,bj)  = 0. _d 0
          objf_ctdt(bi,bj) = 0. _d 0
          objf_ctds(bi,bj) = 0. _d 0
          objf_test(bi,bj) = 0. _d 0
          objf_tracer(bi,bj) = 0. _d 0
        enddo
      enddo

c--   Initialise the "global" parts of the cost function.
      _BEGIN_MASTER( mythid )
        objf_hmean = 0. _d 0
        fc         = 0. _d 0
      _END_MASTER( mythid )

      _BARRIER

      return
      end

