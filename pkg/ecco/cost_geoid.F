C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_geoid.F,v 1.5 2007/10/09 00:02:50 jmc Exp $
C $Name:  $

#include "COST_CPPOPTIONS.h"

      subroutine cost_geoid(
     O                     sphcost,
     I                     shc,
     I                     mythid
     &                   )

c     ==================================================================
c     SUBROUTINE cost_geoid
c     ==================================================================
c
c     o Evaluate the cost function of the geoid contribution.
c
c     started: Christian Eckert eckert@mit.edu 30-Jun-1999
c
c     changed: Christian Eckert eckert@mit.edu 25-Feb-2000
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c              - totally rewrite for parallel processing
c
c              heimbach@mit.edu 05-May-2005
c              - debugged and restructuted
c
c     ==================================================================
c     SUBROUTINE cost_geoid
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

#include "ecco_cost.h"
#ifdef ALLOW_SPHERE
# include "sphere.h"
#else
      integer ncShc
      parameter (ncShc=1)
#endif

c     == routine arguments ==

      _RL     sphcost
      _RL     shc( ncShc )
      integer mythid

#ifdef ALLOW_EGM96_ERROR_COV

c     == local variables ==

      Real*8  pinv( ncshc )

      integer i,j
      integer ilo,ihi
      integer ireads
      integer egmsize
      parameter( egmsize = 8 )

      integer jsize, joff, jbeg, jend
      integer reclength

      _RL factor
      _RL recip_rr

c     == end of interface ==

c--   Only the master thread is doing I/O
      _BEGIN_MASTER( mythid )

c--   initialise cost variable for all threads
c--   to allow usage of global_sum_r8
      sphcost = 0. _d 0
      do i=1,ncShc
         pinv(i) = 0. _d 0
      enddo

c--   Initialise variables.
      recip_rr = recip_rsphere*recip_rsphere

c--   round up the quotient to get jsize
      jsize = ncshc / numberOfProcs
      if (jsize*numberOfProcs .lt. ncshc ) then
         jsize = jsize + 1
      end if

c--   compute sub-intervall of 1..ncshc for this processor
      joff = myProcId*jsize
      jbeg = 1 + joff
      jend = min( ncshc, joff+jsize )
      reclength = ncshc*egmsize

c--   read part of geoid error covariance matrix
c--   and compute corresponding part of cost contribution
CADJ loop = parallel
      do j = jbeg,jend

         call cost_readgeoid(
     I       geoid_covariancefile, reclength, j,
     O       pinv,
     I       mythid )

         factor = 0.
         do i = 1,ncShc
            factor = factor + pinv(i)*shc(i)*recip_rr
         enddo
         sphcost = sphcost + factor*shc(j)
      enddo

      _END_MASTER( mythid )

      call global_sum_r8( sphcost, mythid )

#else  /* ALLOW_EGM96_ERROR_COV */
      sphcost = 0.

#endif /* ALLOW_EGM96_ERROR_COV */

      end


