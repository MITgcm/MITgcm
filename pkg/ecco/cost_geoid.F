C $Header: /u/gcmpack/MITgcm/pkg/ecco/Attic/cost_geoid.F,v 1.2 2004/10/11 16:38:53 heimbach Exp $

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
c
c              - Restructured the code in order to create a package
c                for the MITgcmUV.
c
c     changed: Ralf Giering Ralf.Giering@FastOpt.de 12-Jun-2001
c
c              - totally rewrite for parallel processing
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
#include "cost_sph.h"

c     == routine arguments ==

      _RL     sphcost
      _RL     shc( ncShc )
      integer mythid

#ifdef ALLOW_EGM96_ERROR_COV

c     == local variables ==

      Real*8  pinv( ncshc )

      integer i,j
      integer ilo,ihi
      integer ilnblnk
      integer ifnblnk
      integer ireads
      integer egmunit
      integer egmsize
      parameter( egmsize = 8 )

      integer jsize, joff, jbeg, jend, j

      _RL factor
      _RL recip_rr

c     == external functions ==
      integer  ilnblnk
      external ilnblnk
      integer  ifnblnk
      external ifnblnk

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

c--   Open the geoid error covariance data file.
      call mdsfindunit( egmunit, mythid )
      ilo = ifnblnk(geoid_covariancefile)
      ihi = ilnblnk(geoid_covariancefile)

      print*
      print*,' cost_Geoid: egmunit              = ',egmunit
      print*,'             geoid_covariancefile = ',
     &                     geoid_covariancefile(ilo:ihi)
      print*,'             opening file ...'

      open(egmunit, file   = geoid_covariancefile(ilo:ihi),
     &              form   = 'unformatted',
     &              access = 'direct',
     &              recl   = ncshc*egmsize,
     &              status = 'old' )

c--   round up the quotient to get jsize
      jsize = ncshc / numberOfProcs
      if (jsize*numberOfProcs .lt. ncshc ) then
         jsize = jsize + 1
      end if

c--   compute sub-intervall of 1..ncshc for this processor
      joff = myProcId*jsize
      jbeg = 1 + joff
      jend = min( ncshc, joff+jsize )

c--   read part of geoid error covariance matrix
c--   and compute corresponding part of cost contribution
CADJ loop = parallel
      do j = jbeg,jend

         read(egmunit,rec=j) pinv

         factor = 0.
         do i = 1,ncShc
            factor = factor + pinv(i)*shc(i)*recip_rr
         enddo
         sphcost = sphcost + factor*shc(j)
      enddo

c--   close geoid error covariance file
      close(egmunit)

      _END_MASTER( mythid )

      call global_sum_r8( cost, mythid )
      sphcost = cost

#else  /* ALLOW_EGM96_ERROR_COV */
      sphcost = 0.

#endif /* ALLOW_EGM96_ERROR_COV */

      end


