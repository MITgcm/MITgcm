C $Header: /u/gcmpack/MITgcm/pkg/cost/Attic/cost_initvaria.F,v 1.5 2004/09/27 18:00:19 heimbach Exp $

#include "PACKAGES_CONFIG.h"
#include "COST_CPPOPTIONS.h"

      subroutine cost_initvaria( mythid )

c     ==================================================================
c     SUBROUTINE cost_initvaria
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
c     SUBROUTINE cost_initvaria
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"

#include "cost.h"

c     == routine arguments ==

      integer mythid

c     == local variables ==

      integer i,j,k
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
          objf_atl(bi,bj)  = 0. _d 0
          objf_test(bi,bj) = 0. _d 0
          objf_tracer(bi,bj) = 0. _d 0
#ifdef ALLOW_COST_VECTOR
          do i=1,sNx
            objf_vector(i,bi,bj) = 0. _d 0
          end do
#endif
c
#ifdef ALLOW_COST_ATLANTIC_HEAT
          do k=1,nr
            do j=1,sNy
              do i=1,sNx
                cMeanTheta(i,j,k,bi,bj) = 0. _d 0
                cMeanUVel(i,j,k,bi,bj)  = 0. _d 0
                cMeanVVel(i,j,k,bi,bj)  = 0. _d 0
              end do
            end do
          end do
#endif
c
#ifdef ALLOW_COST_STATE_FINAL
c          do k=1,nr
            do j=1,sNy
              do i=1,sNx
                objf_state_final(i,j,bi,bj) = 0. _d 0
cph No init. of cost_state_final here,
cph because we need it in ADM*TLM
              end do
            end do
c          end do
#endif
       enddo
      enddo

#ifdef ALLOW_ECCO
      call ecco_cost_initvaria( mythid )
#endif

c--   Initialise the "global" parts of the cost function.
      _BEGIN_MASTER( mythid )
        fc         = 0. _d 0
      _END_MASTER( mythid )

      _BARRIER

      return
      end

