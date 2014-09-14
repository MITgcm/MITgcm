C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_init_varia.F,v 1.16 2014/09/14 15:21:13 gforget Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      SUBROUTINE COST_INIT_VARIA( mythid )

c     ==================================================================
c     SUBROUTINE cost_init_varia
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
c     SUBROUTINE cost_init_varia
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
          tile_fc(bi,bj)   = 0. _d 0
#if ( !defined (ALLOW_ECCO) || !defined (ALLOW_COST_ATLANTIC) )
          objf_atl(bi,bj)  = 0. _d 0
#endif
          objf_test(bi,bj) = 0. _d 0
          objf_tracer(bi,bj) = 0. _d 0
#ifdef ALLOW_COST_TRANSPORT
          objf_transport(bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_COST_DEPTH
          objf_depth(bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_COST_VECTOR
          do i=1,sNx
            objf_vector(i,bi,bj) = 0. _d 0
          end do
#endif
c
#ifdef ALLOW_COST
          do k=1,Nr
            do j=1,sNy
              do i=1,sNx
                cMeanTheta(i,j,k,bi,bj) = 0. _d 0
                cMeanUVel(i,j,k,bi,bj)  = 0. _d 0
                cMeanVVel(i,j,k,bi,bj)  = 0. _d 0
                cMeanThetaUVel(i,j,k,bi,bj) = 0. _d 0
                cMeanThetaVVel(i,j,k,bi,bj) = 0. _d 0
              end do
            end do
          end do
#endif
c
#ifdef ALLOW_COST_STATE_FINAL
            do j=1,sNy
             do i=1,sNx
              do k=1,4*Nr
                objf_state_final(i,j,bi,bj,k) = 0. _d 0
              enddo
              objf_state_final(i,j,bi,bj,4*Nr+1) = 0. _d 0
cph No init. of cost_state_final here,
cph because we need it in ADM*TLM
              end do
            end do
#endif
       enddo
      enddo

#ifdef ALLOW_SEAICE
      call seaice_cost_init_varia( mythid )
#endif

#ifdef ALLOW_THSICE
      call thsice_cost_init_varia( mythid )
#endif

c--   Initialise the "global" parts of the cost function.
      _BEGIN_MASTER( mythid )
        fc         = 0. _d 0
        glofc      = 0. _d 0
      _END_MASTER( mythid )

      _BARRIER

      RETURN
      END
