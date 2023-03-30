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
c     == end of interface ==

c--   Initialize the tiled cost function contributions.
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          tile_fc(bi,bj)   = 0. _d 0
          objf_atl(bi,bj)  = 0. _d 0
          objf_test(bi,bj) = 0. _d 0
          objf_tracer(bi,bj) = 0. _d 0
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
       ENDDO
      ENDDO

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
