C $Header: /u/gcmpack/MITgcm/pkg/cost/cost_vector.F,v 1.5 2012/08/10 19:36:02 jmc Exp $
C $Name:  $

#include "COST_OPTIONS.h"

      subroutine cost_vector( myThid )
C     /==========================================================\
C     | subroutine cost_vector                                   |
C     | o This routine computes the meridional heat transport.   |
C     |   The current indices are for North Atlantic 29N         |
C     |   2x2 global setup.                                      |
C     \==========================================================/
       implicit none

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "cost.h"

C     ======== Routine arguments ======================
C     myThid - Thread number for this instance of the routine.
      integer myThid

#ifdef ALLOW_COST_VECTOR
C     ========= Local variables =========================
      integer    isecbeg      , isecend      , jsec
      integer    kmaxdepth
      integer i, j, k
      integer ig, jg
      integer bi, bj
      _RL     locfc
      _RL        vVel_bar(Nr), theta_bar(Nr), count(Nr)
      _RL     petawatt
      _RL     sum
      parameter( petawatt = 1.e+15 )

C     80W - 0W at 24N
      parameter( isecbeg = 70, isecend = 90, jsec = 27 )
      parameter ( kmaxdepth = 15 )
C     80W - 0W at 48N
C      parameter( isecbeg = 70, isecend = 90, jsec = 33 )
C      parameter ( kmaxdepth = 14 )



C------------------------------------------------------
C     Accumulate meridionally integrated transports
C     Note bar(V)*bar(T) not bar(VT)
C     Attention pYFaceA [m^2*gravity*rhoConst]
C------------------------------------------------------

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        locfc = 0.0
        do j=1,sNy
         jg = myYGlobalLo-1+(bj-1)*sNy+j
         if (jg .eq. jsec) then

          do i=1,sNx
           ig = myXGlobalLo-1+(bi-1)*sNx+i
           if ((ig .ge. isecbeg) .and. (ig .le. isecend)) then
            sum = 0. _d 0
            do k = 1, kmaxdepth
             sum = sum
     &             + vVel(i,j,k,bi,bj) * maskS(i,j,k,bi,bj)
     &             * drF(k)
C     &             * 0.5*(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj))
            end do
            objf_vector(i,bi,bj) = sum*dxG(i,j,bi,bj)
           end if
          end do

         end if
        end do

        do i = 1,sNx
           print*,' --> objf_vector(i,bi,bj)  = ',
     &          objf_vector(i,bi,bj)
        end do

       END DO
      END DO

#endif

      end
