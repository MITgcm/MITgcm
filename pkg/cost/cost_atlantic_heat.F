
#include "COST_CPPOPTIONS.h"

      subroutine cost_atlantic_heat( myThid )
C     /==========================================================\
C     | subroutine cost_atlantic_heat                            |
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

#ifdef ALLOW_COST_ATLANTIC_HEAT
C     ========= Local variables =========================
      integer    isecbeg      , isecend      , jsec
      integer    kmaxdepth
      integer i, j, k
      integer ig, jg
      integer bi, bj
      _RL     locfc
      _RL     vVel_bar(Nr), theta_bar(Nr) 
      _RL     countT(Nr), countV(Nr)
      _RL     petawatt
      _RL     sum
      parameter( petawatt = 1.e+15 )

C     80W - 0W at 24N
cph      parameter( isecbeg = 66, isecend = 86, jsec = 27 )
      parameter( isecbeg = 70, isecend = 90, jsec = 27 )
      parameter ( kmaxdepth = 12 )
C     80W - 0W at 48N
C      parameter( isecbeg = 70, isecend = 90, jsec = 33 )
C      parameter ( kmaxdepth = 14 )



C------------------------------------------------------
C     Accumulate meridionally integrated transports
C     Note bar(V)*bar(T) not bar(VT)
C     Attention pYFaceA [m^2*gravity*rhonil]
C------------------------------------------------------

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        locfc = 0.0
        sum = 0.0

        do j=1,sNy
         jg = myYGlobalLo-1+(bj-1)*sNy+j
         if (jg .eq. jsec) then

#undef ENERGYNORM

#ifdef ENERGYNORM

        do i=1,sNx
         ig = myXGlobalLo-1+(bi-1)*sNx+i
         if ((ig .ge. isecbeg) .and. (ig .le. isecend)) then
          sum = 0.0
          do k = 1, kmaxdepth
           sum = sum
     &            + (vVel(i,j,k,bi,bj) * maskS(i,j,k,bi,bj)
     &            * drF(k))**2
          end do
          locfc = locfc + sum*dxG(i,j,bi,bj)
         end if
        end do

#else

          do k = 1, Nr
           vVel_bar(k) = 0.0
           theta_bar(k) = 0.0
           countT(k) = 0.0
           countV(k) = 0.0
           do i=1,sNx
            ig = myXGlobalLo-1+(bi-1)*sNx+i
c
            if ((ig .ge. isecbeg) .and. (ig .le. isecend)) then
             vVel_bar(k) = vVel_bar(k) 
     &                      + vVel(i,j,k,bi,bj)
     &                      *maskS(i,j,k,bi,bj)
             theta_bar(k) = theta_bar(k)
     &                      + theta(i,j,k,bi,bj)*dxG(i,j,bi,bj)
     &                      *maskC(i,j,k,bi,bj)
cph     &            0.5*(theta(i,j,k,bi,bj)+theta(i,j-1,k,bi,bj) )
cph     &               *maskS(i,j,k,bi,bj)*dxG(i,j,bi,bj)
             countT(k) = countT(k) + maskC(i,j,k,bi,bj)
             countV(k) = countV(k) + maskS(i,j,k,bi,bj)
            end if

           enddo
          enddo
c
          do k = 1, Nr
           if ( k .LE. kmaxdepth .AND.
     &          countT(k) .NE. 0 .AND. countV(k) .NE. 0) then
            sum = sum
     &            + vVel_bar(k) * theta_bar(k) * drF(k) 
     &            / ( countT(k) * countV(k) )
           end if
          end do

#endif

         end if
        end do

        objf_atl(bi,bj) = 
     &   sum*HeatCapacity_Cp*rhonil/petawatt

c--   end of bi,bj loop
       end do
      end do

#endif
      
      end
