
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
      integer    jsecbeg      , jsecend      , isec
      integer    kmaxdepth
      integer i, j, k
      integer ig, jg
      integer bi, bj
      _RL     locfc
      _RL     uVel_bar(Nr), vVel_bar(Nr), theta_bar(Nr)
      _RL     thetaUvel_bar(Nr), thetaVvel_bar(Nr)
      _RL     countU(Nr), countV(Nr), countT(Nr)
      _RL     countTU(Nr), countTV(Nr)
      _RL     petawatt
      _RL     sum
      parameter( petawatt = 1.e+15 )

C     80W - 0W at 24N
      parameter( isecbeg = 69, isecend = 87, jsec = 28 )
cph      parameter( isecbeg = 70, isecend = 90, jsec = 30 )
      parameter( jsecbeg = 10, jsecend = 27, isec = 59 )
      parameter ( kmaxdepth = 5 )
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
        sum = 0.0

#define MERID_TRANSPORT

#ifdef MERID_TRANSPORT

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

        do j=1,sNy
         jg = myYGlobalLo-1+(bj-1)*sNy+j
         if (jg .eq. jsec) then

          do k = 1, Nr
           vVel_bar(k) = 0.0
           thetaVvel_bar(k) = 0.0
           countV(k) = 0.0
           countTV(k) = 0.0
           do i=1,sNx
            ig = myXGlobalLo-1+(bi-1)*sNx+i
c
            if ((ig .ge. isecbeg) .and. (ig .le. isecend)) then
               vVel_bar(k) = vVel_bar(k) 
     &              + cMeanVVel(i,j,k,bi,bj)*maskS(i,j,k,bi,bj)

               thetaVvel_bar(k) = thetaVvel_bar(k)
     &            + cMeanThetaVVel(i,j,k,bi,bj)*dxG(i,j,bi,bj)
     &                 *maskS(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)

             countTV(k) = countTV(k) + 
     &            maskS(i,j,k,bi,bj)*maskC(i,j,k,bi,bj)
             countV(k) = countV(k) + 
     &            maskS(i,j,k,bi,bj)
            end if

           enddo
          enddo
c
          do k = 1, Nr
#ifdef ALLOW_COST_ATLANTIC_HEAT_DOMASS
           if ( k .LE. kmaxdepth .AND. countV(k) .NE. 0) then
            sum = sum
     &            + vVel_bar(k)*drF(k)/countV(k)
           end if
#else
           if ( k .LE. kmaxdepth .AND. countTV(k) .NE. 0) then
            sum = sum
     &            + thetaVVel_bar(k)*drF(k)/countTV(k)
           end if
#endif
          end do

#endif /* ENERGYNORM */

#else

cph need to change this part to go from 
cph \bar{u}*\bar{T} to \bar{u*T}
cph (required store dir. are now in place)

        do i=1,sNx
         ig = myXGlobalLo-1+(bi-1)*sNx+i
         if (ig .eq. isec) then

          do k = 1, Nr
           uVel_bar(k) = 0.0
           theta_bar(k) = 0.0
           countT(k) = 0.0
           countU(k) = 0.0
           do j=1,sNy
            jg = myYGlobalLo-1+(bj-1)*sNy+j
c
            if ((jg .ge. jsecbeg) .and. (jg .le. jsecend)) then
             uVel_bar(k) = uVel_bar(k)
     &                      + cMeanUVel(i,j,k,bi,bj)
     &                      *maskW(i,j,k,bi,bj)
     &                      *maskC(i,j,k,bi,bj)*maskC(i-1,j,k,bi,bj)
             theta_bar(k) = theta_bar(k) +
     &            0.5*( cMeanTheta(i,j,k,bi,bj)
     &                 +cMeanTheta(i-,j,k,bi,bj) )
     &                 *maskW(i,j,k,bi,bj)*dyG(i,j,bi,bj)
     &                 *maskC(i,j,k,bi,bj)*maskC(i-1,j,k,bi,bj)
             countT(k) = countT(k) + maskW(i,j,k,bi,bj)
     &                   *maskC(i,j,k,bi,bj)*maskC(i-1,j,k,bi,bj)
             countU(k) = countU(k) + maskW(i,j,k,bi,bj)
     &                   *maskC(i,j,k,bi,bj)*maskC(i-1,j,k,bi,bj)
            end if

           enddo
          enddo
c
          do k = 1, Nr
           if ( k .LE. kmaxdepth .AND.
     &          countT(k) .NE. 0 .AND. countU(k) .NE. 0) then
            sum = sum
     &            + uVel_bar(k) * theta_bar(k) * drF(k)
     &            / ( countT(k) * countU(k) )
           end if
          end do

#endif

         end if
        end do

#ifdef ALLOW_COST_ATLANTIC_HEAT_DOMASS
        objf_atl(bi,bj) = 
     &     sum*1.E-6
#else
        objf_atl(bi,bj) = 
     &     sum*HeatCapacity_Cp*rhoConst/petawatt
#endif

c--   end of bi,bj loop
       end do
      end do

#endif
      
      end
