#include "SEAICE_OPTIONS.h"

      subroutine cost_ice_test( mytime, myiter, mythid )

c     ==================================================================
c     SUBROUTINE cost_ice_test
c     ==================================================================
c
c     o Compute sea-ice cost function.  The following options can be
c       selected with data.seaice (SEAICE_PARM02) variable cost_ice_flag
c
c     cost_ice_flag = 1
c     - compute mean sea-ice volume
c       costIceStart < mytime < costIceEnd
c
c     cost_ice_flag = 2
c     - compute mean sea-ice area
c       costIceStart < mytime < costIceEnd
c
c     cost_ice_flag = 3
c     - heat content of top level plus latent heat of sea-ice
c       costIceStart < mytime < costIceEnd
c
c     cost_ice_flag = 4
c     - heat content of top level
c       costIceStart < mytime < costIceEnd
c
c     cost_ice_flag = 5
c     - heat content of top level plus sea-ice plus latent heat of snow
c       costIceStart < mytime < costIceEnd
c
c     cost_ice_flag = 6
c     - quadratic cost function measuring difference between pkg/seaice
c       AREA variable and simulated sea-ice measurements at every time
c       step.
c
c     ==================================================================
c
c     started: menemenlis@jpl.nasa.gov 26-Feb-2003
c
c     ==================================================================
c     SUBROUTINE cost_ice_test
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_COST_ICE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_COST.h"
#include "SEAICE.h"
#include "DYNVARS.h"
#include "cost.h"
#endif /* ALLOW_COST_ICE */

c     == routine arguments ==

      _RL     mytime
      integer myiter
      integer mythid

#ifdef ALLOW_COST_ICE

c     == local variables ==

c     msgBuf      - Informational/error message buffer
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      integer bi,bj,i,j,kSrf
      _RL tempVar

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( usingPCoords ) then
       kSrf = Nr
      else
       kSrf = 1
      endif
      if ( myTime .GT. (endTime - lastinterval) ) then
         tempVar = 1. _d 0/
     &             ( ( 1. _d 0 + min(endTime-startTime,lastinterval) )
     &             / deltaTClock )

cph(
      write(standardMessageUnit,*) 'ph-ice B ', myiter,
     &        theta(4,4,kSrf,1,1), area(4,4,1,1), heff(4,4,1,1)
cph)
         if ( cost_ice_flag .eq. 1 ) then
c     sea-ice volume
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       tempVar * rA(i,j,bi,bj) * HEFF(i,j,bi,bj)
                     enddo
                  enddo
               enddo
            enddo

         elseif ( cost_ice_flag .eq. 2 ) then
c     sea-ice area
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       tempVar * rA(i,j,bi,bj) * AREA(i,j,bi,bj)
                     enddo
                  enddo
               enddo
            enddo

c     heat content of top level:
c     theta * delZ * (sea water heat capacity = 3996 J/kg/K)
c                  * (density of sea-water = 1026 kg/m^3)
c
c     heat content of sea-ice:
c     tice * heff * (sea ice heat capacity = 2090 J/kg/K)
c                 * (density of sea-ice = 910 kg/m^3)
c
c     note: to remove mass contribution to heat content,
c     which is not properly accounted for by volume converving
c     ocean model, theta and tice are referenced to freezing
c     temperature of sea-ice, here -1.96 deg C
c
c     latent heat content of sea-ice:
c     - heff * (latent heat of fusion = 334000 J/kg)
c            * (density of sea-ice = 910 kg/m^3)
c
c     latent heat content of snow:
c     - hsnow * (latent heat of fusion = 334000 J/kg)
c             * (density of snow = 330 kg/m^3)

         elseif ( cost_ice_flag .eq. 3 ) then
c     heat content of top level plus latent heat of sea-ice
            do bj=myByLo(myThid),myByHi(myThid)
             do bi=myBxLo(myThid),myBxHi(myThid)
              do j = 1,sny
               do i =  1,snx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 -
     &                 HEFF(i,j,bi,bj) * 334000. _d 0 * 910. _d 0 )
               enddo
              enddo
             enddo
            enddo

         elseif ( cost_ice_flag .eq. 4 ) then
c     heat content of top level
            do bj=myByLo(myThid),myByHi(myThid)
             do bi=myBxLo(myThid),myBxHi(myThid)
              do j = 1,sny
               do i =  1,snx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 )
               enddo
              enddo
             enddo
            enddo

         elseif ( cost_ice_flag .eq. 5 ) then
c     heat content of top level plus sea-ice plus latent heat of snow
            do bj=myByLo(myThid),myByHi(myThid)
             do bi=myBxLo(myThid),myBxHi(myThid)
              do j = 1,sny
               do i =  1,snx
                objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                 tempVar * rA(i,j,bi,bj) * (
     &                 (THETA(i,j,kSrf,bi,bj) + 1.96 _d 0 ) *
     &                 drF(kSrf) * 3996. _d 0 * 1026. _d 0 +
     &                 (TICES(i,j,1,bi,bj) - 273.15 _d 0 + 1.96 _d 0 ) *
     &                 HEFF(i,j,bi,bj) * 2090. _d 0 * 910. _d 0 -
     &                 HEFF(i,j,bi,bj) * 334000. _d 0 * 910. _d 0 -
     &                 HSNOW(i,j,bi,bj) * 334000. _d 0 * 330. _d 0 )
               enddo
              enddo
             enddo
            enddo

         elseif ( cost_ice_flag .eq. 6 ) then
c     Qadratic cost function measuring difference between pkg/seaice
c     AREA variable and simulated sea-ice measurements at every time
c     step.  For time being no measurements are read-in.  It is
c     assumed that measurements are AREA=0.5 at all times everywhere.
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       ( AREA(i,j,bi,bj) - 0.5 _d 0 ) *
     &                       ( AREA(i,j,bi,bj) - 0.5 _d 0 )
                     enddo
                  enddo
               enddo
            enddo

         elseif ( cost_ice_flag .eq. 7 ) then

            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       UICE(i,j,bi,bj) * UICE(i,j,bi,bj) +
     &                       VICE(i,j,bi,bj) * VICE(i,j,bi,bj)

                     enddo
                  enddo
               enddo
            enddo

         else
            WRITE(msgBuf,'(A)')
     &           'COST_ICE: invalid cost_ice_flag'
            CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &           SQUEEZE_RIGHT , myThid )
            STOP 'ABNORMAL END: S/R COST_ICE'
         endif
      endif

cph(
      write(standardMessageUnit,*) 'ph-ice C ', myiter, objf_ice(1,1)
cph)

#endif /* ALLOW_COST_ICE */

      return
      end
