C $Header: /u/gcmpack/MITgcm/pkg/seaice/Attic/cost_ice.F,v 1.1 2005/08/25 16:10:42 heimbach Exp $

#include "SEAICE_OPTIONS.h"

      subroutine cost_ice           (
     I                                mytime,
     I                                myiter,
     I                                mythid
     &                              )

c     ==================================================================
c     SUBROUTINE cost_ice
c     ==================================================================
c
c     o Compute sea-ice cost function.  The following options can
c       be selected with data.cost variable cost_ice_flag:
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
c     SUBROUTINE cost_ice
c     ==================================================================

      implicit none

c     == global variables ==
#ifdef ALLOW_COST_ICE
#include "EEPARAMS.h"
#include "SIZE.h"
#include "GRID.h"
#include "PARAMS.h"
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
      integer bi,bj,i,j
      _RL tempVar

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

c     == end of interface ==

      if ( myTime .GT. (endTime - lastinterval) ) then
         tempVar = 1. / 
     &             ( ( 1. + min(endTime-startTime,lastinterval) )
     &             / deltaTClock ) 

         if ( cost_ice_flag .eq. 1 ) then
c     sea-ice volume
            do bj=myByLo(myThid),myByHi(myThid)
               do bi=myBxLo(myThid),myBxHi(myThid)
                  do j = 1,sny
                     do i =  1,snx
                        objf_ice(bi,bj) = objf_ice(bi,bj) +
     &                       tempVar * rA(i,j,bi,bj) * HEFF(i,j,1,bi,bj)
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
     &                       tempVar * rA(i,j,bi,bj) * AREA(i,j,1,bi,bj)
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
     &                 (THETA(i,j,1,bi,bj) + 1.96 ) *
     &                 drF(1) * 3996 * 1026 -
     &                 HEFF(i,j,1,bi,bj) * 334000 * 910 )
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
     &                 (THETA(i,j,1,bi,bj) + 1.96 ) *
     &                 drF(1) * 3996 * 1026 )
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
     &                 (THETA(i,j,1,bi,bj) + 1.96 ) *
     &                 drF(1) * 3996 * 1026 +
     &                 (TICE(i,j,bi,bj) - 273.15 + 1.96 ) *
     &                 HEFF(I,J,1,bi,bj) * 2090 * 910 -
     &                 HEFF(i,j,1,bi,bj) * 334000 * 910 -
     &                 HSNOW(I,J,bi,bj) * 334000 * 330 )
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
     &                       ( AREA(i,j,1,bi,bj) - 0.5 ) *
     &                       ( AREA(i,j,1,bi,bj) - 0.5 )
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
      print *, 'ph-ice ', objf_ice(1,1)
cph)

#endif /* ALLOW_COST_ICE */

      end
