c swd -- bulkf formula pkg

#include "CPP_OPTIONS.h"

      subroutine bulkf_forcing(  bi,bj,
     I                           mycurrenttime,
     I                           mycurrentiter,
     I                           mythid
     I                         )

c     ==================================================================
c     SUBROUTINE BULKF_FORCING
c     ==================================================================
c
c     o Get the surface fluxes used to force ocean model
c       Output:
c       ------
c       ustress, vstress - wind stress
c       qnet             - net heat flux
c       empmr            - freshwater flux
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)     at height hu (m)
c       Tair  - mean air temperature (K)  at height ht (m)
c       Qair  - mean air humidity (kg/kg) at height hq (m)
c       theta(k=1) - sea surface temperature (K)
c       rain  - precipitation
c       runoff - river(ice) runoff
c
c     ==================================================================
c     SUBROUTINE bulkf_forcing
c     ==================================================================

      implicit none

c     == global variables ==

#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_BULK_FORCE
#include "BULKF.h"
#include "BULKF_DIAG.h"
#endif
#ifdef ALLOW_THERM_SEAICE
#include "ICE.h"
#endif
c     == routine arguments ==

      integer mythid
      integer mycurrentiter
      _RL     mycurrenttime
      integer bi,bj

C     == Local variables ==
      integer i,j,k

#ifdef ALLOW_BULK_FORCE

      _RL     evap(1-olx:snx+olx,1-oly:sny+oly)
      _RL     ustress(1-olx:snx+olx,1-oly:sny+oly)
      _RL     vstress(1-olx:snx+olx,1-oly:sny+oly)
      _RL     savssq(1-olx:snx+olx,1-oly:sny+oly)
      _RL     fsh(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flh(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flwup(1-olx:snx+olx,1-oly:sny+oly)
      _RL     flwupnet(1-olx:snx+olx,1-oly:sny+oly)
      _RL     fswnet(1-olx:snx+olx,1-oly:sny+oly)

      _RL     df0dT,hfl

c variables to include seaice effect
      _RL     tmp
      _RL     albedo
      integer iceornot

c     == external functions ==

#endif /* ALLOW_BULK_FORCE */

c     determine forcing field records

#ifdef ALLOW_BULK_FORCE

      
          k = 1
          do j = 1-oly,sny+oly
            do i = 1-olx,snx+olx
              if (hFacC(i,j,1,bi,bj).ne.0.0) then
#ifdef ALLOW_THERM_SEAICE
               if (ICEMASK(i,j,bi,bj).gt.0.d0) then
                 tmp=Tsrf(i,j,bi,bj)
                if (snowheight(i,j,bi,bj).gt.3.d-1) then
                   iceornot=2
                 else
                   iceornot=1
                 endif
               else
                 tmp=theta(i,j,1,bi,bj)
                 iceornot=0
               endif
#else
               tmp=theta(i,j,1,bi,bj)
               iceornot=0
#endif
                call BULKF_FORMULA_LANL(
     &            uwind(i,j,bi,bj), vwind(i,j,bi,bj),
     &            wspeed(i,j,bi,bj), Tair(i,j,bi,bj), Qair(i,j,bi,bj),
     &            cloud(i,j,bi,bj), tmp,
     &            flwup(i,j), flh(i,j), 
     &            fsh(i,j), df0dT,
     &            ustress(i,j), vstress(i,j),
     &            evap(i,j), savssq(i,j),
     &            iceornot, readwindstress)
cQQ use down long wave data
                  flwupnet(i,j)=flwup(i,j)-flw(i,j,bi,bj)
cQQ using down solar, need to have water albedo -- .1
#ifdef ALLOW_THERM_SEAICE
                  if (ICEMASK(i,j,bi,bj).gt.0.d0) then
                     call sfc_albedo(ICEHEIGHT(i,j,bi,bj),
     &                               SNOWHEIGHT(i,j,bi,bj),
     &                               Tsrf(i,j,bi,bj),
     &                               sage(i,j,bi,bj), albedo)
                  else
                     albedo=.1
                  endif
#else
                  albedo=.1                  
#endif
                   fswnet(i,j)=solar(i,j,bi,bj)*(1.d0-albedo)
              else
                  ustress(i,j) = 0. _d 0
                  vstress(i,j) = 0. _d 0
                  fsh(i,j)      = 0. _d 0
                  flh(i,j)      = 0. _d 0
                  flwup(i,j)      =0. _d 0
                  evap(i,j)     =0. _d 0
                  fswnet(i,j)   =0. _d 0
                  savssq(i,j)   =0. _d 0
              endif
          enddo
         enddo


      if ( .NOT.readwindstress) then
cswd move wind stresses to u and v points
           do j = 1,sny
             do i = 1,snx
                fu(i,j,bi,bj)=
     &            (ustress(i,j)+ustress(i-1,j))/2*
     &                         maskW(i,j,1,bi,bj)
                fv(i,j,bi,bj)=
     &            (vstress(i,j)+vstress(i,j-1))/2*
     &                          maskS(i,j,1,bi,bj)
             enddo
           enddo
      endif
c
c
c     Add all contributions.
      k = 1
          do j = 1,sny
            do i = 1,snx
             if (hFacC(i,j,1,bi,bj).ne.0.0) then
c             Net surface heat flux.
              hfl = 0. _d 0
              hfl = hfl + fsh(i,j)
              hfl = hfl + flh(i,j)
              hfl = hfl + flwupnet(i,j)
c QQ minus solar for ncep data
c QQ plus solar for dasilva
c             hfl = hfl - solar(i,j,bi,bj)
cQQ for calculated sw net
              hfl = hfl - fswnet(i,j)
c Heat flux:
              Qnet(i,j,bi,bj) = -hfl*maskc(i,j,k,bi,bj)
cswdcou -- add ---
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = df0dT
#endif
cswdcou -- end add ---
c Salt flux from Precipitation and Evaporation.
              EmPmR(i,j,bi,bj) = (-evap(i,j)+rain(i,j,bi,bj)
     &                           - runoff(i,j,bi,bj))*maskc(i,j,k,bi,bj)

cccccccccccCHEATccccccccccccccccccccccccc
c            Qnet(i,j,bi,bj) = Qnetch(i,j,bi,bj)
c            EmPmR(i,j,bi,bj) = EmPch(i,j,bi,bj)
cccccccccccccccccccccccccccccccccccccccccc
             else
              Qnet(i,j,bi,bj) =0.d0
              EmPmR(i,j,bi,bj)=0.d0
cswdcou -- add ---
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = 0.d0
#endif
cswdcou -- end add ---
             endif
            enddo
          enddo


cc     Update the tile edges.
c      _EXCH_XY_R8(Qnet,   mythid)
c      _EXCH_XY_R8(EmPmR,   mythid)
c      _EXCH_XY_R8(fu     , mythid)
c      _EXCH_XY_R8(fv     , mythid)


#ifdef ALLOW_TIMEAVE
        call BULKF_AVE(bi,bj,mythid, fswnet,
     &                 flh, fsh, flwup, evap, savssq, flwupnet)
#endif  /*ALLOW_TIMEAVE*/

#endif  /*ALLOW_BULK_FORCE*/

       return
      end
