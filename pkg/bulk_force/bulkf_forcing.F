C $Header: /u/gcmpack/MITgcm/pkg/bulk_force/bulkf_forcing.F,v 1.5 2003/11/23 01:36:55 jmc Exp $
C $Name:  $

#include "BULK_FORCE_OPTIONS.h"
#undef ALLOW_THSICE

      subroutine bulkf_forcing(
     I                           myTime, myIter, mythid )

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
#include "BULKF_PARAMS.h"
#include "BULKF.h"
#include "BULKF_INT.h"
#include "BULKF_DIAG.h"
#ifdef ALLOW_THSICE
#include "THSICE.h"
#endif
c     == routine arguments ==

      integer mythid
      integer myIter
      _RL     myTime

#ifdef ALLOW_BULK_FORCE
C     == Local variables ==
      integer bi,bj
      integer i,j,k

      _RL     df0dT,hfl

c variables to include seaice effect
      _RL     tmp
      _RL     albedo
      integer iceornot

c     == external functions ==


c     determine forcing field records

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
          do j = 1-oly,sny+oly
           do i = 1-olx,snx+olx
             if (hFacC(i,j,1,bi,bj).ne.0. _d 0) then
#ifdef ALLOW_THSICE
               if (ICEMASK(i,j,bi,bj).gt.0. _d 0) then
                 tmp=Tsrf(i,j,bi,bj)
                if (snowheight(i,j,bi,bj).gt.3. _d -1) then
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
     &            flwup(i,j,bi,bj), flh(i,j,bi,bj), 
     &            fsh(i,j,bi,bj), df0dT,
     &            ustress(i,j,bi,bj), vstress(i,j,bi,bj),
     &            evap(i,j,bi,bj), savssq(i,j,bi,bj),
     &            iceornot, readwindstress)

C               Note that the LANL flux conventions are opposite
C               of what they are in the model.

cQQ use down long wave data
               flwupnet(i,j,bi,bj)=flwup(i,j,bi,bj)-flw(i,j,bi,bj)
cQQ using down solar, need to have water albedo -- .1
#ifdef ALLOW_THSICE
               if (ICEMASK(i,j,bi,bj).gt.0. _d 0) then
                  call sfc_albedo(ICEHEIGHT(i,j,bi,bj),
     &                               SNOWHEIGHT(i,j,bi,bj),
     &                               Tsrf(i,j,bi,bj),
     &                               sage(i,j,bi,bj), albedo)
               else
                  albedo= 0.1 _d 0
               endif
#else
               albedo= 0.1 _d 0                 
#endif
               fswnet(i,j,bi,bj)=solar(i,j,bi,bj)*(1. _d 0-albedo)
             else
               ustress(i,j,bi,bj) = 0. _d 0
               vstress(i,j,bi,bj) = 0. _d 0
               fsh(i,j,bi,bj)     = 0. _d 0
               flh(i,j,bi,bj)     = 0. _d 0
               flwup(i,j,bi,bj)   = 0. _d 0
               evap(i,j,bi,bj)    = 0. _d 0
               fswnet(i,j,bi,bj)  = 0. _d 0
               savssq(i,j,bi,bj)  = 0. _d 0
             endif
          enddo
         enddo


      if ( .NOT.readwindstress) then
cswd move wind stresses to u and v points
           DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx+1,sNx+Olx
              fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)
     &          *(ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
           DO j = 1-Oly+1,sNy+Oly
            DO i = 1-Olx,sNx+Olx
              fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)
     &          *(vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
      endif

c
c     Add all contributions.
      k = 1
         DO j = 1-Oly,sNy+Oly
          DO i = 1-Olx,sNx+Olx
            if (hFacC(i,j,1,bi,bj).ne.0. _d 0) then
c        Net downward surface heat flux :
              hfl = 0. _d 0
              hfl = hfl + fsh(i,j,bi,bj)
              hfl = hfl + flh(i,j,bi,bj)
              hfl = hfl - flwupnet(i,j,bi,bj)
              hfl = hfl + fswnet(i,j,bi,bj)
c Heat flux:
              Qnet(i,j,bi,bj) = -hfl
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = df0dT
#endif
c Fresh-water flux from Precipitation and Evaporation.
              EmPmR(i,j,bi,bj) = (evap(i,j,bi,bj)-rain(i,j,bi,bj)
     &                           - runoff(i,j,bi,bj))
cccccccccccCHEATccccccccccccccccccccccccc
c            Qnet(i,j,bi,bj) = Qnetch(i,j,bi,bj)
c            EmPmR(i,j,bi,bj) = EmPch(i,j,bi,bj)
cccccccccccccccccccccccccccccccccccccccccc
            else
              Qnet(i,j,bi,bj) = 0. _d 0
              EmPmR(i,j,bi,bj)= 0. _d 0
#ifdef COUPLE_MODEL
              dFdT(i,j,bi,bj) = 0. _d 0
#endif
            endif
          ENDDO
         ENDDO

        IF ( blk_taveFreq.GT.0. _d 0 )
     &    CALL BULKF_AVE(bi,bj,mythid)

C--   end bi,bj loops
       ENDDO
      ENDDO

C--   Update the tile edges.
C jmc: Is it necessary ?
c     _EXCH_XY_R8(Qnet,   mythid)
c     _EXCH_XY_R8(EmPmR,   mythid)
c     CALL EXCH_UV_XY_RS(fu, fv, .TRUE., myThid)

#endif  /*ALLOW_BULK_FORCE*/

      RETURN
      END
