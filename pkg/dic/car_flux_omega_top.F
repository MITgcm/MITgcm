#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: CAR_FLUX_OMEGA_TOP

C !INTERFACE: ==========================================================
      SUBROUTINE CAR_FLUX_OMEGA_TOP( bioac, cflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myTime, myIter, myThid )

C !DESCRIPTION:
C  Calculate carbonate fluxes
C  HERE ONLY HAVE DISSOLUTION WHEN OMEGA < 1.0
C  Karsten Friis and Mick Follows Sep 2004

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C  bioac                :: biological productivity
C  myTime               :: current time
C  myIter               :: current timestep
C  myThid               :: thread number
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER imin, imax, jmin, jmax, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ===================================================
C cflux                :: carbonate flux
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#if ( defined DIC_BIOTIC && defined DIC_CALCITE_SAT )
C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
C  ko                     :: loop-within-loop index
C caexport                :: flux of carbonate from base each "productive"
C                            layer
C depth_u, depth_l        :: depths of upper and lower interfaces
C flux_u, flux_l          :: flux through upper and lower interfaces
       _RL caexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER i,j,k, ko, kBottom
       _RL flux_u, flux_l
C variables for calcium carbonate dissolution
       _RL KierRate
       _RL DissolutionRate
       _RL dumrate

C diagnostics
c     _RL   exp_tot
c     _RL   flx_tot
c     integer knum
c     _RL   omeg_bot
c     _RL   tmp

CEOP
C initialize fluxes
      flux_u = 0. _d 0
      flux_l = 0. _d 0

C to either remineralize in bottom or top layer if flux reaches bottom layer
C      selectCalciteBottomRemin =0 : in bottom layer, =1 : in top layer
C set some nominal particulate sinking rate, try 100m/day:
C       WsinkPIC = 100. _d 0/86400. _d 0
C calculate carbonate flux from base of each nlev
       DO j=jmin,jmax
        DO i=imin,imax
c        exp_tot=0
         do k=1,Nr
            cflux(i,j,k)=0. _d 0
         enddo

         kBottom   = kLowC(i,j,bi,bj)

         DO k=1,nLev
          if (hFacC(i,j,k,bi,bj).gt.0. _d 0) then
           caexport(i,j)= R_CP*rain_ratio(i,j,bi,bj)*bioac(i,j,k)*
     &           (1. _d 0-DOPfraction)*drF(k)*hFacC(i,j,k,bi,bj)
c          exp_tot=exp_tot+caexport(i,j)
C calculate flux to each layer from base of k
           Do ko=k+1,Nr
            if (hFacC(i,j,ko,bi,bj).gt.0. _d 0) then
              if (ko .eq. k+1) then
                flux_u = caexport(i,j)
              else
                flux_u = flux_l
              endif

C flux through lower face of cell
              if (omegaC(i,j,ko,bi,bj) .gt. 1. _d 0) then
                flux_l = flux_u

C if at bottom, remineralize remaining flux
                if (ko.eq.kBottom) then
                  if (selectCalciteBottomRemin.EQ.1) then
C ... at surface
                     cflux(i,j,1)=cflux(i,j,1)+
     &                  ( (flux_l)/(drF(1)*hFacC(i,j,1,bi,bj)) )
                  else

C ... at bottom
                     flux_l=0. _d 0
                  endif
                endif
              else
C if dissolution, then use rate from Kier (1980) Geochem. Cosmochem. Acta
C Kiers dissolution rate in %  per day
                 KierRate = KierRateK*
     &             ((1. _d 0-omegaC(i,j,ko,bi,bj))**KierRateExp)
C convert to per s
C Karsten finds Kier value not in 0/0 after all... therefore drop 100 factor
c                DissolutionRate = KierRate/(100.0*86400.0)
                 DissolutionRate = KierRate/(86400. _d 0)
c                flux_l = flux_u*(1.0-DissolutionRate*drF(k)/WsinkPIC)
C Karstens version
C - gives NaNs (because using kierrate, not dissolution rate)???
c                flux_l = flux_u*(1.0-KierRate)**(drF(k)/WsinkPIC)
C MICKS NEW VERSION... based on vertical sinking/remin balance
                 dumrate = -1. _d 0*DissolutionRate*drF(ko)*
     &                       hFacC(i,j,ko,bi,bj)/WsinkPIC
                 flux_l = flux_u*exp(dumrate)
C TEST ............................
c           if(i .eq. 76 .and. j .eq. 36)then
c            write(6,*)'k,flux_l/flux_u',ko,(flux_l/flux_u)
c            write(6,*)'K, KierRate, drF(k), drF(ko), WsinkPIC,OmegaC'
c            write(6,*)ko,KierRate,drF(k),drF(ko),WsinkPIC,
c    &            omegaC(i,j,ko,bi,bj)
c           endif
C TEST ............................
C no flux to ocean bottom
                 if (ko.eq.kBottom)
     &                      flux_l=0. _d 0
              endif

C flux divergence
             cflux(i,j,ko)=cflux(i,j,ko) +
     &          ( (flux_u-flux_l)/(drF(ko)*hFacC(i,j,ko,bi,bj)) )
C TEST ............................
c            if(i .eq. 76 .and. j .eq. 36)then
c               write(6,*)'k,flux_l/flux_u',ko,(flux_l/flux_u)
c              write(6,*)'k,flux_l,cflux ',ko,flux_l,cflux(i,j,ko)
c            endif
C TEST ............................
           else
C if no layer below initial layer, remineralize
               if (ko.eq.k+1) then
                if ( selectCalciteBottomRemin.EQ.1 .AND.
     &                  omegaC(i,j,k,bi,bj) .GT. 1. _d 0 ) then
C ... at surface
                   cflux(i,j,1)=cflux(i,j,1)
     &                  +bioac(i,j,k)*(1. _d 0-DOPfraction)*
     &                    R_CP*rain_ratio(i,j,bi,bj)
     &                   *drF(k)*hFacC(i,j,k,bi,bj)/
     &                    (drF(1)*hFacC(i,j,1,bi,bj) )
                else
C ... at bottom
                  cflux(i,j,k)=cflux(i,j,k)
     &                  +bioac(i,j,k)*(1. _d 0-DOPfraction)*
     &                    R_CP*rain_ratio(i,j,bi,bj)
                endif
               endif
           endif
          ENDDO

          endif
         ENDDO
C diagnostic
c        flx_tot=0
c        k=0
c        do k=1,nR
c          flx_tot=flx_tot+cflux(i,j,k)*drF(k)*hFacC(i,j,k,bi,bj)
c          if (hFacC(i,j,k,bi,bj).gt.0 _d 0) then
c             knum=k
c             omeg_bot=omegaC(i,j,k,bi,bj)
c          endif
c        enddo
c        if (hFacC(i,j,k,bi,bj).gt. 0. _d 0) then
c         tmp=abs(exp_tot-flx_tot)
c         if (tmp>1 _d -20) then
c          print*,'QQ car_flux', knum,
c    &                 omeg_bot, exp_tot, flx_tot, exp_tot-flx_tot
c         endif
c        endif
C end diagnostic
        ENDDO
       ENDDO
c
#endif /* DIC_BIOTIC and DIC_CALCITE_SAT */
       RETURN
       END
