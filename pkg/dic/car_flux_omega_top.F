#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: CAR_FLUX

C !INTERFACE: ==========================================================
      SUBROUTINE CAR_FLUX_OMEGA_TOP( bioac, cflux,
     I           bi,bj,imin,imax,jmin,jmax,
     I           myIter,myTime,myThid)

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
#include "DIC_BIOTIC.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
C  bioac                :: biological productivity
      INTEGER myIter
      _RL myTime
      INTEGER myThid
      _RL  bioac(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
       INTEGER imin, imax, jmin, jmax, bi, bj

C !OUTPUT PARAMETERS: ===================================================
C cflux                :: carbonate flux
      _RL  cflux(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_PTRACERS
#ifdef DIC_BIOTIC

C !LOCAL VARIABLES: ====================================================
C  i,j,k                  :: loop indices
c  ko                     :: loop-within-loop index
c caexport                :: flux of carbonate from base each "productive"
c                            layer
c depth_u, depth_l        :: depths of upper and lower interfaces
c flux_u, flux_l          :: flux through upper and lower interfaces
       _RL caexport(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
       INTEGER I,J,k, ko
       _RL flux_u, flux_l
c variables for calcium carbonate dissolution
       _RL KierRate
       _RL DissolutionRate
       _RL WsinkPIC
       INTEGER iflx
       _RL dumrate

c diagnostics
c     _RL   exp_tot
c     _RL   flx_tot
c     integer knum
c     _RL   omeg_bot
c     _RL   tmp


CEOP

c flag to either remineralize in bottom or top layer if flux
c reaches bottom layer 0=bottom, 1=top
      iflx=1
c set some nominal particulate sinking rate
c try 100m/day
       WsinkPIC = 100/86400.0
c calculate carbonate flux from base of each nlev
       DO j=jmin,jmax
        DO i=imin,imax
c        exp_tot=0
         do k=1,nR
            cflux(i,j,k)=0.d0
         enddo
         DO k=1,nLev
          if (hFacC(i,j,k,bi,bj).gt.0.d0) then
           caexport(i,j)= R_cp*rain_ratio(i,j,bi,bj)*bioac(i,j,k)*
     &           (1.0-DOPfraction)*drF(k)*hFacC(i,j,k,bi,bj)
c          exp_tot=exp_tot+caexport(i,j)
c calculate flux to each layer from base of k
           Do ko=k+1,Nr
            if (hFacC(i,j,ko,bi,bj).gt.0.d0) then
              if (ko .eq. k+1) then
                flux_u = caexport(i,j)
              else
                flux_u = flux_l
              endif



C flux through lower face of cell
              if (omegaC(i,j,ko,bi,bj) .gt. 1.0) then
                flux_l = flux_u

c if at bottom, remineralize remaining flux
                if (ko.eq.Nr.or.hFacC(i,j,ko+1,bi,bj).eq.0.d0) then
                  if (iflx.eq.1) then
c ... at surface
                     cflux(i,j,1)=cflux(i,j,1)+
     &                  ( (flux_l)/(drF(1)*hFacC(i,j,1,bi,bj)) )
                  else

c ... at bottom
                     flux_l=0.d0
                  endif
                endif
              else
c if dissolution, then use rate from Kier (1980) Geochem. Cosmochem. Acta
c Kiers dissolution rate in %  per day
                 KierRate = 7.177* ((1.0-omegaC(i,j,ko,bi,bj))**4.54)
c convert to per s
c Karsten finds Kier value not in 0/0 after all... therefore drop 100 factor
c                DissolutionRate = KierRate/(100.0*86400.0)
                 DissolutionRate = KierRate/(86400.0)
c                flux_l = flux_u*(1.0-DissolutionRate*drF(k)/WsinkPIC)
c Karstens version
c - gives NaNs (because using kierrate, not dissolution rate)???
c                flux_l = flux_u*(1.0-KierRate)**(drF(k)/WsinkPIC)
c MICKS NEW VERSION... based on vertical sinking/remin balance
                 dumrate = -1.0d0*DissolutionRate*drF(ko)*
     &                       hFacC(i,j,ko,bi,bj)/WsinkPIC
                 flux_l = flux_u*exp(dumrate)
c TEST ............................
c           if(i .eq. 76 .and. j .eq. 36)then
c            write(6,*)'k,flux_l/flux_u',ko,(flux_l/flux_u)
c            write(6,*)'K, KierRate, drF(k), drF(ko), WsinkPIC,OmegaC'
c            write(6,*)ko,KierRate,drF(k),drF(ko),WsinkPIC,
c    &            omegaC(i,j,ko,bi,bj)
c           endif
c TEST ............................
c no flux to ocean bottom
                 if (ko.eq.Nr.or.hFacC(i,j,ko+1,bi,bj).eq.0.d0)
     &                      flux_l=0.d0
              endif

c flux divergence
             cflux(i,j,ko)=cflux(i,j,ko) +
     &          ( (flux_u-flux_l)/(drF(ko)*hFacC(i,j,ko,bi,bj)) )
c TEST ............................
c            if(i .eq. 76 .and. j .eq. 36)then
c               write(6,*)'k,flux_l/flux_u',ko,(flux_l/flux_u)
c              write(6,*)'k,flux_l,cflux ',ko,flux_l,cflux(i,j,ko)
c            endif
c TEST ............................
           else
c if no layer below initial layer, remineralize
               if (ko.eq.k+1) then
                if (iflx.eq.1.and.omegaC(i,j,k,bi,bj) .gt. 1.d0) then
c ... at surface
                   cflux(i,j,1)=cflux(i,j,1)
     &                  +bioac(i,j,k)*(1.0-DOPfraction)*
     &                    R_cp*rain_ratio(i,j,bi,bj)
     &                   *drF(k)*hFacC(i,j,k,bi,bj)/
     &                    (drF(1)*hFacC(i,j,1,bi,bj) )
                else
c ... at bottom
                  cflux(i,j,k)=cflux(i,j,k)
     &                  +bioac(i,j,k)*(1.0-DOPfraction)*
     &                    R_cp*rain_ratio(i,j,bi,bj)
                endif
               endif
           endif
          ENDDO

          endif
         ENDDO
c diagnostic
c        flx_tot=0
c        k=0
c        do k=1,nR
c          flx_tot=flx_tot+cflux(i,j,k)*drF(k)*hFacC(i,j,k,bi,bj)
c          if (hFacC(i,j,k,bi,bj).gt.0) then
c             knum=k
c             omeg_bot=omegaC(i,j,k,bi,bj)
c          endif
c        enddo
c        if (hFacC(i,j,k,bi,bj).gt.0) then
c         tmp=abs(exp_tot-flx_tot)
c         if (tmp>1e-20) then
c          print*,'QQ car_flux', knum,
c    &                 omeg_bot, exp_tot, flx_tot, exp_tot-flx_tot
c         endif
c        endif
c end diagnostic
        ENDDO
       ENDDO
c
#endif
#endif
       RETURN
       END
