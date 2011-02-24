C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml.F,v 1.9 2011/02/24 20:21:42 wienders Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#undef ALLOW_THSICE

      subroutine cheapaml(
     I                           myTime, myIter, mythid )

c     ==================================================================
c     SUBROUTINE cheapaml
c     ==================================================================
c
c     o Get the surface fluxes used to force ocean model
c       Output:
c       ------
c       ustress, vstress - wind stress
c       Qnet             - net heat flux
c       EmPmR            - net freshwater flux
c       Tair  - mean air temperature (K)  at height ht (m)
c       Qair - Specific humidity kg/kg
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)
c     Tr - Relaxation profile for Tair on boundaries (C)
c     qr - Relaxation profile for specific humidity (kg/kg)
c     ==================================================================
c     SUBROUTINE cheapaml
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
#include "CHEAPAML.h"

c     == routine arguments ==

      integer mythid
      integer myIter
      _RL     myTime

C     == Local variables ==
      integer bi,bj,iG,jG
      integer i,j,nt

c tendencies of atmospheric temperature, current and past
        _RL gTair(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
        _RL gqair(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c zonal and meridional transports
        _RL uTrans(1-olx:snx+olx,1-oly:sny+oly)
        _RL vTrans(1-olx:snx+olx,1-oly:sny+oly)
C       AML timestep
        _RL deltaTTracer,deltaTm,ts,xalwu
        _RL dm,pt,fsha,flha,evp,xalwd,xolw
        _RL dtemp,xflu,xfld,xrelf,dq
        _RL q,precip,ssqt,ttt,q100

c useful values
c inverse of time step
        deltaTm=1. _d 0/deltaT

c relaxation time scales from input
        xrelf=1. _d 0/(1. _d 0+xgs*deltaT)

C atmospheric timestep
        deltaTtracer = deltaT/FLOAT(cheapaml_ntim)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
c initialize net heat flux and fresh water flux arrays
          DO j = 1-oly,sny+oly
           DO i = 1-olx,snx+olx
              Qnet(i,j,bi,bj)=0. _d 0
              EmPmR(i,j,bi,bj)=0. _d 0
           ENDDO
          ENDDO

c close bi, bj loops
         ENDDO
        ENDDO

c this is a reprogramming to speed up cheapaml
c the short atmospheric time step is applied to
c advection and diffusion only.  diabatic forcing is computed
c once and used for the entire oceanic time step.

c cycle through atmospheric advective/diffusive
c surface temperature evolution

        do nt=1,cheapaml_ntim
                
           DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)

c compute advective and diffusive flux divergence

         do j=1-oly,sny+oly
         do i=1-olx,snx+olx
         gTair(i,j,bi,bj)=0. _d 0
         uTrans(i,j)=uwind(i,j,bi,bj)
         vTrans(i,j)=vwind(i,j,bi,bj)
         enddo
         enddo
         call GAD_2d_CALC_RHS(
     I           bi,bj,1-olx,nsx+olx,1-oly,nsy+oly,
     I           uTrans,vTrans,
     I           uwind, vwind,
     I           cheapaml_kdiff, Tair, zu,
     U           gTair,deltaTtracer,useFluxLimit,
     I           myTime, myIter, myThid )

c       CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
        call adams2d(
     I                     bi, bj,
     U                     gTair, gTairm,
     I                     nt-1, myIter, myThid )
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)

        call timestep_2d_tracer(
     I                     bi, bj, 1-olx,snx+olx ,1-oly, sny+oly,
     I                     deltaTtracer,
     I                     Tair, gTair,
     I                     myIter, myThid )
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)

        call cycle_2d_tracer(
     I                   bi, bj,
     U                   Tair, gTair,nt,
     I                   myTime, myIter, myThid )
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c close bi,bj loops
        enddo
        enddo
c update edges

         _EXCH_XY_RL(Tair,mythid)

        if(useFreshwaterFlux)then
c do water
           DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1-oly,sny+oly
          do i=1-olx,snx+olx
          gqair(i,j,bi,bj)=0. _d 0
         uTrans(i,j)=uwind(i,j,bi,bj)
         vTrans(i,j)=vwind(i,j,bi,bj)
          enddo
          enddo
        
         call GAD_2d_CALC_RHS(
     I           bi,bj,1-olx,nsx+olx,1-oly,nsy+oly,
     I           uTrans,vTrans,
     I           uwind, vwind,
     I           cheapaml_kdiff, qair, zu,
     U           gqair,deltaTtracer,useFluxLimit,
     I           myTime, myIter, myThid )
         call adams2d(
     I                     bi, bj,
     U                     gqair, gqairm,
     I                     nt-1, myIter, myThid )
         call timestep_2d_tracer(
     I                     bi, bj, 1-olx,snx+olx ,1-oly, sny+oly,
     I                     deltaTtracer,
     I                     qair, gqair,
     I                     myIter, myThid )
         call cycle_2d_tracer(
     I                   bi, bj,
     U                   qair, gqair,nt,
     I                   myTime, myIter, myThid )
                                       
c close bi, bj loops
        enddo
        enddo

c update edges
        _EXCH_XY_RL(qair,mythid)
        endif         ! if use freshwater

c reset boundaries to open boundary profile

        DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            do j=1-oly,sny+oly
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              do i=1-olx,snx+olx
                iG=myXGlobalLo-1+(bi-1)*sNx+i
                if(iG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                if(iG.ge.Nx)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                if(jG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                if(jG.ge.Ny)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                if(useFreshWaterFlux)then
                  if(iG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(iG.ge.Nx)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.ge.Ny)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                endif
              enddo
            enddo
          ENDDO
        ENDDO
        
        enddo
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      CALL PLOT_FIELD_XYRS( qair, 'S/R CHEAPAML qair',1,myThid)

c cycling on short atmospheric time step is now done

c now continue with diabatic forcing
       DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        do j=1-oly,sny+oly
        do i=1-olx,snx+olx
        if(FluxFormula.eq.'LANL')then
        call cheapaml_LANL_flux(i,j,bi,bj,fsha,flha,evp,
     &xolw,ssqt,q100)
        elseif(FluxFormula.eq.'COARE3')then
        call cheapaml_COARE3_flux(i,j,bi,bj,fsha,flha,evp,
     &xolw,ssqt,q100)
        endif

c atmospheric upwelled long wave
        ttt=Tair(i,j,bi,bj)-gamma_blk*(cheapaml_h-zt)
        xalwu=stefan*(ttt+Celsius2K)**4*0.5 _d 0
c atmospheric downwelled long wave
        xalwd=stefan*(Tair(i,j,bi,bj)+Celsius2K)**4*0.5 _d 0
c total flux at upper atmospheric layer interface
        xflu=(-solar(i,j,bi,bj)+xalwu+flha)*xef*maskC(i,j,1,bi,bj)
c lower flux calculation. 
        xfld=(-solar(i,j,bi,bj)-xalwd+xolw+fsha+flha)
     .*xef*maskC(i,j,1,bi,bj)
c add flux divergences into atmospheric temperature tendency
        gTair(i,j,bi,bj)=(xfld-xflu)*hm
        Qnet(i,j,bi,bj)=(
     $     -solar(i,j,bi,bj)
     $     -xalwd
     $     +xolw
     $     +fsha
     $     +flha
     $     )*maskC(i,j,1,bi,bj)

c need to precip?
        if(useFreshwaterFlux)then
        q=q100
c compute saturation specific humidity at atmospheric
c layer top
c first, what is the pressure there?
c ts is surface atmospheric temperature
        ts=Tair(i,j,bi,bj)+gamma_blk*zt+Celsius2K
        pt=p0*(1-gamma_blk*cheapaml_h/ts)**(gravity/gamma_blk/gasR)
        if(q.gt.ssqt)then
c factor to compute rainfall from specific humidity
        dm=(p0-pt)*recip_gravity
        precip=(q-ssqt)*dm*deltaTm
        else
        precip=0. _d 0
        endif
        gqair(i,j,bi,bj)=(evp-precip)*hm/rhoa*maskC(i,j,1,bi,bj)
        EmPmR(i,j,bi,bj)=(evp-precip)*maskC(i,j,1,bi,bj)
        endif

c       CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
        enddo
        enddo

c it is not necessary to use the Adams2d subroutine as
c the forcing is always computed at the current time step. 

        call timestep_2d_tracer(
     I                     bi, bj, 1-olx,snx+olx ,1-oly, sny+oly,
     I                     deltaT,
     I                     Tair, gTair,
     I                     myIter, myThid )
c full oceanic time step deltaT is used in the above

        call cycle_2d_tracer(
     I                   bi, bj,
     U                   Tair, gTair,nt,
     I                   myTime, myIter, myThid )

c       do implicit time stepping over land
        do j=1-oly,sny+oly
        do i=1-olx,snx+olx
        dtemp=tr(i,j,bi,bj)-Tair(i,j,bi,bj)
        Tair(i,j,bi,bj)=Tair(i,j,bi,bj)+xgs*deltaT*dtemp
     .  *xrelf*(1. _d 0-maskC(i,j,1,bi,bj)+Cheapmask(i,j,bi,bj))
        enddo
        enddo

c do water
        if(useFreshwaterFlux)then
        call timestep_2d_tracer(
     I                     bi, bj,1-olx,snx+olx ,1-oly, sny+oly,
     I                     deltaT,
     I                     qair,gqair,
     I                     myIter, myThid )
        call cycle_2d_tracer(
     I                       bi, bj, 
     U                       qair,gqair,nt,
     I                       myTime,myIter,myThid )
c       do implicit time stepping over land and or buffer
        do j=1-oly,sny+oly
        do i=1-olx,snx+olx
        dq=qr(i,j,bi,bj)-qair(i,j,bi,bj)
        qair(i,j,bi,bj)=qair(i,j,bi,bj)+xgs*deltaT*dq
     .  *xrelf*(1. _d 0-maskC(i,j,1,bi,bj)+Cheapmask(i,j,bi,bj))
        if (qair(i,j,bi,bj).lt.0.0) qair(i,j,bi,bj) = 0.0 _d 0
        enddo
        enddo
        endif

c close bi,bj loops
        enddo
        enddo

c update edges


         _EXCH_XY_RL(Tair,mythid)
         _EXCH_XY_RS(Qnet,mythid)
      if(useFreshwaterFlux)then
         _EXCH_XY_RL(qair,mythid)
         _EXCH_XY_RS(EmPmR,mythid)
      endif

c reset edges to open boundary profiles

           DO bj=myByLo(myThid),myByHi(myThid)
           DO bi=myBxLo(myThid),myBxHi(myThid)
                do j=1-oly,sny+oly
                  jG = myYGlobalLo-1+(bj-1)*sNy+j
                do i=1-olx,snx+olx
                  iG=myXGlobalLo-1+(bi-1)*sNx+i
                  if(iG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                  if(iG.ge.Nx)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                  if(jG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                  if(jG.ge.Ny)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
                  if(useFreshWaterFlux)then
                  if(iG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(iG.ge.Nx)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.ge.Ny)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  endif
                enddo
                enddo
          enddo
          enddo

c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
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

c     &    CALL BULKF_AVE(bi,bj,mythid)

C--   end bi,bj loops
       ENDDO
      ENDDO
      
#ifdef ALLOW_DIAGNOSTICS
      CALL DIAGNOSTICS_FILL(uwind,'CH_Uwind',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(vwind,'CH_Vwind',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(tair,'CH_TAIR ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(Qnet,'CH_QNET ',0,1,0,1,1,myThid)
      if(useFreshwaterFlux)then
        CALL DIAGNOSTICS_FILL(Qair,'CH_QAIR ',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(EmPmR,'CH_EmP  ',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      

      RETURN
      END
