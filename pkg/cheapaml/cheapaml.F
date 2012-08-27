C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml.F,v 1.13 2012/08/27 19:24:02 jmc Exp $
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
c
c       Output:
c       ------
c       ustress, vstress - wind stress
c       Qnet             - net heat flux
c       EmPmR            - net freshwater flux
c       Tair  - mean air temperature (K)  at height ht (m)
c       Qair - Specific humidity kg/kg
c       Cheaptracer - passive tracer
c       ---------
c
c       Input:
c       ------
c       uwind, vwind  - mean wind speed (m/s)
c       Tr - Relaxation profile for Tair on boundaries (C)
c       qr - Relaxation profile for specific humidity (kg/kg)
c       CheaptracerR - Relaxation profile for passive tracer
c     ==================================================================
c     SUBROUTINE cheapaml
c     ==================================================================

      implicit none

c     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
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
        _RL gCheaptracer(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c zonal and meridional transports
        _RL uTrans(1-olx:snx+olx,1-oly:sny+oly)
        _RL vTrans(1-olx:snx+olx,1-oly:sny+oly)
C       AML timestep
        _RL deltaTTracer,deltaTm,ts,xalwu
        _RL dm,pt,fsha,flha,evp,xalwd,xolw,xlwnet
        _RL dtemp,xflu,xfld,dq,dtr
c       _RL Fclouds, ttt2
        _RL q,precip,ssqt,ttt,q100,entrain,cdq

C temp var
        _RL SH_sauv(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
        _RL LH_sauv(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)


C variables for htflux
#ifdef ALLOW_SEAGER
        integer iperx
        integer lsm(snx,sny)

        real slat, salt_fixed,tstep
        real dyd_htf(sny),dxd_htf(snx,sny)
        real sst_htf(snx,sny)
        real cldfr_htf(snx,sny),wspd_htf(snx,sny),u_htf(snx,sny)
        real v_htf(snx,sny)
        real q_htf(snx,sny),t_htf(snx,sny),rlh(snx,sny)
        real sh(snx,sny),qlw(snx,sny)
        real qsw_htf(snx,sny),ppo(snx,sny),qa(snx,sny),th(snx,sny)
        real rh(snx,sny)
        real qisw(snx,sny),ppi(snx,sny),hice(snx,sny),cice(snx,sny)
        real thice(snx,sny),tsnw(snx,sny),qios(snx,sny),brne(snx,sny)
        real rlhi(snx,sny),shi(snx,sny),qlwi(snx,sny),qswi(snx,sny)
        real albedo(snx,sny)
#endif /*  ALLOW_SEAGER */

c useful values
c inverse of time step
        deltaTm=1. _d 0/deltaT

C atmospheric timestep
        deltaTtracer = deltaT/FLOAT(cheapaml_ntim)

#ifdef ALLOW_SEAGER

C initialize array for the seager computation
        slat = ygOrigin
        salt_fixed = 35.0
        iperx = 0
        tstep = deltaT

        DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            DO j = 1,sny
              DO i = 1,snx
C     inputs
                lsm (i,j) = 1-maskC(i,j,1,bi,bj)
                lsm(1,j)   = 1.0
                lsm(snx,j) = 1.0
                lsm(i,1)   = 1.0
                lsm(i,sny) = 1.0
C                if (i.le.100) lsm(i,j) = 1.0


                dyd_htf(j) = delY(j)
                dxd_htf(i,j) = delX(i)
                sst_htf(i,j) = theta(i,j,1,bi,bj) + Celsius2K
                cldfr_htf(i,j) = 0.0 _d 0
                u_htf(i,j) = uwind(i,j,bi,bj)
                v_htf(i,j) = vwind(i,j,bi,bj)
                q_htf(i,j) = qair(i,j,bi,bj)
                t_htf(i,j) = Tair(i,j,bi,bj) + Celsius2K
                qisw(i,j) = solar(i,j,bi,bj)
                ppi(i,j) = 0.0 _d 0
                wspd_htf(i,j) = sqrt(uwind(i,j,bi,bj)**2
     $             + vwind(i,j,bi,bj)**2)

                cice(i,j) = 0.0 _d 0
C je met la temperature de la glace la dedans
                tsnw(i,j) = 0.0 _d 0 + Celsius2K


C     outputs
C                rlh(snx,sny)
C                sh(snx,sny)
C                qlw(snx,sny)
C                qsw_htf(snx,sny)
C                ppo(snx,sny)
C                qa(snx,sny)
C                th(snx,sny)
C                rh(snx,sny)
C                hice(snx,sny)
C                thice(snx,sny)
C                qios(snx,sny)
C                brne(snx,sny)
C                rlhi(snx,sny)
C                shi(snx,sny)
C                qlwi(snx,sny)
C                qswi(snx,sny)
C                albedo(snx,sny) = 0. _d 0
              ENDDO
            ENDDO

c     close bi, bj loops
          ENDDO
        ENDDO


CALL HTFLUX
      call htfluxice(snx,sny,lsm,dxd_htf,dyd_htf,tstep,
     +  sst_htf,cldfr_htf,wspd_htf,u_htf,v_htf,q_htf,t_htf
     $     ,rlh,sh,qlw,qsw_htf,ppo,qa,th,rh,
     +  qisw,ppi,hice,cice,thice,tsnw,qios,brne,rlhi,shi,qlwi,qswi,
     +  iperx,salt_fixed,albedo,slat)


        DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            DO j = 1,sny
              DO i = 1,snx
C     OUTPUT
                if (lsm(i,j).eq.0) then
                qair(i,j,bi,bj) = qa(i,j)
                Tair(i,j,bi,bj) = th(i,j) - Celsius2K
                SH_sauv(i,j,bi,bj) = sh(i,j)
                LH_sauv(i,j,bi,bj) = rlh(i,j)
                else
                qair(i,j,bi,bj) = qr(i,j,bi,bj)
                Tair(i,j,bi,bj) = tr(i,j,bi,bj)
              endif

              ENDDO
            ENDDO

c     close bi, bj loops
          ENDDO
        ENDDO

#else /*  ALLOW_SEAGER */

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
     O           wwind,
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
     O           wwind,
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
     I                     bi, bj,
     U                     qair, gqair,nt,
     I                     myTime, myIter, myThid )

c close bi, bj loops
        enddo
        enddo

c update edges
        _EXCH_XY_RL(qair,mythid)
        endif         ! if use freshwater




        if(useCheapTracer)then
c     do tracer
          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              do j=1-oly,sny+oly
                do i=1-olx,snx+olx
                  gCheaptracer(i,j,bi,bj)=0. _d 0
                  uTrans(i,j)=uwind(i,j,bi,bj)
                  vTrans(i,j)=vwind(i,j,bi,bj)
                enddo
              enddo

              call GAD_2d_CALC_RHS(
     I           bi,bj,1-olx,nsx+olx,1-oly,nsy+oly,
     I           uTrans,vTrans,
     I           uwind, vwind,
     O           wwind,
     I           cheapaml_kdiff, Cheaptracer, zu,
     U           gCheaptracer,deltaTtracer,useFluxLimit,
     I           myTime, myIter, myThid )
              call adams2d(
     I           bi, bj,
     U           gCheaptracer, gCheaptracerm,
     I           nt-1, myIter, myThid )
              call timestep_2d_tracer(
     I           bi, bj, 1-olx,snx+olx ,1-oly, sny+oly,
     I           deltaTtracer,
     I           Cheaptracer, gCheaptracer,
     I           myIter, myThid )
              call cycle_2d_tracer(
     I           bi, bj,
     U           Cheaptracer, gCheaptracer,nt,
     I           myTime, myIter, myThid )

c     close bi, bj loops
            enddo
          enddo

c     update edges
          _EXCH_XY_RL(Cheaptracer,mythid)
        endif                   ! if use tracer




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
              enddo
            enddo
          ENDDO
        ENDDO

        if(useFreshWaterFlux)then
          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              do j=1-oly,sny+oly
                jG = myYGlobalLo-1+(bj-1)*sNy+j
                do i=1-olx,snx+olx
                  iG=myXGlobalLo-1+(bi-1)*sNx+i
                  if(iG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(iG.ge.Nx)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                  if(jG.ge.Ny)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                enddo
              enddo
            ENDDO
          ENDDO
        endif

        if(useCheapTracer)then
          DO bj=myByLo(myThid),myByHi(myThid)
            DO bi=myBxLo(myThid),myBxHi(myThid)
              do j=1-oly,sny+oly
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              do i=1-olx,snx+olx
              iG=myXGlobalLo-1+(bi-1)*sNx+i
              if(iG.le.1) Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              if(iG.ge.Nx)Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              if(jG.le.1) Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              if(jG.ge.Ny)Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              enddo
              enddo
            ENDDO
          ENDDO
        endif


      enddo
c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      CALL PLOT_FIELD_XYRS( qair, 'S/R CHEAPAML qair',1,myThid)

c cycling on short atmospheric time step is now done

c     now continue with diabatic forcing
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              if(FluxFormula.eq.'LANL')then
                call cheapaml_LANL_flux(i,j,bi,bj,fsha,flha,evp,
     &             xolw,ssqt,q100)
              elseif(FluxFormula.eq.'COARE3')then
                call cheapaml_COARE3_flux(i,j,bi,bj,fsha,flha,evp,
     &             xolw,ssqt,q100,cdq)
              endif

c atmospheric upwelled long wave
        ttt=Tair(i,j,bi,bj)-gamma_blk*(cheaphgrid(i,j,bi,bj)-zt)
C        xalwu=stefan*(ttt+Celsius2K)**4*0.5 _d 0
        xalwu=stefan*(0.5*Tair(i,j,bi,bj)+0.5*ttt+Celsius2K)**4*0.5 _d 0
c atmospheric downwelled long wave
        xalwd=stefan*(Tair(i,j,bi,bj)+Celsius2K)**4*0.5 _d 0
c total flux at upper atmospheric layer interface
        xflu=(-solar(i,j,bi,bj)+xalwu+flha)*xef*maskC(i,j,1,bi,bj)
c lower flux calculation.
        xfld=(-solar(i,j,bi,bj)-xalwd+xolw+fsha+flha)
     .*xef*maskC(i,j,1,bi,bj)

        if (usedlongwave) THEN
          xlwnet = xolw-cheapdlongwave(i,j,bi,bj)
        ELSE
C net long wave (see Josey et al. JGR 1997)
C coef lambda replaced by 0.5+lat/230
C convert spec humidity in water vapor pressure (mbar) using coef 1000/0.622=1607.7
          xlwnet = 0.98*stefan*(theta(i,j,1,bi,bj)+273.1)**4.
     $       *(0.39-0.05*sqrt(qair(i,j,bi,bj)*1607.7))
     $       *(1-(0.5+abs(yG(i,j,bi,bj))/230)*cheapclouds(i,j,bi,bj)**2)
     $       + 4*0.98*stefan*(theta(i,j,1,bi,bj)+273.1)**3
     $       *(theta(i,j,1,bi,bj)-Tair(i,j,bi,bj));

C          xlwnet = xolw-stefan*(theta(i,j,1,bi,bj)+273.1)**4.
C     $       *(0.65+11.22*qair(i,j,bi,bj) + 0.25*cheapclouds(i,j,bi,bj)
C     $       -8.23*qair(i,j,bi,bj)*cheapclouds(i,j,bi,bj))
        endif
C clouds
C        ttt2=Tair(i,j,bi,bj)-1.5*gamma_blk*cheaphgrid(i,j,bi,bj)
C        Fclouds = stefan*ttt2**4*(0.4*cheapclouds(i,j,bi,bj)+1-0.4)/2
C        ttt2=Tair(i,j,bi,bj)-3*gamma_blk*cheaphgrid(i,j,bi,bj)+Celsius2K
C        Fclouds = 0.3*stefan*ttt2**4 + 0.22*xolw*cheapclouds(i,j,bi,bj)
c add flux divergences into atmospheric temperature tendency
        gTair(i,j,bi,bj)= (xfld-xflu)/cheaphgrid(i,j,bi,bj)
        Qnet(i,j,bi,bj)=(
     $     -solar(i,j,bi,bj)
C     $     -xalwd
C     $     -Fclouds
C     $     +xolw
     $     +xlwnet
     $     +fsha
     $     +flha
     $     )*maskC(i,j,1,bi,bj)

C save var
        SH_sauv(i,j,bi,bj)= fsha*maskC(i,j,1,bi,bj)
        LH_sauv(i,j,bi,bj)= flha*maskC(i,j,1,bi,bj)


c need to precip?
        if(useFreshwaterFlux)then
        q=q100
c compute saturation specific humidity at atmospheric
c layer top
c first, what is the pressure there?
c ts is surface atmospheric temperature
        ts=Tair(i,j,bi,bj)+gamma_blk*zt+Celsius2K
        pt=p0*(1-gamma_blk*cheaphgrid(i,j,bi,bj)/ts)
     $     **(gravity/gamma_blk/gasR)


c factor to compute rainfall from specific humidity
        dm=100*(p0-pt)*recip_gravity
C     Large scale precip
        precip = 0
        if(wwind(i,j,bi,bj).GT.0.and.q.gt.ssqt*0.7)then
          precip = precip + ((q-ssqt*0.7)*dm/(cheap_pr2))
     $     *(wwind(i,j,bi,bj)/0.75e-5)**2
        endif

C     Convective precip
        if (q>0.0214.and.q.gt.ssqt*0.9) then
          precip = precip + ((q-ssqt*0.9)*dm/cheap_pr1)
        endif

        entrain = cdq*q*0.25

C        gqair(i,j,bi,bj)=(evp-precip-entrain)/cheaphgrid(i,j,bi,bj)
        gqair(i,j,bi,bj)=(evp-entrain)/cheaphgrid(i,j,bi,bj)
     $     /rhoa*maskC(i,j,1,bi,bj)
        EmPmR(i,j,bi,bj)=(evp-precip*1200/cheaphgrid(i,j,bi,bj)
     $     )*maskC(i,j,1,bi,bj)
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
        Tair(i,j,bi,bj)=Tair(i,j,bi,bj)+dtemp*xrelf(i,j,bi,bj)
        enddo
        enddo

c do water
        if(useFreshwaterFlux)then
          call timestep_2d_tracer(
     I       bi, bj,1-olx,snx+olx ,1-oly, sny+oly,
     I       deltaT,
     I       qair,gqair,
     I       myIter, myThid )
          call cycle_2d_tracer(
     I       bi, bj,
     U       qair,gqair,nt,
     I       myTime,myIter,myThid )
c     do implicit time stepping over land and or buffer
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              dq=qr(i,j,bi,bj)-qair(i,j,bi,bj)
              qair(i,j,bi,bj)=qair(i,j,bi,bj)+dq*xrelf(i,j,bi,bj)
              if (qair(i,j,bi,bj).lt.0.0) qair(i,j,bi,bj) = 0.0 _d 0
            enddo
          enddo
        endif

c do tracer
        if(useCheapTracer)then
c     do implicit time stepping over land and or buffer
          do j=1-oly,sny+oly
            do i=1-olx,snx+olx
              dtr=CheaptracerR(i,j,bi,bj)-Cheaptracer(i,j,bi,bj)
      Cheaptracer(i,j,bi,bj)=Cheaptracer(i,j,bi,bj)+dtr*xrelf(i,j,bi,bj)
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

      if(useCheapTracer)then
        _EXCH_XY_RL(Cheaptracer,mythid)
      endif

c reset edges to open boundary profiles

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          do j=1-oly,sny+oly
            jG = myYGlobalLo-1+(bj-1)*sNy+j
            do i=1-olx,snx+olx
              iG=myXGlobalLo-1+(bi-1)*sNx+i
              if(notUsingXPeriodicity)then
              if(iG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
              if(iG.ge.Nx)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
              endif
              if(notUsingYPeriodicity)then
              if(jG.le.1)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
              if(jG.ge.Ny)Tair(i,j,bi,bj)=Tr(i,j,bi,bj)
              endif
            enddo
          enddo
        enddo
      enddo

      if(useFreshWaterFlux)then
        DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            do j=1-oly,sny+oly
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              do i=1-olx,snx+olx
                iG=myXGlobalLo-1+(bi-1)*sNx+i
                if(notUsingXPeriodicity)then
                if(iG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                if(iG.ge.Nx)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                endif
                if(notUsingYPeriodicity)then
                if(jG.le.1)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                if(jG.ge.Ny)qair(i,j,bi,bj)=qr(i,j,bi,bj)
                endif
              enddo
            enddo
          ENDDO
        ENDDO
      endif


      if(useCheapTracer)then
        DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
            do j=1-oly,sny+oly
              jG = myYGlobalLo-1+(bj-1)*sNy+j
              do i=1-olx,snx+olx
                iG=myXGlobalLo-1+(bi-1)*sNx+i
              if(notUsingXPeriodicity)then
              if(iG.le.1) Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              if(iG.ge.Nx)Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              endif
              if(notUsingYPeriodicity)then
              if(jG.le.1) Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              if(jG.ge.Ny)Cheaptracer(i,j,bi,bj)=CheaptracerR(i,j,bi,bj)
              endif
              enddo
            enddo
          ENDDO
        ENDDO
      endif

c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
cswd move wind stresses to u and v points
           DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx+1,sNx+OLx
              fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)
     &          *(ustress(i,j,bi,bj)+ustress(i-1,j,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO
           DO j = 1-OLy+1,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)
     &          *(vstress(i,j,bi,bj)+vstress(i,j-1,bi,bj))*0.5 _d 0
            ENDDO
           ENDDO

C--   end bi,bj loops
       ENDDO
      ENDDO


#endif /*  ALLOW_SEAGER */


#ifdef ALLOW_DIAGNOSTICS
      CALL DIAGNOSTICS_FILL(uwind,  'CH_Uwind',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(vwind,  'CH_Vwind',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(tair,   'CH_TAIR ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(Qnet,   'CH_QNET ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(SH_sauv,'CH_SH   ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(LH_sauv,'CH_LH   ',0,1,0,1,1,myThid)
      IF(useFreshwaterFlux)THEN
        CALL DIAGNOSTICS_FILL(Qair, 'CH_QAIR ',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(EmPmR,'CH_EmP  ',0,1,0,1,1,myThid)
      ENDIF
      IF(useCheapTracer)THEN
          CALL DIAGNOSTICS_FILL(Cheaptracer,'CH_Trace',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx+1,sNx+OLx
C              fu(i,j,bi,bj) = 0.0
C              fv(i,j,bi,bj) = 0.0
C              Qnet(i,j,bi,bj) = 0.0
C              EmPmR(i,j,bi,bj) = 0.0

            ENDDO
          ENDDO
        ENDDO
      ENDDO


      RETURN
      END
