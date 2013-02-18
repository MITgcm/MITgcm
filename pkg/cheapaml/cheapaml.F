C $Header: /u/gcmpack/MITgcm/pkg/cheapaml/cheapaml.F,v 1.20 2013/02/18 21:17:50 jmc Exp $
C $Name:  $

#include "CHEAPAML_OPTIONS.h"
#undef ALLOW_THSICE

      SUBROUTINE CHEAPAML(
     I                           myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE cheapaml
C     ==================================================================
C
C     o Get the surface fluxes used to force ocean model
C
C       Output:
C       ------
C       ustress, vstress - wind stress
C       Qnet             - net heat flux
C       EmPmR            - net freshwater flux
C       Tair  - mean air temperature (K)  at height ht (m)
C       Qair - Specific humidity kg/kg
C       Cheaptracer - passive tracer
C       ---------
C
C       Input:
C       ------
C       uwind, vwind  - mean wind speed (m/s)
C       Tr - Relaxation profile for Tair on boundaries (C)
C       qr - Relaxation profile for specific humidity (kg/kg)
C       CheaptracerR - Relaxation profile for passive tracer
C     ==================================================================
C     SUBROUTINE cheapaml
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==
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

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER mythid

C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j, nt, startAB
      LOGICAL writeDbug
      CHARACTER*10 sufx
      LOGICAL xIsPeriodic, yIsPeriodic

C tendencies of atmospheric temperature, current and past
        _RL gTair(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
        _RL gqair(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
        _RL gCheaptracer(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C zonal and meridional transports
        _RL uTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL vTrans(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C       AML timestep
        _RL deltaTTracer,deltaTm,ts,xalwu
        _RL dm,pt,fsha,flha,evp,xalwd,xolw,xlwnet
        _RL dtemp,xflu,xfld,dq,dtr
c       _RL Fclouds, ttt2
        _RL q,precip,ssqt,ttt,q100,entrain,cdq

C temp var
        _RL SH_sauv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
        _RL LH_sauv(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

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

C useful values
C inverse of time step
        deltaTm=1. _d 0/deltaT

C atmospheric timestep
        deltaTtracer = deltaT/FLOAT(cheapaml_ntim)

c       writeDbug = debugLevel.GE.debLevC .AND.
c     &             DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock)
        writeDbug = debugLevel.GE.debLevC .AND. diagFreq.GT.0.

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
c                if (i.le.100) lsm(i,j) = 1.0

                dyd_htf(j) = delY(j)
                dxd_htf(i,j) = delX(i)
                sst_htf(i,j) = theta(i,j,1,bi,bj) + celsius2K
                cldfr_htf(i,j) = 0.0 _d 0
                u_htf(i,j) = uwind(i,j,bi,bj)
                v_htf(i,j) = vwind(i,j,bi,bj)
                q_htf(i,j) = qair(i,j,bi,bj)
                t_htf(i,j) = Tair(i,j,bi,bj) + celsius2K
                qisw(i,j) = solar(i,j,bi,bj)
                ppi(i,j) = 0.0 _d 0
                wspd_htf(i,j) = sqrt(uwind(i,j,bi,bj)**2
     &             + vwind(i,j,bi,bj)**2)

                cice(i,j) = 0.0 _d 0
C je met la temperature de la glace la dedans
                tsnw(i,j) = 0.0 _d 0 + celsius2K

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

C     close bi, bj loops
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
                Tair(i,j,bi,bj) = th(i,j) - celsius2K
                SH_sauv(i,j,bi,bj) = sh(i,j)
                LH_sauv(i,j,bi,bj) = rlh(i,j)
                else
                qair(i,j,bi,bj) = qr(i,j,bi,bj)
                Tair(i,j,bi,bj) = tr(i,j,bi,bj)
              endif

              ENDDO
            ENDDO

C     close bi, bj loops
          ENDDO
        ENDDO

#else /*  ALLOW_SEAGER */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C initialize net heat flux and fresh water flux arrays
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            Qnet(i,j,bi,bj) = 0. _d 0
            EmPmR(i,j,bi,bj)= 0. _d 0
          ENDDO
         ENDDO
       ENDDO
      ENDDO

C this is a reprogramming to speed up cheapaml
C the short atmospheric time step is applied to
C advection and diffusion only.  diabatic forcing is computed
C once and used for the entire oceanic time step.

C cycle through atmospheric advective/diffusive
C surface temperature evolution

      DO nt=1,cheapaml_ntim

        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)

C compute advective and diffusive flux divergence
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             gTair(i,j,bi,bj)=0. _d 0
             uTrans(i,j)=uwind(i,j,bi,bj)
             vTrans(i,j)=vwind(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL GAD_2d_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uwind, vwind,
     I           cheapaml_kdiff, Tair,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wwind,
     U           gTair,
     I           myTime, myIter, myThid )
c        CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
         startAB = cheapTairStartAB + nt - 1
         CALL adams2d(
     I           bi, bj,
     U           gTair, gTairm,
     I           startAB, myIter, myThid )
c        CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c        CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
         CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gTair,
     U           Tair,
     I           nt, myIter, myThid )
c        CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c        CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
C close bi,bj loops
         ENDDO
        ENDDO
C update edges
        _EXCH_XY_RL(Tair,mythid)

       IF (useFreshWaterFlux) THEN
C do water
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             gqair(i,j,bi,bj)=0. _d 0
             uTrans(i,j)=uwind(i,j,bi,bj)
             vTrans(i,j)=vwind(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL GAD_2d_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uwind, vwind,
     I           cheapaml_kdiff, qair,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wwind,
     U           gqair,
     I           myTime, myIter, myThid )
          startAB = cheapTairStartAB + nt - 1
          CALL adams2d(
     I           bi, bj,
     U           gqair, gqairm,
     I           startAB, myIter, myThid )
          CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gqair,
     U           qair,
     I           nt, myIter, myThid )
C close bi, bj loops
         ENDDO
        ENDDO
C update edges
        _EXCH_XY_RL(qair,mythid)
       ENDIF         ! if use freshwater

       IF (useCheapTracer) THEN
C     do tracer
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             gCheaptracer(i,j,bi,bj)=0. _d 0
             uTrans(i,j)=uwind(i,j,bi,bj)
             vTrans(i,j)=vwind(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL GAD_2d_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uwind, vwind,
     I           cheapaml_kdiff, Cheaptracer,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wwind,
     U           gCheaptracer,
     I           myTime, myIter, myThid )
          startAB = cheapTracStartAB + nt - 1
          CALL adams2d(
     I           bi, bj,
     U           gCheaptracer, gCheaptracerm,
     I                 startAB, myIter, myThid )
          CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gCheaptracer,
     U           Cheaptracer,
     I           nt, myIter, myThid )
C     close bi, bj loops
         ENDDO
        ENDDO
C     update edges
        _EXCH_XY_RL(Cheaptracer,mythid)
       ENDIF                   ! if use tracer

C reset boundaries to open boundary profile
       IF ( .NOT.(cheapamlXperiodic.AND.cheapamlXperiodic) ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL CHEAPAML_COPY_EDGES(
     I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   Tr(1-OLx,1-OLy,bi,bj),
     U                   Tair(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          IF (useFreshWaterFlux) THEN
           CALL CHEAPAML_COPY_EDGES(
     I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   qr(1-OLx,1-OLy,bi,bj),
     U                   qair(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          ENDIF
          IF (useCheapTracer) THEN
           CALL CHEAPAML_COPY_EDGES(
     I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   CheaptracerR(1-OLx,1-OLy,bi,bj),
     U                   Cheaptracer(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          ENDIF
         ENDDO
        ENDDO
       ENDIF

C--   end loop on nt (short time-step loop)
      ENDDO
      IF ( writeDbug ) THEN
       WRITE(sufx,'(I10.10)') myIter
       CALL WRITE_FLD_XY_RL('tAir_afAdv.', sufx, Tair, myIter, myThid )
       IF (useFreshWaterFlux)
     & CALL WRITE_FLD_XY_RL('qAir_afAdv.', sufx, qair, myIter, myThid )
      ENDIF

C cycling on short atmospheric time step is now done

C     now continue with diabatic forcing
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            IF (FluxFormula.EQ.'LANL') THEN
              CALL cheapaml_LANL_flux(i,j,bi,bj,fsha,flha,evp,
     &             xolw,ssqt,q100)
            ELSEIF (FluxFormula.EQ.'COARE3') THEN
              CALL cheapaml_COARE3_flux(i,j,bi,bj,fsha,flha,evp,
     &             xolw,ssqt,q100,cdq)
            ENDIF

C atmospheric upwelled long wave
        ttt=Tair(i,j,bi,bj)-gamma_blk*(cheaphgrid(i,j,bi,bj)-zt)
C        xalwu=stefan*(ttt+celsius2K)**4*0.5 _d 0
        xalwu=stefan*(0.5*Tair(i,j,bi,bj)+0.5*ttt+celsius2K)**4*0.5 _d 0
C atmospheric downwelled long wave
        xalwd=stefan*(Tair(i,j,bi,bj)+celsius2K)**4*0.5 _d 0
C total flux at upper atmospheric layer interface
        xflu=(-solar(i,j,bi,bj)+xalwu+flha)*xef*maskC(i,j,1,bi,bj)
C lower flux calculation.
        xfld=(-solar(i,j,bi,bj)-xalwd+xolw+fsha+flha)
     &      *xef*maskC(i,j,1,bi,bj)

        IF (useDLongWave) THEN
          xlwnet = xolw-cheapdlongwave(i,j,bi,bj)
        ELSE
C net long wave (see Josey et al. JGR 1997)
C coef lambda replaced by 0.5+lat/230
C convert spec humidity in water vapor pressure (mbar) using coef 1000/0.622=1607.7
          xlwnet = 0.98 _d 0*stefan*(theta(i,j,1,bi,bj)+celsius2K)**4
     &       *(0.39 _d 0 - 0.05 _d 0*SQRT(qair(i,j,bi,bj)*1607.7 _d 0))
     &       *( oneRL - (halfRL+ABS(yG(i,j,bi,bj))/230. _d 0)
     &                 *cheapclouds(i,j,bi,bj)**2 )
     &       + 4.0*0.98 _d 0*stefan*(theta(i,j,1,bi,bj)+celsius2K)**3
     &       *(theta(i,j,1,bi,bj)-Tair(i,j,bi,bj))

c          xlwnet = xolw-stefan*(theta(i,j,1,bi,bj)+celsius2K)**4.
c     &       *(0.65+11.22*qair(i,j,bi,bj) + 0.25*cheapclouds(i,j,bi,bj)
c     &       -8.23*qair(i,j,bi,bj)*cheapclouds(i,j,bi,bj))
        ENDIF
C clouds
c        ttt2=Tair(i,j,bi,bj)-1.5*gamma_blk*cheaphgrid(i,j,bi,bj)
c        Fclouds = stefan*ttt2**4*(0.4*cheapclouds(i,j,bi,bj)+1-0.4)/2
c        ttt2=Tair(i,j,bi,bj)-3*gamma_blk*cheaphgrid(i,j,bi,bj)+celsius2K
c        Fclouds = 0.3*stefan*ttt2**4 + 0.22*xolw*cheapclouds(i,j,bi,bj)
C add flux divergences into atmospheric temperature tendency
        gTair(i,j,bi,bj)= (xfld-xflu)/cheaphgrid(i,j,bi,bj)
        Qnet(i,j,bi,bj)=(
     &     -solar(i,j,bi,bj)
C     &     -xalwd
C     &     -Fclouds
C     &     +xolw
     &     +xlwnet
     &     +fsha
     &     +flha
     &     )*maskC(i,j,1,bi,bj)

C save var
        SH_sauv(i,j,bi,bj)= fsha*maskC(i,j,1,bi,bj)
        LH_sauv(i,j,bi,bj)= flha*maskC(i,j,1,bi,bj)

C need to precip?
        IF (useFreshWaterFlux) THEN
          q=q100
C compute saturation specific humidity at atmospheric
C layer top
C first, what is the pressure there?
C ts is surface atmospheric temperature
          ts=Tair(i,j,bi,bj)+gamma_blk*zt+celsius2K
          pt=p0*(1-gamma_blk*cheaphgrid(i,j,bi,bj)/ts)
     &       **(gravity/gamma_blk/gasR)

C factor to compute rainfall from specific humidity
          dm=100.*(p0-pt)*recip_gravity
C     Large scale precip
          precip = 0.
          IF (wwind(i,j,bi,bj).GT.0. .AND. q.GT.ssqt*0.7 _d 0) THEN
            precip = precip + ( (q-ssqt*0.7 _d 0)*dm/cheap_pr2 )
     &                       *(wwind(i,j,bi,bj)/0.75 _d -5)**2
          ENDIF

C     Convective precip
          IF (q.GT.0.0214 _d 0 .AND. q.GT.ssqt*0.9 _d 0) THEN
            precip = precip + ((q-ssqt*0.9 _d 0)*dm/cheap_pr1)
          ENDIF

          entrain = cdq*q*0.25

c        gqair(i,j,bi,bj)=(evp-precip-entrain)/cheaphgrid(i,j,bi,bj)
          gqair(i,j,bi,bj) = (evp-entrain)/cheaphgrid(i,j,bi,bj)
     &                      /rhoa*maskC(i,j,1,bi,bj)
          EmPmR(i,j,bi,bj) = (evp-precip*1200/cheaphgrid(i,j,bi,bj)
     &                       )*maskC(i,j,1,bi,bj)
        ENDIF

c       CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
          ENDDO
         ENDDO

C it is not necessary to use the Adams2d subroutine as
C the forcing is always computed at the current time step.
C note: full oceanic time step deltaT is used below
         CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaT,
     I           gTair,
     U           Tair,
     I           0, myIter, myThid )
C       do implicit time stepping over land
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dtemp=tr(i,j,bi,bj)-Tair(i,j,bi,bj)
            Tair(i,j,bi,bj)=Tair(i,j,bi,bj)+dtemp*xrelf(i,j,bi,bj)
          ENDDO
         ENDDO

C do water
        IF (useFreshWaterFlux) THEN
         CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaT,
     I           gqair,
     U           qair,
     I           0, myIter, myThid )
C     do implicit time stepping over land and or buffer
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dq=qr(i,j,bi,bj)-qair(i,j,bi,bj)
            qair(i,j,bi,bj)=qair(i,j,bi,bj)+dq*xrelf(i,j,bi,bj)
            IF (qair(i,j,bi,bj).LT.0.0) qair(i,j,bi,bj) = 0.0 _d 0
          ENDDO
         ENDDO
        ENDIF

C do tracer
        IF (useCheapTracer) THEN
C     do implicit time stepping over land and or buffer
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dtr=CheaptracerR(i,j,bi,bj)-Cheaptracer(i,j,bi,bj)
            Cheaptracer(i,j,bi,bj) = Cheaptracer(i,j,bi,bj)
     &                             + dtr*xrelf(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDIF

C close bi,bj loops
       ENDDO
      ENDDO

C update edges
       _EXCH_XY_RL(Tair,mythid)
       _EXCH_XY_RS(Qnet,mythid)
      IF (useFreshWaterFlux) THEN
       _EXCH_XY_RL(qair,mythid)
       _EXCH_XY_RS(EmPmR,mythid)
      ENDIF
      IF (useCheapTracer) THEN
       _EXCH_XY_RL(Cheaptracer,mythid)
      ENDIF

C reset edges to open boundary profiles
c     IF ( .NOT.(cheapamlXperiodic.AND.cheapamlXperiodic) ) THEN
      IF ( notUsingXPeriodicity.OR.notUsingYPeriodicity ) THEN
        xIsPeriodic = .NOT.notUsingXPeriodicity
        yIsPeriodic = .NOT.notUsingYPeriodicity
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           CALL CHEAPAML_COPY_EDGES(
c    I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   xIsPeriodic, yIsPeriodic,
     I                   Tr(1-OLx,1-OLy,bi,bj),
     U                   Tair(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          IF (useFreshWaterFlux) THEN
           CALL CHEAPAML_COPY_EDGES(
c    I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   xIsPeriodic, yIsPeriodic,
     I                   qr(1-OLx,1-OLy,bi,bj),
     U                   qair(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          ENDIF
          IF (useCheapTracer) THEN
           CALL CHEAPAML_COPY_EDGES(
c    I                   cheapamlXperiodic, cheapamlYperiodic,
     I                   xIsPeriodic, yIsPeriodic,
     I                   CheaptracerR(1-OLx,1-OLy,bi,bj),
     U                   Cheaptracer(1-OLx,1-OLy,bi,bj),
     I                   bi, bj, myIter, myThid )
          ENDIF
         ENDDO
        ENDDO
      ENDIF

c      CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c      CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
Cswd move wind stresses to u and v points
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
      CALL DIAGNOSTICS_FILL_RS(Qnet,'CH_QNET ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(SH_sauv,'CH_SH   ',0,1,0,1,1,myThid)
      CALL DIAGNOSTICS_FILL(LH_sauv,'CH_LH   ',0,1,0,1,1,myThid)
      IF (useFreshWaterFlux) THEN
        CALL DIAGNOSTICS_FILL(Qair, 'CH_QAIR ',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL_RS(EmPmR,'CH_EmP  ',0,1,0,1,1,myThid)
      ENDIF
      IF (useCheapTracer) THEN
        CALL DIAGNOSTICS_FILL(Cheaptracer,'CH_Trace',0,1,0,1,1,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

c     DO bj=myByLo(myThid),myByHi(myThid)
c      DO bi=myBxLo(myThid),myBxHi(myThid)
c        DO j = 1-OLy,sNy+OLy
c         DO i = 1-OLx+1,sNx+OLx
c           fu(i,j,bi,bj) = 0.0
c           fv(i,j,bi,bj) = 0.0
c           Qnet(i,j,bi,bj) = 0.0
c           EmPmR(i,j,bi,bj) = 0.0
c         ENDDO
c        ENDDO
c      ENDDO
c     ENDDO

      RETURN
      END
