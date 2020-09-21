#include "CHEAPAML_OPTIONS.h"

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
C       uWind, vWind  - mean wind speed (m/s)
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
#include "CHEAPAML.h"

C     == routine arguments ==
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j, nt, startAB
      INTEGER iMin, iMax
      INTEGER jMin, jMax
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
        _RL dm,pt,xalwd,xlwnet
        _RL dtemp,xflu,xfld,dq,dtr
c       _RL Fclouds, ttt2
        _RL q,precip,ttt,entrain
        _RL uRelWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL vRelWind(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL windSq  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL fsha(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL flha(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL evp (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL xolw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL ssqt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL q100(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL cdq (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     surfDrag   :: surface drag coeff (for wind stress)
        _RL surfDrag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
        _RL dumArg(6)
        _RL fsha0, flha0, evp_0, xolw0, ssqt0, q100_0, cdq_0
        _RL Tsurf  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL iceFrac(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
        _RL icFrac, opFrac
C temp var
        _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C useful values
C inverse of time step
        deltaTm=1. _d 0/deltaT

C atmospheric timestep
        deltaTtracer = deltaT/FLOAT(cheapaml_ntim)

c       writeDbug = debugLevel.GE.debLevC .AND.
c     &             DIFFERENT_MULTIPLE(diagFreq, myTime, deltaTClock)
        writeDbug = debugLevel.GE.debLevC .AND. diagFreq.GT.0.

#ifdef ALLOW_DIAGNOSTICS
C--   fill-in diagnostics for cheapAML state variables:
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL( Tair, 'CH_TAIR ', 0,1,0,1,1, myThid )
       IF (useFreshWaterFlux)
     & CALL DIAGNOSTICS_FILL( Qair, 'CH_QAIR ', 0,1,0,1,1, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

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
             uTrans(i,j) = uWind(i,j,bi,bj)*dyG(i,j,bi,bj)
             vTrans(i,j) = vWind(i,j,bi,bj)*dxG(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL CHEAPAML_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uWind, vWind,
     I           cheapaml_kdiff, Tair,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wWind,
     U           gTair,
     I           myTime, myIter, myThid )
         IF  (.NOT.useFluxLimit ) THEN
           startAB = cheapTairStartAB + nt - 1
           CALL ADAMS_BASHFORTH2(
     I           bi, bj, 1, 1,
     U           gTair(1-OLx,1-OLy,bi,bj),
     U           gTairm(1-OLx,1-OLy,bi,bj), tmpFld,
     I           startAB, myIter, myThid )
         ENDIF
         CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gTair,
     U           Tair,
     I           nt, myIter, myThid )
C close bi,bj loops
         ENDDO
        ENDDO
C update edges
        _EXCH_XY_RL(Tair,myThid)

       IF (useFreshWaterFlux) THEN
C do water
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             gqair(i,j,bi,bj)=0. _d 0
             uTrans(i,j) = uWind(i,j,bi,bj)*dyG(i,j,bi,bj)
             vTrans(i,j) = vWind(i,j,bi,bj)*dxG(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL CHEAPAML_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uWind, vWind,
     I           cheapaml_kdiff, qair,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wWind,
     U           gqair,
     I           myTime, myIter, myThid )
          IF  (.NOT.useFluxLimit ) THEN
           startAB = cheapTairStartAB + nt - 1
           CALL ADAMS_BASHFORTH2(
     I           bi, bj, 1, 1,
     U           gqair(1-OLx,1-OLy,bi,bj),
     U           gqairm(1-OLx,1-OLy,bi,bj), tmpFld,
     I           startAB, myIter, myThid )
          ENDIF
          CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gqair,
     U           qair,
     I           nt, myIter, myThid )
C close bi, bj loops
         ENDDO
        ENDDO
C update edges
        _EXCH_XY_RL(qair,myThid)
       ENDIF         ! if use freshwater

       IF (useCheapTracer) THEN
C     do tracer
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             gCheaptracer(i,j,bi,bj)=0. _d 0
             uTrans(i,j) = uWind(i,j,bi,bj)*dyG(i,j,bi,bj)
             vTrans(i,j) = vWind(i,j,bi,bj)*dxG(i,j,bi,bj)
           ENDDO
          ENDDO
          CALL CHEAPAML_CALC_RHS(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy,
     I           uTrans, vTrans,
     I           uWind, vWind,
     I           cheapaml_kdiff, Cheaptracer,
     I           deltaTtracer, zu, useFluxLimit,
     I           cheapamlXperiodic, cheapamlYperiodic,
     O           wWind,
     U           gCheaptracer,
     I           myTime, myIter, myThid )
          IF  (.NOT.useFluxLimit ) THEN
           startAB = cheapTracStartAB + nt - 1
           CALL ADAMS_BASHFORTH2(
     I           bi, bj, 1, 1,
     U           gCheaptracer(1-OLx,1-OLy,bi,bj),
     U           gCheaptracerm(1-OLx,1-OLy,bi,bj), tmpFld,
     I           startAB, myIter, myThid )
          ENDIF
          CALL CHEAPAML_TIMESTEP(
     I           bi, bj, 1-OLx,sNx+OLx, 1-OLy,sNy+OLy, deltaTtracer,
     I           gCheaptracer,
     U           Cheaptracer,
     I           nt, myIter, myThid )
C     close bi, bj loops
         ENDDO
        ENDDO
C     update edges
        _EXCH_XY_RL(Cheaptracer,myThid)
       ENDIF                   ! if use tracer

C reset boundaries to open boundary profile
       IF ( .NOT.(cheapamlXperiodic.AND.cheapamlYperiodic) ) THEN
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
      iMin = 1
      iMax = sNx
      jMin = 1
      jMax = sNy

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

           DO j = 1-OLy, sNy+OLy
             DO i = 1-OLx, sNx+OLx
               surfDrag(i,j,bi,bj) = 0.
             ENDDO
           ENDDO

         IF (useRelativeWind) THEN
           DO j = 1-OLy, sNy+OLy
             DO i = 1-OLx, sNx+OLx
               uRelWind(i,j) = uWind(i,j,bi,bj)-uVel(i,j,1,bi,bj)
               vRelWind(i,j) = vWind(i,j,bi,bj)-vVel(i,j,1,bi,bj)
             ENDDO
           ENDDO

           DO j = jMin,jMax
             DO i = iMin,iMax
               windSq(i,j) = ( uRelWind( i ,j)*uRelWind( i ,j)
     &            + uRelWind(i+1,j)*uRelWind(i+1,j)
     &            + vRelWind(i, j )*vRelWind(i, j )
     &            + vRelWind(i,j+1)*vRelWind(i,j+1)
     &            )*0.5 _d 0
#ifdef INCONSISTENT_WIND_LOCATION
               windSq(i,j) = uRelWind(i,j)*uRelWind(i,j)
     &            + vRelWind(i,j)*vRelWind(i,j)
#endif
             ENDDO
           ENDDO
         ELSE
           DO j = jMin,jMax
             DO i = iMin,iMax
               windSq(i,j) = ( uWind( i ,j,bi,bj)*uWind( i ,j,bi,bj)
     &            + uWind(i+1,j,bi,bj)*uWind(i+1,j,bi,bj)
     &            + vWind(i, j ,bi,bj)*vWind(i, j,bi,bj )
     &            + vWind(i,j+1,bi,bj)*vWind(i,j+1,bi,bj)
     &            )*0.5 _d 0
#ifdef INCONSISTENT_WIND_LOCATION
               windSq(i,j) = uWind(i,j,bi,bj)*uWind(i,j,bi,bj)
     &            + vWind(i,j,bi,bj)*vWind(i,j,bi,bj)
#endif
             ENDDO
           ENDDO

         ENDIF

         IF ( useThSIce ) THEN
           CALL CHEAPAML_SEAICE(
     I                    solar(1-OLx,1-OLy,bi,bj),
     I                    cheapdlongwave(1-OLx,1-OLy,bi,bj),
     I                    uWind(1-OLx,1-OLy,bi,bj),
     I                    vWind(1-OLx,1-OLy,bi,bj), lath,
     O                    fsha, flha, evp, xolw, ssqt, q100, cdq,
     O                    Tsurf, iceFrac, Qsw(1-OLx,1-OLy,bi,bj),
     I                    bi, bj, myTime, myIter, myThid )
           DO j = jMin,jMax
            DO i = iMin,iMax
              CALL CHEAPAML_COARE3_FLUX(
     I                      i, j, bi, bj, 0,
     I                      theta(1-OLx,1-OLy,1,bi,bj), windSq,
     O                      fsha0, flha0, evp_0, xolw0,
     O                      ssqt0, q100_0, cdq_0,
     O                      surfDrag(i,j,bi,bj),
     O                      dumArg(1), dumArg(2), dumArg(3), dumArg(4),
     I                      myIter, myThid )
              Qnet(i,j,bi,bj) = (
     &                           -solar(i,j,bi,bj)
     &                           +xolw0 - cheapdlongwave(i,j,bi,bj)
     &                           +fsha0
     &                           +flha0
     &                          )*maskC(i,j,1,bi,bj)
              EmPmR(i,j,bi,bj) = evp_0
              icFrac  = iceFrac(i,j)
              opFrac = 1. _d 0 - icFrac
C-     Qsw (from FFIELDS.h) has opposite (and annoying) sign convention:
              Qsw(i,j,bi,bj) = - ( icFrac*Qsw(i,j,bi,bj)
     &                           + opFrac*solar(i,j,bi,bj) )
              fsha(i,j) = icFrac*fsha(i,j) + opFrac*fsha0
              flha(i,j) = icFrac*flha(i,j) + opFrac*flha0
              evp(i,j)  = icFrac*evp(i,j)  + opFrac*evp_0
              xolw(i,j) = icFrac*xolw(i,j) + opFrac*xolw0
              ssqt(i,j) = icFrac*ssqt(i,j) + opFrac*ssqt0
              q100(i,j) = icFrac*q100(i,j) + opFrac*q100_0
              cdq(i,j)  = icFrac*cdq(i,j)  + opFrac*cdq_0
            ENDDO
           ENDDO
         ELSE
           DO j = jMin,jMax
            DO i = iMin,iMax
             IF (FluxFormula.EQ.'LANL') THEN
              CALL cheapaml_LANL_flux(
     I                      i, j, bi, bj,
     O                      fsha(i,j), flha(i,j), evp(i,j),
     O                      xolw(i,j), ssqt(i,j), q100(i,j) )
             ELSEIF (FluxFormula.EQ.'COARE3') THEN
              CALL CHEAPAML_COARE3_FLUX(
     I                      i, j, bi, bj, 0,
     I                      theta(1-OLx,1-OLy,1,bi,bj), windSq,
     O                      fsha(i,j), flha(i,j), evp(i,j), xolw(i,j),
     O                      ssqt(i,j), q100(i,j), cdq(i,j),
     O                      surfDrag(i,j,bi,bj),
     O                      dumArg(1), dumArg(2), dumArg(3), dumArg(4),
     I                      myIter, myThid )
             ENDIF
             IF (useFreshWaterFlux) THEN
              EmPmR(i,j,bi,bj) = evp(i,j)
             ENDIF
            ENDDO
           ENDDO
         ENDIF

         DO j = jMin,jMax
          DO i = iMin,iMax

C atmospheric upwelled long wave
           ttt = Tair(i,j,bi,bj)-gamma_blk*(CheapHgrid(i,j,bi,bj)-zt)
c          xalwu = stefan*(ttt+celsius2K)**4*0.5 _d 0
           xalwu = stefan*(0.5*Tair(i,j,bi,bj)+0.5*ttt+celsius2K)**4
     &                   *0.5 _d 0
C atmospheric downwelled long wave
           xalwd = stefan*(Tair(i,j,bi,bj)+celsius2K)**4*0.5 _d 0
C total flux at upper atmospheric layer interface
           xflu = ( -solar(i,j,bi,bj) + xalwu + flha(i,j)
     &            )*xef*maskC(i,j,1,bi,bj)
C lower flux calculation.
           xfld = ( -solar(i,j,bi,bj) - xalwd + xolw(i,j)
     &              + fsha(i,j) + flha(i,j)
     &            )*xef*maskC(i,j,1,bi,bj)

           IF (useDLongWave) THEN
             xlwnet = xolw(i,j)-cheapdlongwave(i,j,bi,bj)
           ELSE
C net long wave (see Josey et al. JGR 1997)
C coef lambda replaced by 0.5+lat/230
C convert spec humidity in water vapor pressure (mbar) using coef 1000/0.622=1607.7
             xlwnet = 0.98 _d 0*stefan*(theta(i,j,1,bi,bj)+celsius2K)**4
     &          *(0.39 _d 0 - 0.05 _d 0*SQRT(ABS(qair(i,j,bi,bj))
     &          *     1607.7 _d 0))
     &        *( oneRL - (halfRL+ABS(yG(i,j,bi,bj))/230. _d 0)
     &                   *cheapclouds(i,j,bi,bj)**2 )
     &        + 4.0*0.98 _d 0*stefan*(theta(i,j,1,bi,bj)+celsius2K)**3
     &          *(theta(i,j,1,bi,bj)-Tair(i,j,bi,bj))

c            xlwnet = xolw-stefan*(theta(i,j,1,bi,bj)+celsius2K)**4.
c     &       *(0.65+11.22*qair(i,j,bi,bj) + 0.25*cheapclouds(i,j,bi,bj)
c     &       -8.23*qair(i,j,bi,bj)*cheapclouds(i,j,bi,bj))
           ENDIF
C clouds
c          ttt2=Tair(i,j,bi,bj)-1.5*gamma_blk*CheapHgrid(i,j,bi,bj)
c          Fclouds = stefan*ttt2**4*(0.4*cheapclouds(i,j,bi,bj)+1-0.4)/2
c          ttt2=Tair(i,j,bi,bj)-3*gamma_blk*CheapHgrid(i,j,bi,bj)+celsius2K
c          Fclouds = 0.3*stefan*ttt2**4 + 0.22*xolw*cheapclouds(i,j,bi,bj)
C add flux divergences into atmospheric temperature tendency
           gTair(i,j,bi,bj)= (xfld-xflu)/CheapHgrid(i,j,bi,bj)
           IF ( .NOT.useThSIce ) THEN
            Qnet(i,j,bi,bj) = (
     &                         -solar(i,j,bi,bj)
c    &                         -xalwd
c    &                         -Fclouds
c    &                         +xolw
     &                         +xlwnet
     &                         +fsha(i,j)
     &                         +flha(i,j)
     &                        )*maskC(i,j,1,bi,bj)
             Qsw(i,j,bi,bj) = -solar(i,j,bi,bj)
           ENDIF

C need to precip?
           IF (useFreshWaterFlux) THEN
             q=q100(i,j)
C compute saturation specific humidity at atmospheric
C layer top
C first, what is the pressure there?
C ts is surface atmospheric temperature
            ts=Tair(i,j,bi,bj)+gamma_blk*zt+celsius2K
            pt=p0*(1-gamma_blk*CheapHgrid(i,j,bi,bj)/ts)
     &          **(gravity/gamma_blk/gasR)

             IF (.NOT.usePrecip) THEN
C factor to compute rainfall from specific humidity
              dm=100.*(p0-pt)*recip_gravity
C     Large scale precip
              precip = 0.
              IF ( wWind(i,j,bi,bj).GT.0. .AND.
     &             q.GT.ssqt(i,j)*0.7 _d 0 ) THEN
                precip = precip
     &               + ( (q-ssqt(i,j)*0.7 _d 0)*dm/cheap_pr2 )
     &                 *(wWind(i,j,bi,bj)/0.75 _d -5)**2
              ENDIF

C     Convective precip
              IF (q.GT.0.0214 _d 0 .AND. q.GT.ssqt(i,j)*0.9 _d 0) THEN
                precip = precip + ((q-ssqt(i,j)*0.9 _d 0)*dm/cheap_pr1)
              ENDIF

              cheapPrecip(i,j,bi,bj) = precip*1200/CheapHgrid(i,j,bi,bj)
            ENDIF

              entrain = cdq(i,j)*q*0.25

c           gqair(i,j,bi,bj)=(evp-precip-entrain)/CheapHgrid(i,j,bi,bj)
            gqair(i,j,bi,bj) = (evp(i,j)-entrain)/CheapHgrid(i,j,bi,bj)
     &                        /rhoa*maskC(i,j,1,bi,bj)
            EmPmR(i,j,bi,bj) = ( EmPmR(i,j,bi,bj)
     &                          -cheapPrecip(i,j,bi,bj)
     &                         )*maskC(i,j,1,bi,bj)
           ENDIF

          ENDDO
         ENDDO

C it is not necessary to use the Adams2d subroutine as
C the forcing is always computed at the current time step.
C note: full oceanic time step deltaT is used below
         CALL CHEAPAML_TIMESTEP(
     I           bi, bj, iMin, iMax, jMin, jMax, deltaT,
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
     I           bi, bj, iMin, iMax, jMin, jMax, deltaT,
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

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL( fsha,'CH_SH   ',0,1,2,bi,bj,myThid )
         CALL DIAGNOSTICS_FILL( flha,'CH_LH   ',0,1,2,bi,bj,myThid )
         CALL DIAGNOSTICS_FILL( q100,'CH_q100 ',0,1,2,bi,bj,myThid )
         CALL DIAGNOSTICS_FILL( ssqt,'CH_ssqt ',0,1,2,bi,bj,myThid )
        ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C close bi,bj loops
       ENDDO
      ENDDO

C update edges
       _EXCH_XY_RL(Tair,myThid)
       _EXCH_XY_RS(Qnet,myThid)
      IF (useFreshWaterFlux) THEN
       _EXCH_XY_RL(qair,myThid)
       _EXCH_XY_RS(EmPmR,myThid)
      ENDIF
      IF (useCheapTracer) THEN
       _EXCH_XY_RL(Cheaptracer,myThid)
      ENDIF
      IF (.NOT.useStressOption) THEN
       IF ( FluxFormula.EQ.'COARE3' ) THEN
        _EXCH_XY_RL( surfDrag, myThid )
       ELSE
        CALL EXCH_UV_AGRID_3D_RL( ustress, vstress, .TRUE., 1, myThid )
       ENDIF
      ENDIF

C reset edges to open boundary profiles
c     IF ( .NOT.(cheapamlXperiodic.AND.cheapamlYperiodic) ) THEN
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

c     CALL PLOT_FIELD_XYRS( gTair, 'S/R CHEAPAML gTair',1,myThid)
c     CALL PLOT_FIELD_XYRS( Tair, 'S/R CHEAPAML Tair',1,myThid)
c     CALL PLOT_FIELD_XYRS( Qnet, 'S/R CHEAPAML Qnet',1,myThid)

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        IF ( .NOT.useStressOption .AND. FluxFormula.EQ.'COARE3' ) THEN
          IF (useRelativeWind) THEN

          DO j = 1-OLy+1,sNy+OLy
          DO i = 1-OLx+1,sNx+OLx
            fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i-1,j,bi,bj) + surfDrag(i,j,bi,bj) )
     &          *( uWind(i,j,bi,bj)-uVel(i,j,1,bi,bj) )
            fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i,j-1,bi,bj) + surfDrag(i,j,bi,bj) )
     &          *( vWind(i,j,bi,bj)-vVel(i,j,1,bi,bj) )
          ENDDO
         ENDDO
#ifdef INCONSISTENT_WIND_LOCATION
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx+1,sNx+OLx
            fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i-1,j,bi,bj)
     &             *( uWind(i-1,j,bi,bj)-uVel(i-1,j,1,bi,bj) )
     &           + surfDrag(i,j,bi,bj)
     &             *( uWind(i,j,bi,bj) - uVel(i,j,1,bi,bj) ) )
          ENDDO
         ENDDO
         DO j = 1-OLy+1,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i,j-1,bi,bj)
     &             *( vWind(i,j-1,bi,bj)-vVel(i,j-1,1,bi,bj) )
     &           + surfDrag(i,j,bi,bj)
     &             *( vWind(i,j,bi,bj) - vVel(i,j,1,bi,bj) ) )
          ENDDO
         ENDDO
#endif /* INCONSISTENT_WIND_LOCATION */
       ELSE ! relative wind

          DO j = 1-OLy+1,sNy+OLy
          DO i = 1-OLx+1,sNx+OLx
            fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i-1,j,bi,bj) + surfDrag(i,j,bi,bj) )
     &          *uWind(i,j,bi,bj)
            fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i,j-1,bi,bj) + surfDrag(i,j,bi,bj) )
     &          *vWind(i,j,bi,bj)
          ENDDO
         ENDDO
#ifdef INCONSISTENT_WIND_LOCATION
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx+1,sNx+OLx
            fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i-1,j,bi,bj)
     &             *uWind(i-1,j,bi,bj)
     &           + surfDrag(i,j,bi,bj)
     &             *uWind(i,j,bi,bj))
          ENDDO
         ENDDO
         DO j = 1-OLy+1,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)*0.5 _d 0
     &          *( surfDrag(i,j-1,bi,bj)
     &             *vWind(i,j-1,bi,bj)
     &           + surfDrag(i,j,bi,bj)
     &             *vWind(i,j,bi,bj))
          ENDDO
         ENDDO
#endif /* INCONSISTENT_WIND_LOCATION */
         ENDIF !relative wind

       ELSE
Cswd move wind stresses to u and v points
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx+1,sNx+OLx
            fu(i,j,bi,bj) = maskW(i,j,1,bi,bj)*0.5 _d 0
     &          *( ustress(i,j,bi,bj) + ustress(i-1,j,bi,bj) )
          ENDDO
         ENDDO
         DO j = 1-OLy+1,sNy+OLy
          DO i = 1-OLx,sNx+OLx
            fv(i,j,bi,bj) = maskS(i,j,1,bi,bj)*0.5 _d 0
     &          *( vstress(i,j,bi,bj) + vstress(i,j-1,bi,bj) )
          ENDDO
         ENDDO
        ENDIF

C--   end bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
C- note: with thSIce, CH_QNET and CH_EmP correspond to the fluxes over the
C     open-ocean fraction of the grid-cell (similar to EXFqnet and EXFempmr).
C     Use instead diagnostics SIflxAtm & SIfrwAtm to get the grid-cell
C      averaged (open-ocean fraction + ice-covered fraction).
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(uWind,  'CH_Uwind',0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(vWind,  'CH_Vwind',0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL_RS(Qnet,'CH_QNET ',0,1,0,1,1,myThid)
       IF (useFreshWaterFlux) THEN
        CALL DIAGNOSTICS_FILL_RS( EmPmR, 'CH_EmP  ', 0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(cheapPrecip,'CH_Prec ',0,1,0,1,1,myThid)
       ENDIF
       IF (useCheapTracer) THEN
        CALL DIAGNOSTICS_FILL(Cheaptracer,'CH_Trace',0,1,0,1,1,myThid)
       ENDIF
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
