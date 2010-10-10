C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_exf.F,v 1.13 2010/10/10 20:15:06 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_EXF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_EXF(
     I                  bi, bj, siLo, siHi, sjLo, sjHi,
     I                  iMin,iMax, jMin,jMax,
     I                  iceFlag, hSnow, tsfCel,
     O                  flxExcSw, dFlxdT, evapLoc, dEvdT,
     I                  myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_GET_EXF
C     *==========================================================*
C     | Interface S/R : get Surface Fluxes from pkg EXF
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef ALLOW_EXF
# include "EXF_CONSTANTS.h"
# include "EXF_PARAM.h"
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     siLo,siHi   :: size of input/output array: 1rst dim. lower,higher bounds
C     sjLo,sjHi   :: size of input/output array: 2nd  dim. lower,higher bounds
C     bi,bj       :: tile indices
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     iceFlag     :: True= get fluxes at this location ; False= do nothing
C     hSnow       :: snow height [m]
C     tsfCel      :: surface (ice or snow) temperature (oC)
C     flxExcSw    :: net (downward) surface heat flux, except short-wave [W/m2]
C     dFlxdT      :: deriv of flx with respect to Tsf    [W/m/K]
C     evapLoc     :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER siLo, siHi, sjLo, sjHi
      INTEGER bi, bj
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      LOGICAL iceFlag (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     hSnow   (siLo:siHi,sjLo:sjHi)
      _RL     tsfCel  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     flxExcSw(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dFlxdT  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     evapLoc (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     dEvdT   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EXF
#ifdef ALLOW_ATM_TEMP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     hsLocal, hlLocal :: sensible & latent heat flux over sea-ice
C             t0       :: virtual temperature (K)
C             ssq      :: saturation specific humidity (kg/kg)
C             deltap   :: potential temperature diff (K)
C     iceornot         :: 0=open water, 1=ice cover, 2=ice+snow
      _RL     hsLocal, hlLocal
      INTEGER iceornot
      INTEGER iter
      INTEGER i, j
      _RL     delq
      _RL     deltap
      _RL czol
      _RL ws                 ! wind speed [m/s] (unlimited)
      _RL wsm                ! limited wind speed [m/s] (> umin)
      _RL t0                 ! virtual temperature [K]
      _RL ustar              ! friction velocity [m/s]
      _RL tstar              ! turbulent temperature scale [K]
      _RL qstar              ! turbulent humidity scale  [kg/kg]
      _RL ssq
      _RL rd                 ! = sqrt(Cd)          [-]
      _RL re                 ! = Ce / sqrt(Cd)     [-]
      _RL rh                 ! = Ch / sqrt(Cd)     [-]
      _RL rdn, ren, rhn      ! neutral, zref (=10m) values of rd, re, rh
      _RL usn, usm           ! neutral, zref (=10m) wind-speed (+limited)
      _RL stable             ! = 1 if stable ; = 0 if unstable
      _RL huol               ! stability parameter at zwd [-] (=z/Monin-Obuklov length)
      _RL htol               ! stability parameter at zth [-]
      _RL hqol
      _RL x                  ! stability function  [-]
      _RL xsq                ! = x^2               [-]
      _RL psimh              ! momentum stability function
      _RL psixh              ! latent & sensib. stability function
      _RL zwln               ! = log(zwd/zref)
      _RL ztln               ! = log(zth/zref)
      _RL tau                ! surface stress coef = rhoA * Ws * sqrt(Cd)
      _RL tmpbulk

C     additional variables that are copied from bulkf_formula_lay:
C     upward LW at surface (W m-2)
      _RL  flwup
C     net (downward) LW at surface (W m-2)
      _RL  flwNet_dwn
C     gradients of latent/sensible net upward heat flux
C     w/ respect to temperature
      _RL dflhdT, dfshdT, dflwupdT
C     emissivities, called emittance in exf
      _RL     emiss
C     Tsf    :: surface temperature [K]
C     Ts2    :: surface temperature square [K^2]
      _RL Tsf
      _RL Ts2
C     latent heat of evaporation or sublimation [J/kg]
      _RL lath
      _RL qsat_fac, qsat_exp
#ifdef ALLOW_AUTODIFF_TAMC
c     INTEGER ikey_1
c     INTEGER ikey_2
#endif

C     == external functions ==

c     _RL       exf_BulkqSat
c     external  exf_BulkqSat
c     _RL       exf_BulkCdn
c     external  exf_BulkCdn
c     _RL       exf_BulkRhn
c     external  exf_BulkRhn

C     == end of interface ==

#ifdef ALLOW_AUTODIFF_TAMC
c         act1 = bi - myBxLo(myThid)
c         max1 = myBxHi(myThid) - myBxLo(myThid) + 1
c         act2 = bj - myByLo(myThid)
c         max2 = myByHi(myThid) - myByLo(myThid) + 1
c         act3 = myThid - 1
c         max3 = nTx*nTy
c         act4 = ikey_dynamics - 1
c         ikey_1 = i
c    &         + sNx*(j-1)
c    &         + sNx*sNy*act1
c    &         + sNx*sNy*max1*act2
c    &         + sNx*sNy*max1*max2*act3
c    &         + sNx*sNy*max1*max2*max3*act4
#endif
      DO j=jMin,jMax
       DO i=iMin,iMax
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Use atmospheric state to compute surface fluxes.
        IF ( iceFlag(i,j) .AND. atemp(i,j,bi,bj).NE.0. _d 0 ) THEN

          IF ( hSnow(i,j).GT.3. _d -1 ) THEN
           iceornot=2
          ELSE
           iceornot=1
          ENDIF

C     copy a few variables to names used in bulkf_formula_lay
          Tsf =  tsfCel(i,j)+cen2kel
          Ts2 = Tsf*Tsf
C     wind speed
          ws     = wspeed(i,j,bi,bj)
#ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
          wsm = sh(i,j,bi,bj)
          IF ( iceornot.EQ.0 ) THEN
           lath  = flamb
           qsat_fac = cvapor_fac
           qsat_exp = cvapor_exp
          ELSE
           lath  = flamb+flami
           qsat_fac = cvapor_fac_ice
           qsat_exp = cvapor_exp_ice
          ENDIF

C--   air - surface difference of temperature & humidity
c         tmpbulk= exf_BulkqSat(Tsf)
c         ssq    = saltsat*tmpbulk/atmrho
          tmpbulk= qsat_fac*EXP(-qsat_exp/Tsf)
          ssq    = tmpbulk/atmrho
          deltap = atemp(i,j,bi,bj) + gamma_blk*ht - Tsf
          delq   = aqh(i,j,bi,bj) - ssq

          IF ( useStabilityFct_overIce ) THEN
C--   Compute the turbulent surface fluxes (function of stability).

C--   Set surface parameters :
           zwln = LOG(hu/zref)
           ztln = LOG(ht/zref)
           czol = hu*karman*gravity_mks

C             Initial guess: z/l=0.0; hu=ht=hq=z
C             Iterations:    converge on z/l and hence the fluxes.

           t0     = atemp(i,j,bi,bj)*
     &            (exf_one + humid_fac*aqh(i,j,bi,bj))
           stable = exf_half + SIGN(exf_half, deltap)
c          tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
           tmpbulk= cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
           rdn    = SQRT(tmpbulk)
C--  initial guess for exchange other coefficients:
c          rhn    = exf_BulkRhn(stable)
           rhn    = (exf_one-stable)*cstanton_1 + stable*cstanton_2
           ren    = cDalton
C--  calculate turbulent scales
           ustar  = rdn*wsm
           tstar  = rhn*deltap
           qstar  = ren*delq

           DO iter = 1,niter_bulk

#ifdef ALLOW_AUTODIFF_TAMC
c                  ikey_2 = iter
c    &                  + niter_bulk*(i-1)
c    &                  + niter_bulk*sNx*(j-1)
c    &                  + niter_bulk*sNx*sNy*act1
c    &                  + niter_bulk*sNx*sNy*max1*act2
c    &                  + niter_bulk*sNx*sNy*max1*max2*act3
c    &                  + niter_bulk*sNx*sNy*max1*max2*max3*act4
cCADJ STORE rdn    = comlev1_exf_2, key = ikey_2
cCADJ STORE ustar  = comlev1_exf_2, key = ikey_2
cCADJ STORE qstar  = comlev1_exf_2, key = ikey_2
cCADJ STORE tstar  = comlev1_exf_2, key = ikey_2
cCADJ STORE sh(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
#endif

            huol   = (tstar/t0 +
     &                qstar/(exf_one/humid_fac+aqh(i,j,bi,bj))
     &               )*czol/(ustar*ustar)
C-    Large&Pond_1981 code (zolmin default = -100):
c           huol   = MAX(huol,zolmin)
C-    Large&Yeager_2004 code:
            huol   = MIN( MAX(-10. _d 0,huol), 10. _d 0 )
            htol   = huol*ht/hu
            hqol   = huol*hq/hu
            stable = exf_half + SIGN(exf_half, huol)

C     Evaluate all stability functions assuming hq = ht.
C-    Large&Pond_1981 code:
c           xsq    = MAX(SQRT(ABS(exf_one - huol*16. _d 0)),exf_one)
C-    Large&Yeager_2004 code:
            xsq    = SQRT( ABS(exf_one - huol*16. _d 0) )
            x      = SQRT(xsq)
            psimh  = -psim_fac*huol*stable
     &             + (exf_one-stable)
     &              *( LOG( (exf_one + exf_two*x + xsq)
     &                     *(exf_one+xsq)*0.125 _d 0 )
     &                 -exf_two*ATAN(x) + exf_half*pi )
C-    Large&Pond_1981 code:
c           xsq    = MAX(SQRT(ABS(exf_one - htol*16. _d 0)),exf_one)
C-    Large&Yeager_2004 code:
            xsq    = SQRT( ABS(exf_one - htol*16. _d 0) )
            psixh  = -psim_fac*htol*stable
     &             + (exf_one-stable)
     &              *exf_two*LOG( exf_half*(exf_one+xsq) )

C     Shift wind speed using old coefficient
            usn    = ws/( exf_one + rdn*(zwln-psimh)/karman )
            usm    = MAX(usn, umin)

C-    Update the 10m, neutral stability transfer coefficients
c           tmpbulk= exf_BulkCdn(usm)
            tmpbulk= cdrag_1/usm + cdrag_2 + cdrag_3*usm
            rdn    = SQRT(tmpbulk)
c           rhn    = exf_BulkRhn(stable)
            rhn    = (exf_one-stable)*cstanton_1 + stable*cstanton_2

C     Shift all coefficients to the measurement height and stability.
            rd     = rdn/( exf_one + rdn*(zwln-psimh)/karman )
            rh     = rhn/( exf_one + rhn*(ztln-psixh)/karman )
            re     = ren/( exf_one + ren*(ztln-psixh)/karman )

C     Update ustar, tstar, qstar using updated, shifted coefficients.
            ustar  = rd*wsm
            qstar  = re*delq
            tstar  = rh*deltap
           ENDDO

           tau     = atmrho*rd*ws

           evapLoc(i,j) = -tau*qstar
           hlLocal = -lath*evapLoc(i,j)
           hsLocal = atmcp*tau*tstar
c          ustress = tau*rd*UwindSpeed
c          vstress = tau*rd*VwindSpeed

C---  surf.Temp derivative of turbulent Fluxes
           dEvdT(i,j) = (tau*re)*ssq*qsat_exp/Ts2
           dflhdT = -lath*dEvdT(i,j)
           dfshdT = -atmcp*tau*rh

          ELSE
C--   Compute the turbulent surface fluxes using fixed transfert Coeffs
C      with no stability dependence ( useStabilityFct_overIce = false )

           evapLoc(i,j) = -atmrho*exf_iceCe*wsm*delq
           hlLocal = -lath*evapLoc(i,j)
           hsLocal = atmcp*atmrho*exf_iceCh*wsm*deltap
C---  surf.Temp derivative of turbulent Fluxes
           dEvdT(i,j) = (atmrho*exf_iceCe*wsm)*(ssq*qsat_exp/Ts2)
           dflhdT = -lath*dEvdT(i,j)
           dfshdT = -atmcp*atmrho*exf_iceCh*wsm

          ENDIF
C--- Upward long wave radiation
          IF ( iceornot.EQ.0 ) THEN
            emiss = ocean_emissivity
          ELSEIF (iceornot.EQ.2) THEN
            emiss = snow_emissivity
          ELSE
            emiss = ice_emissivity
          ENDIF
          flwup    = emiss*stefanBoltzmann*Ts2*Ts2
          dflwupdT = emiss*stefanBoltzmann*Ts2*Tsf * 4. _d 0

C--   Total derivative with respect to surface temperature
          dFlxdT(i,j) = -dflwupdT+dfshdT+dflhdT

#ifdef ALLOW_DOWNWARD_RADIATION
c         flwNet_dwn  =       lwdown(i,j,bi,bj) - flwup
C-    assume long-wave albedo = 1 - emissivity :
          flwNet_dwn  = emiss*lwdown(i,j,bi,bj) - flwup
#else
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: DOWNWARD_RADIATION undef'
#endif
          flxExcSw(i,j) = flwNet_dwn + hsLocal + hlLocal

        ELSEIF ( iceFlag(i,j) ) THEN
C--     in case atemp is zero:
          flxExcSw(i,j) = 0. _d 0
          dFlxdT  (i,j) = 0. _d 0
          evapLoc (i,j) = 0. _d 0
          dEvdT   (i,j) = 0. _d 0

        ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
       ENDDO
      ENDDO

#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */
#ifdef EXF_READ_EVAP
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: EXF_READ_EVAP defined'
#endif /* EXF_READ_EVAP */
#endif /* ALLOW_EXF */

      RETURN
      END
