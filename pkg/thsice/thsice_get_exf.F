C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_exf.F,v 1.9 2007/05/07 19:26:57 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_EXF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_EXF(
     I                         iceornot, tsfCel,
     O                         flxExceptSw, df0dT, evapLoc, dEvdT,
     I                         i,j,bi,bj,myThid )
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
#ifdef ALLOW_EXF
# include "SIZE.h"
# include "EEPARAMS.h"
# include "PARAMS.h"
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
C     iceornot    :: 0=open water, 1=ice cover
C     tsfCel      :: surface (ice or snow) temperature (oC)
C     flxExceptSw :: net (downward) surface heat flux, except short-wave [W/m2]
C     df0dT       :: deriv of flx with respect to Tsf    [W/m/K]
C     evapLoc     :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     i,j, bi,bj  :: current grid point indices
C     myThid      :: My Thread Id number
      INTEGER i,j, bi,bj
      INTEGER myThid
      INTEGER iceornot
      _RL  tsfCel
      _RL  flxExceptSw
      _RL  df0dT
      _RL  evapLoc
      _RL  dEvdT
CEOP

#ifdef ALLOW_EXF
#ifdef ALLOW_ATM_TEMP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     hsLocal, hlLocal :: sensible & latent heat flux over sea-ice
C             t0       :: virtual temperature (K)
C             ssq      :: saturation specific humidity (kg/kg)
C             deltap   :: potential temperature diff (K)

      _RL     hsLocal, hlLocal
      INTEGER iter
      _RL     delq
      _RL     deltap
c-----------------------
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
c-----------------------

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
      INTEGER ikey_1
      INTEGER ikey_2
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
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1

          ikey_1 = i
     &         + sNx*(j-1)
     &         + sNx*sNy*act1
     &         + sNx*sNy*max1*act2
     &         + sNx*sNy*max1*max2*act3
     &         + sNx*sNy*max1*max2*max3*act4
#endif

C-- Set surface parameters :
      zwln = LOG(hu/zref)
      ztln = LOG(ht/zref)
      czol = hu*karman*gravity_mks

C     copy a few variables to names used in bulkf_formula_lay
      Tsf    =  tsfCel+cen2kel
      Ts2    = Tsf*Tsf
C     wind speed
      ws     = us(i,j,bi,bj)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
      wsm    = sh(i,j,bi,bj)
      IF ( iceornot.EQ.0 ) THEN
        lath  = flamb
        qsat_fac = cvapor_fac
        qsat_exp = cvapor_exp
      ELSE
        lath  = flamb+flami
        qsat_fac = cvapor_fac_ice
        qsat_exp = cvapor_exp_ice
      ENDIF

C--   Use atmospheric state to compute surface fluxes.

C--   Compute the turbulent surface fluxes.

C             Initial guess: z/l=0.0; hu=ht=hq=z
C             Iterations:    converge on z/l and hence the fluxes.

          IF ( atemp(i,j,bi,bj) .NE. 0. _d 0 ) THEN
           t0     = atemp(i,j,bi,bj)*
     &            (exf_one + humid_fac*aqh(i,j,bi,bj))
c          tmpbulk= exf_BulkqSat(Tsf)
c          ssq    = saltsat*tmpbulk/atmrho
           tmpbulk= qsat_fac*EXP(-qsat_exp/Tsf)
           ssq    = tmpbulk/atmrho
           deltap = atemp(i,j,bi,bj) + gamma_blk*ht - Tsf
           delq   = aqh(i,j,bi,bj) - ssq
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
                   ikey_2 = iter
     &                  + niter_bulk*(i-1)
     &                  + niter_bulk*sNx*(j-1)
     &                  + niter_bulk*sNx*sNy*act1
     &                  + niter_bulk*sNx*sNy*max1*act2
     &                  + niter_bulk*sNx*sNy*max1*max2*act3
     &                  + niter_bulk*sNx*sNy*max1*max2*max3*act4

CADJ STORE rdn    = comlev1_exf_2, key = ikey_2
CADJ STORE ustar  = comlev1_exf_2, key = ikey_2
CADJ STORE qstar  = comlev1_exf_2, key = ikey_2
CADJ STORE tstar  = comlev1_exf_2, key = ikey_2
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
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

           evapLoc = -tau*qstar
           hlLocal = -lath*evapLoc
           hsLocal = atmcp*tau*tstar
c          ustress = tau*rd*UwindSpeed
c          vstress = tau*rd*VwindSpeed

C---  surf.Temp derivative of turbulent Fluxes
           dEvdT  =  (tau*re)*ssq*qsat_exp/Ts2
           dflhdT = -lath*dEvdT
           dfshdT = -atmcp*tau*rh

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
           df0dT = -dflwupdT+dfshdT+dflhdT

#ifdef ALLOW_DOWNWARD_RADIATION
C-    assume long-wave albedo = 1 - emissivity
           flwNet_dwn  = emiss*lwdown(i,j,bi,bj) - flwup
#else
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: DOWNWARD_RADIATION undef'
#endif
           flxExceptSw = flwNet_dwn + hsLocal + hlLocal

          ELSE
           flxExceptSw = 0. _d 0
           df0dT   = 0. _d 0
           evapLoc = 0. _d 0
           dEvdT   = 0. _d 0
          ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */
#ifdef EXF_READ_EVAP
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: EXF_READ_EVAP defined'
#endif /* EXF_READ_EVAP */
#endif /* ALLOW_EXF */

      RETURN
      END
