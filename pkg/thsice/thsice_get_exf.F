C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_exf.F,v 1.7 2007/04/27 15:51:28 jmc Exp $
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
C     myThid      :: Thread no. that called this routine.
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

      _RL     aln

      _RL     hsLocal, hlLocal
      integer iter
      _RL     delq
      _RL     deltap
      _RL     hqol
      _RL     htol
      _RL     huol
      _RL     psimh
      _RL     psixh
      _RL     qstar
      _RL     rd
      _RL     re
      _RL     rdn
      _RL     rh
      _RL     ssq
      _RL     stable
      _RL     tstar
      _RL     t0
      _RL     ustar
      _RL     uzn
      _RL     shn
      _RL     xsq
      _RL     x
      _RL     tau
      _RL     tmpbulk

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
#ifdef ALLOW_AUTODIFF_TAMC
      integer ikey_1
      integer ikey_2
#endif

C     == external functions ==

c     _RL       exf_BulkqSat
c     external  exf_BulkqSat
      _RL       exf_BulkCdn
      external  exf_BulkCdn
      _RL       exf_BulkRhn
      external  exf_BulkRhn

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

C     copy a few variables to names used in bulkf_formula_lay
      Tsf    =  tsfCel+cen2kel
      Ts2    = Tsf*Tsf
      IF ( iceornot.EQ.0 ) THEN
        lath  = flamb
        dEvdT = cvapor_exp
      ELSE
        lath  = flamb+flami
        dEvdT = cvapor_exp_ice
      ENDIF

Cph   This statement cannot be a PARAMETER statement in the header,
Cph   but must come here; it is not fortran77 standard
      aln = log(ht/zref)

C--   Use atmospheric state to compute surface fluxes.

C--   Compute the turbulent surface fluxes.

C             Initial guess: z/l=0.0; hu=ht=hq=z
C             Iterations:    converge on z/l and hence the fluxes.
C             t0     : virtual temperature (K)
C             ssq    : sea surface humidity (kg/kg)
C             deltap : potential temperature diff (K)

          if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
           t0     = atemp(i,j,bi,bj)*
     &            (exf_one + humid_fac*aqh(i,j,bi,bj))
c          tmpbulk= exf_BulkqSat(Tsf)
c          ssq    = saltsat*tmpbulk/atmrho
           tmpbulk = cvapor_fac_ice/exp(cvapor_exp_ice/Tsf)
           ssq    = tmpbulk/atmrho
           deltap = atemp(i,j,bi,bj)   + gamma_blk*ht - Tsf
           delq   = aqh(i,j,bi,bj) - ssq
           stable = exf_half + sign(exf_half, deltap)
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif
           tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
           rdn    = sqrt(tmpbulk)
           ustar  = rdn*sh(i,j,bi,bj)
           tmpbulk= exf_BulkRhn(stable)
           tstar  = tmpbulk*deltap
           qstar  = cdalton*delq

           do iter = 1,niter_bulk

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

            huol   = czol*(tstar/t0 +
     &           qstar/(exf_one/humid_fac+aqh(i,j,bi,bj)))/
     &           ustar**2
            huol   = max(huol,zolmin)
            stable = exf_half + sign(exf_half, huol)
            htol   = huol*ht/hu
            hqol   = huol*hq/hu

C     Evaluate all stability functions assuming hq = ht.
            xsq    = max(sqrt(abs(exf_one - 16.*huol)),exf_one)
            x     = sqrt(xsq)
            psimh  = -psim_fac*huol*stable +
     &           (exf_one - stable)*
     &           (log((exf_one + x*(exf_two + x))*
     &           (exf_one + xsq)/8.) - exf_two*atan(x) +
     &           pi*exf_half)
            xsq    = max(sqrt(abs(exf_one - 16.*htol)),exf_one)
            psixh  = -psim_fac*htol*stable + (exf_one - stable)*
     &           exf_two*log((exf_one + xsq)/exf_two)

C     Shift wind speed using old coefficient
ccc   rd   = rdn/(exf_one + rdn/karman*
ccc   &                 (log(hu/zref) - psimh) )
            rd     = rdn/(exf_one - rdn/karman*psimh )
            shn    = sh(i,j,bi,bj)*rd/rdn
            uzn    = max(shn, umin)

C     Update the transfer coefficients at 10 meters
C     and neutral stability.

            tmpbulk= exf_BulkCdn(uzn)
            rdn    = sqrt(tmpbulk)

C     Shift all coefficients to the measurement height
C     and stability.
c     rd = rdn/(exf_one + rdn/karman*(log(hu/zref) - psimh))
            rd     = rdn/(exf_one - rdn/karman*psimh)
            tmpbulk= exf_BulkRhn(stable)
            rh     = tmpbulk/( exf_one +
     &           tmpbulk/karman*(aln - psixh) )
            re     = cdalton/( exf_one +
     &           cdalton/karman*(aln - psixh) )

C     Update ustar, tstar, qstar using updated, shifted
C     coefficients.
            ustar  = rd*sh(i,j,bi,bj)
            qstar  = re*delq
            tstar  = rh*deltap
           enddo

           tau    = atmrho*ustar**2
           tau    = tau*us(i,j,bi,bj)/sh(i,j,bi,bj)

           evapLoc       = -tau*qstar/ustar
           hlLocal = -lath*evapLoc
           hsLocal = atmcp*tau*tstar/ustar
#ifndef EXF_READ_EVAP
cdm             evap(i,j,bi,bj)    = tau*qstar/ustar
cdm !!! need to change sign and to convert from kg/m^2/s to m/s !!!
C jmc: do not reset evap which contains evaporation over ice-free ocean fraction
c          evap(i,j,bi,bj)    = -recip_rhonil*evapLoc
#endif

C---  surf.Temp derivative of turbulent Fluxes
           dEvdT  =  (tau*re/ustar)*ssq*dEvdT/Ts2
           dflhdT = -lath*dEvdT
           dfshdT = -atmcp*tau*rh/ustar

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
           flwNet_dwn  = lwdown(i,j,bi,bj) - flwup
#else
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: DOWNWARD_RADIATION undef'
#endif
           flxExceptSw = flwNet_dwn + hsLocal + hlLocal

          endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */
#endif /* ALLOW_EXF */

      RETURN
      END
