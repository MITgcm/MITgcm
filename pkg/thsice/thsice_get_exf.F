C $Header: /u/gcmpack/MITgcm/pkg/thsice/thsice_get_exf.F,v 1.1 2006/05/30 22:49:00 mlosch Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"
#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_EXF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_EXF(
     I                         iceornot, Tsf,
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
# include "exf_constants.h"
# include "exf_param.h"
# include "exf_fields.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     iceornot    :: 0=open water, 1=ice cover
C     Tsf         :: surface (ice or snow) temperature (oC)
C     flxExceptSw :: net (downward) surface heat flux, except short-wave [W/m2]
C     df0dT       :: deriv of flx with respect to Tsf    [W/m/K]
C     evapLoc     :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     i,j, bi,bj  :: current grid point indices
C     myThid      :: Thread no. that called this routine.
      INTEGER i,j, bi,bj
      INTEGER myThid
      INTEGER iceornot
      _RL  Tsf
      _RL  flxExceptSw
      _RL  df0dT
      _RL  evapLoc
      _RL  dEvdT
CEOP

#ifdef ALLOW_THSICE
#ifdef ALLOW_EXF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===

      _RL     aln

#ifdef ALLOW_ATM_TEMP
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
      _RL     ssttmp
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
#ifdef ALLOW_AUTODIFF_TAMC
      integer ikey_1
      integer ikey_2
#endif
#endif /* ALLOW_ATM_TEMP */

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
C     abbreviation
      _RL Ts2
C     latent heat, of evaporation at 0degC, ice melt [J/kg]
      _RL lath, Lvap, Lfresh
C     above and below freezing saturated specific humidity
      _RL qs2w, qs2i

c     == external functions ==

      integer  ilnblnk
      external ilnblnk

      _RL       exf_BulkqSat
      external  exf_BulkqSat
      _RL       exf_BulkCdn
      external  exf_BulkCdn
      _RL       exf_BulkRhn
      external  exf_BulkRhn

c     == end of interface ==

C     copy a few variables to names used in bulkf_formula_lay
      qs2w   = cvapor_exp
      qs2i   = cvapor_exp_ice
      Lvap   = flamb
      Lfresh = flami
      Ts2    = (Tsf+cen2kel)*(Tsf+cen2kel)

C     This is more or less a copy from exf_bulkformulae (loops kernel)

cph   This statement cannot be a PARAMETER statement in the header,
cph   but must come here; it is not fortran77 standard
      aln = log(ht/zref)

c--   Use atmospheric state to compute surface fluxes.

CMLc     Loop over tiles.
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLC--   HPF directive to help TAMC
CMLCHPF$ INDEPENDENT
CML#endif
CML      do bj = mybylo(mythid),mybyhi(mythid)
CML#ifdef ALLOW_AUTODIFF_TAMC
CMLC--    HPF directive to help TAMC
CMLCHPF$  INDEPENDENT
CML#endif
CML       do bi = mybxlo(mythid),mybxhi(mythid)
CML        k = 1
CML        do j = 1,sny
CML         do i = 1,snx

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

c--   Compute the turbulent surface fluxes.

#ifdef ALLOW_ATM_TEMP

c             Initial guess: z/l=0.0; hu=ht=hq=z
c             Iterations:    converge on z/l and hence the fluxes.
c             t0     : virtual temperature (K)
c             ssq    : sea surface humidity (kg/kg)
c             deltap : potential temperature diff (K)

          if ( atemp(i,j,bi,bj) .ne. 0. _d 0 ) then
           t0     = atemp(i,j,bi,bj)*
     &            (exf_one + humid_fac*aqh(i,j,bi,bj))
CML           ssttmp = theta(i,j,k,bi,bj)
           ssttmp = Tsf
           tmpbulk= exf_BulkqSat(ssttmp + cen2kel)
           ssq    = saltsat*tmpbulk/atmrho
           deltap = atemp(i,j,bi,bj)   + gamma_blk*ht -
     &            ssttmp - cen2kel
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
     &             + niter_bulk*(i-1)
     &             + niter_bulk*sNx*(j-1)
     &             + niter_bulk*sNx*sNy*act1
     &             + niter_bulk*sNx*sNy*max1*act2
     &             + niter_bulk*sNx*sNy*max1*max2*act3
     &             + niter_bulk*sNx*sNy*max1*max2*max3*act4

CADJ STORE rdn    = comlev1_exf_2, key = ikey_2
CADJ STORE ustar  = comlev1_exf_2, key = ikey_2
CADJ STORE qstar  = comlev1_exf_2, key = ikey_2
CADJ STORE tstar  = comlev1_exf_2, key = ikey_2
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
CADJ STORE us(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
#endif

            huol   = czol*(tstar/t0 +
     &           qstar/(exf_one/humid_fac+aqh(i,j,bi,bj)))/
     &           ustar**2
            huol   = max(huol,zolmin)
            stable = exf_half + sign(exf_half, huol)
            htol   = huol*ht/hu
            hqol   = huol*hq/hu
            
c     Evaluate all stability functions assuming hq = ht.
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
            
c     Shift wind speed using old coefficient
ccc   rd   = rdn/(exf_one + rdn/karman*
ccc   &                 (log(hu/zref) - psimh) )
            rd     = rdn/(exf_one - rdn/karman*psimh )
            shn    = sh(i,j,bi,bj)*rd/rdn
            uzn    = max(shn, umin)
            
c     Update the transfer coefficients at 10 meters
c     and neutral stability.
            
            tmpbulk= exf_BulkCdn(uzn)
            rdn    = sqrt(tmpbulk)
            
c     Shift all coefficients to the measurement height
c     and stability.
c     rd = rdn/(exf_one + rdn/karman*(log(hu/zref) - psimh))
            rd     = rdn/(exf_one - rdn/karman*psimh)
            tmpbulk= exf_BulkRhn(stable)
            rh     = tmpbulk/( exf_one + 
     &           tmpbulk/karman*(aln - psixh) )
            re     = cdalton/( exf_one + 
     &           cdalton/karman*(aln - psixh) )
            
c     Update ustar, tstar, qstar using updated, shifted
c     coefficients.
            ustar  = rd*sh(i,j,bi,bj)
            qstar  = re*delq 
            tstar  = rh*deltap
            tau    = atmrho*ustar**2
            tau    = tau*us(i,j,bi,bj)/sh(i,j,bi,bj)
            
           enddo

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustar  = comlev1_exf_1, key = ikey_1
CADJ STORE qstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tau    = comlev1_exf_1, key = ikey_1
CADJ STORE cw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
CADJ STORE sw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif

           hs(i,j,bi,bj)      = atmcp*tau*tstar/ustar
           hl(i,j,bi,bj)      = flamb*tau*qstar/ustar
#ifndef EXF_READ_EVAP
cdm             evap(i,j,bi,bj)    = tau*qstar/ustar
cdm !!! need to change sign and to convert from kg/m^2/s to m/s !!!
           evap(i,j,bi,bj)    = -recip_rhonil*tau*qstar/ustar
#endif

C     From here one this is pretty much a copy of parts of 
C     bulkf_formula_lay and thsice_get_bulkf to compute the required outputs

C---  surf.Temp derivative of turbulent Fluxes
C---  Compute turbulent surface fluxes
C---  Pot. Temp and saturated specific humidity
           IF ( iceornot.EQ.0 ) THEN
            lath  = Lvap
            dEvdT = qs2w
           ELSE
            lath  = Lvap+Lfresh
            dEvdT = qs2i
           ENDIF
           dEvdT  =  tau*re*ssq*dEvdT/Ts2
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
           dflwupdT = emiss*stefanBoltzmann*Ts2*(Tsf+cen2kel) * 4. _d 0
               
C--   Total derivative with respect to surface temperature
           df0dT = -dflwupdT+dfshdT+dflhdT
           
           flwNet_dwn  = lwdown(i,j,bi,bj) - flwup
           flxExceptSw = flwNet_dwn + hs(i,j,bi,bj) + hl(i,i,bi,bj)
C     
           evapLoc     = evap(i,j,bi,bj)*rhoNil
C     This is where the copies from bulkf_formula_lay and thsice_get_bulkf
C     end

CML           ustress(i,j,bi,bj) = tau*cw(i,j,bi,bj)
CML           vstress(i,j,bi,bj) = tau*sw(i,j,bi,bj)
CML          else
CML           ustress(i,j,bi,bj) = 0. _d 0
CML           vstress(i,j,bi,bj) = 0. _d 0
CML           hflux  (i,j,bi,bj) = 0. _d 0
CML           hs(i,j,bi,bj)      = 0. _d 0
CML           hl(i,j,bi,bj)      = 0. _d 0

          endif
           
#else  /* ifndef ALLOW_ATM_TEMP */
CML#ifdef ALLOW_ATM_WIND
CML          tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
CML          ustress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
CML     &         uwind(i,j,bi,bj)
CML          vstress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
CML     &         vwind(i,j,bi,bj)
CML#endif
#endif /* ifndef ALLOW_ATM_TEMP */
CML         enddo
CML        enddo
CML       enddo
CML      enddo

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#endif /* ALLOW_EXF */
#endif /* ALLOW_THSICE */

      RETURN
      END
