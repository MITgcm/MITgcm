C $Header: /u/gcmpack/MITgcm/pkg/exf/Attic/exf_bulk_largyeag.F,v 1.2 2007/04/16 23:27:20 jmc Exp $
C $Name:  $

#include "EXF_OPTIONS.h"

CBOP
C     !ROUTINE: EXF_BULK_LARGYEAG
C     !INTERFACE:
      SUBROUTINE EXF_BULK_LARGYEAG( mytime, myiter, mythid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_BULK_LARGYEAG
C     | o Calculate bulk formula fluxes over open ocean or seaice
C     |   Large and Yeager, 2004, NCAR/TN-460+STR.
C     *==========================================================*
C     \ev
C
C === Turbulent Fluxes :
C  * use the approach "B": shift coeff to height & stability of the
C    atmosphere state (instead of "C": shift temp & humid to the height
C    of wind, then shift the coeff to this height & stability of the atmos).
C  * similar to EXF (except over sea-ice) ; default parameter values
C    taken from Large & Yeager.
C  * assume that Qair & Tair inputs are from the same height (zq=zt)
C  * formulae in short:
C     wind stress = (ust,vst) = rhoA * Cd * Ws * (del.u,del.v)
C     Sensib Heat flux = fsha = rhoA * Ch * Ws * del.T * CpAir
C     Latent Heat flux = flha = rhoA * Ce * Ws * del.Q * Lvap
C                      = -Evap * Lvap
C   with Ws = wind speed = sqrt(del.u^2 +del.v^2) ;
C        del.T = Tair - Tsurf ; del.Q = Qair - Qsurf ;
C        Cd,Ch,Ce = drag coefficient, Stanton number and Dalton number
C              respectively [no-units], function of height & stability

C     !USES:
       IMPLICIT NONE
C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     input:
       _RL myTime
       INTEGER myIter
       INTEGER myThid           ! my Thread Id number
C     output:
CEOP

#ifdef ALLOW_BULK_LARGYEAG04

C     == Local variables ==
      INTEGER i,j,k,bi,bj     !current grid point indices

      _RL Tsf                ! surface temperature [K]
      _RL Ts2                ! surface temperature square [K^2]
      _RL ws
      _RL wsm                ! limited wind speed [m/s] (> umin)
      _RL t0                 ! virtual temperature [K]
      _RL delq               ! specific humidity difference [kg/kg]
      _RL deltap
      _RL ustar              ! friction velocity [m/s]
      _RL tstar              ! temperature scale [K]
      _RL qstar              ! humidity scale  [kg/kg]
      _RL ssttmp
      _RL ssq
      _RL rd                 ! = sqrt(Cd)          [-]
      _RL re                 ! = Ce / sqrt(Cd)     [-]
      _RL rh                 ! = Ch / sqrt(Cd)     [-]
      _RL rdn, ren, rhn      ! neutral, zref (=10m) values of rd, re, rh
      _RL usn, usm
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
      _RL tau                ! surface stress  coef = rhoA * Ws * Cd
      _RL tmpbulk
      INTEGER iter

#ifdef ALLOW_AUTODIFF_TAMC
      integer ikey_1
      integer ikey_2
#endif

C     == external Functions

C-- Set surface parameters :
      zwln = LOG(hu/zref)
      ztln = LOG(ht/zref)

c     Loop over tiles.
#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAMC
CHPF$ INDEPENDENT
#endif
      do bj = mybylo(mythid),mybyhi(mythid)
#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAMC
CHPF$  INDEPENDENT
#endif
       do bi = mybxlo(mythid),mybxhi(mythid)
        k = 1
        do j = 1,sny
         do i = 1,snx

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

#ifdef ALLOW_ATM_TEMP

C-   Surface Temp.
          ssttmp = theta(i,j,1,bi,bj)
          Tsf = ssttmp + cen2kel
          Ts2 = Tsf*Tsf
C-   Wind speed
          ws  = us(i,j,bi,bj)
          wsm = sh(i,j,bi,bj)

C--- Compute turbulent surface fluxes
C-   Pot. Temp and saturated specific humidity
          if ( atemp(i,j,bi,bj) .NE. 0. _d 0 ) then
c
            t0     = atemp(i,j,bi,bj)*
     &          (exf_one + humid_fac*aqh(i,j,bi,bj))
            tmpbulk = cvapor_fac*exp(-cvapor_exp/Tsf)
            ssq = saltsat*tmpbulk/atmrho
            deltap = atemp(i,j,bi,bj) + gamma_blk*ht - Tsf
            delq   = aqh(i,j,bi,bj) - ssq

C--  initial guess for exchange coefficients:
C    take U_N = del.U ; stability from del.Theta ;
            stable = exf_half + SIGN(exf_half, deltap)
            tmpbulk = cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
            rdn = SQRT(tmpbulk)
            rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
            ren = cDalton
C--  calculate turbulent scales
            ustar=rdn*wsm
            tstar=rhn*deltap
            qstar=ren*delq

C--- iterate with psi-functions to find transfer coefficients
            DO iter=1,niter_bulk

#ifdef ALLOW_AUTODIFF_TAMC
              ikey_2 = iter
     &            + niter_bulk*(i-1)
     &            + niter_bulk*sNx*(j-1)
     &            + niter_bulk*sNx*sNy*act1
     &            + niter_bulk*sNx*sNy*max1*act2
     &            + niter_bulk*sNx*sNy*max1*max2*act3
     &            + niter_bulk*sNx*sNy*max1*max2*max3*act4
CADJ STORE rdn    = comlev1_exf_2, key = ikey_2
CADJ STORE ustar  = comlev1_exf_2, key = ikey_2
CADJ STORE qstar  = comlev1_exf_2, key = ikey_2
CADJ STORE tstar  = comlev1_exf_2, key = ikey_2
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
CADJ STORE us(i,j,bi,bj)     = comlev1_exf_2, key = ikey_2
#endif

              huol = ( tstar/t0 +
     &                 qstar/(exf_one/humid_fac+aqh(i,j,bi,bj))
     &               )*czol/(ustar*ustar)
cph The following is different in Large&Pond1981 code:
cph huol = max(huol,zolmin) with zolmin = -100
              tmpbulk = MIN(abs(huol),10. _d 0)
              huol   = SIGN(tmpbulk , huol)
              htol   = huol*ht/hu
              hqol   = huol*hq/hu
              stable = exf_half + sign(exf_half, huol)

c                 Evaluate all stability functions assuming hq = ht.
cph The following is different in Large&Pond1981 code:
cph xsq = max(sqrt(abs(exf_one - 16.*huol)),exf_one)
              xsq    = SQRT( ABS(exf_one - huol*16. _d 0) )
              x      = SQRT(xsq)

              psimh = -psim_fac*huol*stable
     &                +(exf_one-stable)*
     &                 ( LOG( (exf_one + exf_two*x + xsq)*
     &                        (exf_one+xsq)*.125 _d 0 )
     &                   -exf_two*ATAN(x) + exf_half*pi )
cph The following is different in Large&Pond1981 code:
cph xsq = max(sqrt(abs(exf_one - 16.*htol)),exf_one)
              xsq   = SQRT( ABS(exf_one - htol*16. _d 0) )
              psixh = -psim_fac*htol*stable + (exf_one-stable)*
     &                 ( exf_two*LOG(exf_half*(exf_one+xsq)) )

C-   Shift wind speed using old coefficient
              usn = ws/(exf_one + rdn/karman*(zwln-psimh) )
              usm = MAX(usn, umin)

C-   Update the 10m, neutral stability transfer coefficients
              tmpbulk = cdrag_1/usm + cdrag_2 + cdrag_3*usm
              rdn = SQRT(tmpbulk)
              rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
              ren = cDalton

C-   Shift all coefficients to the measurement height and stability.
              rd = rdn/(exf_one + rdn*(zwln-psimh)/karman)
              rh = rhn/(exf_one + rhn*(ztln-psixh)/karman)
              re = ren/(exf_one + ren*(ztln-psixh)/karman)

C--  Update ustar, tstar, qstar using updated, shifted coefficients.
              ustar = rd*wsm
              qstar = re*delq
              tstar = rh*deltap

c end of iteration loop
            ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE ustar  = comlev1_exf_1, key = ikey_1
CADJ STORE qstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tstar  = comlev1_exf_1, key = ikey_1
CADJ STORE tau    = comlev1_exf_1, key = ikey_1
CADJ STORE cw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
CADJ STORE sw(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif

C-   Coeff:
cph Bug in Large&Pond1981 code?
cph tau = atmrho*ustar**2
cph No, all quantities later divided by ustar in orig. code
            tau   = atmrho*rd*ws

C-   Turbulent Fluxes
            hs(i,j,bi,bj) = atmcp*tau*tstar
            hl(i,j,bi,bj) = flamb*tau*qstar
#ifndef EXF_READ_EVAP
cdm !!! need to change sign and to convert from kg/m^2/s to m/s via recip_rhonil !!!
            evap(i,j,bi,bj) = -recip_rhonil*tau*qstar
#endif
            ustress(i,j,bi,bj) = tau*rd*ws*cw(i,j,bi,bj)
            vstress(i,j,bi,bj) = tau*rd*ws*sw(i,j,bi,bj)

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          else
            ustress(i,j,bi,bj) = 0. _d 0
            vstress(i,j,bi,bj) = 0. _d 0
            hflux  (i,j,bi,bj) = 0. _d 0
            evap   (i,j,bi,bj) = 0. _d 0
            hs(i,j,bi,bj)      = 0. _d 0
            hl(i,j,bi,bj)      = 0. _d 0

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          endif

#else  /* ifndef ALLOW_ATM_TEMP */
#ifdef ALLOW_ATM_WIND
              tmpbulk= exf_BulkCdn(sh(i,j,bi,bj))
              ustress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
     &                             uwind(i,j,bi,bj)
              vstress(i,j,bi,bj) = atmrho*tmpbulk*us(i,j,bi,bj)*
     &                             vwind(i,j,bi,bj)
#endif
#endif /* ifndef ALLOW_ATM_TEMP */
            enddo
          enddo
        enddo
      enddo

#endif /* ALLOW_BULKFORMULAE */

      END
