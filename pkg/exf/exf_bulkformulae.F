#include "EXF_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: EXF_BULKFORMULAE
C     !INTERFACE:
      SUBROUTINE EXF_BULKFORMULAE( exf_Tsf, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXF_BULKFORMULAE
C     | o Calculate bulk formula fluxes over open ocean
C     |   either following
C     |   Large and Pond, 1981 & 1982
C     |   or (if defined ALLOW_BULK_LARGEYEAGER04)
C     |   Large and Yeager, 2004, NCAR/TN-460+STR.
C     *==========================================================*
C     \ev
C
C
C     NOTES:
C     ======
C
C     See EXF_OPTIONS.h for a description of the various possible
C     ocean-model forcing configurations.
C
C     The bulk formulae of pkg/exf are not valid for sea-ice covered
C     oceans but they can be used in combination with a sea-ice model,
C     for example, pkg/seaice, to specify open water flux contributions.
C
C     ==================================================================
C
C     for Large and Pond, 1981 & 1982
C
C     The calculation of the bulk surface fluxes has been adapted from
C     the NCOM model which uses the formulae given in Large and Pond
C     (1981 & 1982 )
C
C
C     Header taken from NCOM version: ncom1.4.1
C     -----------------------------------------
C
C     Following procedures and coefficients in Large and Pond
C     (1981 ; 1982)
C
C     Output: Bulk estimates of the turbulent surface fluxes.
C     -------
C
C     hs  - sensible heat flux  (W/m^2), into ocean
C     hl  - latent   heat flux  (W/m^2), into ocean
C
C     Input:
C     ------
C
C     us  - mean wind speed (m/s)     at height hu (m)
C     th  - mean air temperature (K)  at height ht (m)
C     qh  - mean air humidity (kg/kg) at height hq (m)
C     sst - sea surface temperature (K)
C     tk0 - Kelvin temperature at 0 Celsius (K)
C
C     Assume 1) a neutral 10m drag coefficient =
C
C               cdn = .0027/u10 + .000142 + .0000764 u10
C
C            2) a neutral 10m stanton number =
C
C               ctn = .0327 sqrt(cdn), unstable
C               ctn = .0180 sqrt(cdn), stable
C
C            3) a neutral 10m dalton number =
C
C               cen = .0346 sqrt(cdn)
C
C            4) the saturation humidity of air at
C
C               t(k) = exf_BulkqSat(t)  (kg/m^3)
C
C     Note:  1) here, tstar = <wt>/u*, and qstar = <wq>/u*.
C            2) wind speeds should all be above a minimum speed,
C               say 0.5 m/s.
C            3) with optional iteration loop, niter=3, should suffice.
C            4) this version is for analyses inputs with hu = 10m and
C               ht = hq.
C            5) sst enters in Celsius.
C
C     ==================================================================
C
C       started: Christian Eckert eckert@mit.edu 27-Aug-1999
C
C     changed: Christian Eckert eckert@mit.edu 14-Jan-2000
C            - restructured the original version in order to have a
C              better interface to the MITgcmUV.
C
C            Christian Eckert eckert@mit.edu  12-Feb-2000
C            - Changed Routine names (package prefix: exf_)
C
C            Patrick Heimbach, heimbach@mit.edu  04-May-2000
C            - changed the handling of precip and sflux with respect
C              to CPP options ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP
C            - included some CPP flags ALLOW_BULKFORMULAE to make
C              sure ALLOW_ATM_TEMP, ALLOW_ATM_WIND are used only in
C              conjunction with defined ALLOW_BULKFORMULAE
C            - statement functions discarded
C
C            Ralf.Giering@FastOpt.de 25-Mai-2000
C            - total rewrite using new subroutines
C
C            Detlef Stammer: include river run-off. Nov. 21, 2001
C
C            heimbach@mit.edu, 10-Jan-2002
C            - changes to enable field swapping
C
C     mods for pkg/seaice: menemenlis@jpl.nasa.gov 20-Dec-2002
C
C     martin.losch@awi.de: merged with exf_bulk_largeyeager04, 21-May-2010
C
C     ==================================================================
C
C     for Large and Yeager, 2004
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
C     myTime  :: Current time in simulation
C     myIter  :: Current iteration number in simulation
C     myThid  :: My Thread Id number
C     exf_Tsf :: local copy of global field gcmSST or extrapolated
C                surface temperature (in deg Celsius)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      _RL exf_Tsf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     output:
CEOP

#ifdef ALLOW_BULKFORMULAE
#ifdef ALLOW_ATM_TEMP
C     == external Functions

C     == Local variables ==
C     i,j      :: grid point indices
C     bi,bj    :: tile indices
C     ssq      :: saturation specific humidity [kg/kg] in eq. with Sea-Surface water
      INTEGER i,j,bi,bj

      _RL czol
      _RL Tsf                   ! surface temperature [K]
      _RL wsm                   ! limited wind speed [m/s] (> umin)
      _RL t0                    ! virtual temperature [K]
C     these need to be 2D-arrays for vectorizing code
      _RL tstar (1:sNx,1:sNy)   ! turbulent temperature scale [K]
      _RL qstar (1:sNx,1:sNy)   ! turbulent humidity scale  [kg/kg]
      _RL ustar (1:sNx,1:sNy)   ! friction velocity [m/s]
      _RL tau   (1:sNx,1:sNy)   ! surface stress coef = rhoA * Ws * sqrt(Cd)
      _RL rdn   (1:sNx,1:sNy)   ! neutral, zref (=10m) values of rd
      _RL rd    (1:sNx,1:sNy)   ! = sqrt(Cd)          [-]
      _RL delq  (1:sNx,1:sNy)   ! specific humidity difference [kg/kg]
      _RL deltap(1:sNx,1:sNy)
#ifdef EXF_CALC_ATMRHO
      _RL atmrho_loc(1:sNx,1:sNy) ! local atmospheric density [kg/m^3]
#endif

#ifdef ALLOW_BULK_LARGEYEAGER04
      _RL dzTmp
#endif
      _RL ssq
      _RL re                    ! = Ce / sqrt(Cd)     [-]
      _RL rh                    ! = Ch / sqrt(Cd)     [-]
      _RL ren, rhn              ! neutral, zref (=10m) values of re, rh
      _RL usn, usm
      _RL stable                ! = 1 if stable ; = 0 if unstable
      _RL huol                  ! stability parameter at zwd [-] (=z/Monin-Obuklov length)
      _RL htol                  ! stability parameter at zth [-]
      _RL hqol
      _RL x                     ! stability function  [-]
      _RL xsq                   ! = x^2               [-]
      _RL psimh                 ! momentum stability function
      _RL psixh                 ! latent & sensib. stability function
      _RL zwln                  ! = log(zwd/zref)
      _RL ztln                  ! = log(zth/zref)
      _RL tmpbulk
      _RL recip_rhoConstFresh
      INTEGER ks, kl
      INTEGER iter
C     solve4Stress :: if F, by-pass momentum turb. flux (wind-stress) calculation
      LOGICAL solve4Stress
      _RL windStress            ! surface wind-stress (@ grid cell center)

#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER ikey, ikey_1, ikey_2
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( useAtmWind ) THEN
        solve4Stress = .TRUE.
      ELSE
#ifdef ALLOW_BULK_LARGEYEAGER04
        solve4Stress = wspeedfile .NE. ' '
#else
        solve4Stress = .FALSE.
#endif
      ENDIF

      IF ( usingPCoords ) THEN
       ks = Nr
       kl = Nr-1
      ELSE
       ks = 1
       kl = 2
      ENDIF

C--   Set surface parameters :
      zwln = LOG(hu/zref)
      ztln = LOG(ht/zref)
      czol = hu*karman*gravity_mks
C--   abbreviation
      recip_rhoConstFresh = 1. _d 0/rhoConstFresh

C     Loop over tiles.
#ifdef ALLOW_AUTODIFF_TAMC
C--   HPF directive to help TAF
CHPF$ INDEPENDENT
#endif
      DO bj = myByLo(myThid),myByHi(myThid)
#ifdef ALLOW_AUTODIFF_TAMC
C--    HPF directive to help TAF
CHPF$  INDEPENDENT
#endif
       DO bi = myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        ikey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif
        DO j = 1,sNy
         DO i = 1,sNx

#ifdef ALLOW_AUTODIFF_TAMC
          ikey_1 = i + (j-1)*sNx + (ikey-1)*sNx*sNy
#endif

#ifdef ALLOW_AUTODIFF
          deltap(i,j) = 0. _d 0
          delq(i,j)   = 0. _d 0
#endif
C--- Compute turbulent surface fluxes
C-   Pot. Temp and saturated specific humidity
          IF ( atemp(i,j,bi,bj) .NE. 0. _d 0 ) THEN
C-   Surface Temp.
           Tsf = exf_Tsf(i,j,bi,bj)
           tmpbulk = cvapor_fac*exp(-cvapor_exp/Tsf)
#ifdef EXF_CALC_ATMRHO
           atmrho_loc(i,j) = apressure(i,j,bi,bj) /
     &                  (287.04 _d 0*atemp(i,j,bi,bj)
     &                  *(1. _d 0 + humid_fac*aqh(i,j,bi,bj)))
           ssq = saltsat*tmpbulk/atmrho_loc(i,j)
#else
           ssq = saltsat*tmpbulk/atmrho
#endif
           deltap(i,j) = atemp(i,j,bi,bj) + gamma_blk*ht - Tsf
           delq(i,j)   = aqh(i,j,bi,bj) - ssq
C--  no negative evaporation over ocean:
           IF ( noNegativeEvap ) delq(i,j) = MIN( 0. _d 0, delq(i,j) )

C--  initial guess for exchange coefficients:
C    take U_N = del.U ; stability from del.Theta ;
           stable = exf_half + SIGN(exf_half, deltap(i,j))

           IF ( solve4Stress ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE sh(i,j,bi,bj)     = comlev1_exf_1, key = ikey_1
#endif /* ALLOW_AUTODIFF_TAMC */
C--   Wind speed
            wsm        = sh(i,j,bi,bj)
#ifdef  ALLOW_DRAG_LARGEYEAGER09
C     Large and Yeager (2009), Climate Dynamics, equation 11a/b
            tmpbulk =  cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
     &           + cdrag_8 * wsm**6
            tmpbulk = exf_scal_BulkCdn * (
     &             ( halfRL - SIGN(halfRL, wsm-umax) )*tmpbulk
     &           + ( halfRL + SIGN(halfRL, wsm-umax) )*cdragMax
     &           )
#else
            tmpbulk    = exf_scal_BulkCdn *
     &           ( cdrag_1/wsm + cdrag_2 + cdrag_3*wsm )
#endif
            rdn(i,j)   = SQRT(tmpbulk)
            ustar(i,j) = rdn(i,j)*wsm
           ELSE
            rdn(i,j)   = 0. _d 0
            windStress = wStress(i,j,bi,bj)
#ifdef EXF_CALC_ATMRHO
            ustar(i,j) = SQRT(windStress/atmrho_loc(i,j))
            tau(i,j)   = SQRT(windStress*atmrho_loc(i,j))
#else
            ustar(i,j) = SQRT(windStress/atmrho)
c           tau(i,j)   = windStress/ustar(i,j)
            tau(i,j)   = SQRT(windStress*atmrho)
#endif
           ENDIF

C--  initial guess for exchange other coefficients:
           rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
           ren = cDalton
C--  calculate turbulent scales
           tstar(i,j)=rhn*deltap(i,j)
           qstar(i,j)=ren*delq(i,j)

          ELSE
C     atemp(i,j,bi,bj) .EQ. 0.
           tstar (i,j) = 0. _d 0
           qstar (i,j) = 0. _d 0
           ustar (i,j) = 0. _d 0
           tau   (i,j) = 0. _d 0
           rdn   (i,j) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO
        DO iter=1,niter_bulk
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( atemp(i,j,bi,bj) .NE. 0. _d 0 ) THEN
C--- iterate with psi-functions to find transfer coefficients

#ifdef ALLOW_AUTODIFF_TAMC
            ikey_2 = i + (j-1)*sNx + (iter-1)*sNx*sNy
     &           + (ikey-1)*sNx*sNy*niter_bulk
CADJ STORE rdn   (i,j)       = comlev1_exf_2, key = ikey_2
CADJ STORE ustar (i,j)       = comlev1_exf_2, key = ikey_2
CADJ STORE qstar (i,j)       = comlev1_exf_2, key = ikey_2
CADJ STORE tstar (i,j)       = comlev1_exf_2, key = ikey_2
CADJ STORE sh    (i,j,bi,bj) = comlev1_exf_2, key = ikey_2
CADJ STORE wspeed(i,j,bi,bj) = comlev1_exf_2, key = ikey_2
#endif

            t0   = atemp(i,j,bi,bj)*
     &           (exf_one + humid_fac*aqh(i,j,bi,bj))
            huol = ( tstar(i,j)/t0 +
     &               qstar(i,j)/(exf_one/humid_fac+aqh(i,j,bi,bj))
     &              )*czol/(ustar(i,j)*ustar(i,j))
#ifdef ALLOW_BULK_LARGEYEAGER04
            tmpbulk = MIN(abs(huol),10. _d 0)
            huol   = SIGN(tmpbulk , huol)
#else
C--   Large&Pond1981:
            huol   = max(huol,zolmin)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
            htol   = huol*ht/hu
            hqol   = huol*hq/hu
            stable = exf_half + sign(exf_half, huol)

C     Evaluate all stability functions assuming hq = ht.
            IF ( solve4Stress ) THEN
#ifdef ALLOW_BULK_LARGEYEAGER04
C--   Large&Yeager04:
             xsq    = SQRT( ABS(exf_one - huol*16. _d 0) )
#else
C--   Large&Pond1981:
             xsq    = MAX(SQRT(ABS(exf_one - 16.*huol)),exf_one)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
             x      = SQRT(xsq)
             psimh  = -psim_fac*huol*stable
     &               +(exf_one-stable)*
     &                ( LOG( (exf_one + exf_two*x + xsq)*
     &                       (exf_one+xsq)*.125 _d 0 )
     &                  -exf_two*ATAN(x) + exf_half*pi )
            ELSE
             psimh  = 0. _d 0
            ENDIF
#ifdef ALLOW_BULK_LARGEYEAGER04
C--   Large&Yeager04:
            xsq   = SQRT( ABS(exf_one - htol*16. _d 0) )
#else
C--   Large&Pond1981:
            xsq   = MAX(SQRT(ABS(exf_one - 16.*htol)),exf_one)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
            psixh = -psim_fac*htol*stable + (exf_one-stable)*
     &               ( exf_two*LOG(exf_half*(exf_one+xsq)) )

            IF ( solve4Stress ) THEN
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rdn(i,j)       = comlev1_exf_2, key = ikey_2
#endif
C-   Shift wind speed using old coefficient
#ifdef ALLOW_BULK_LARGEYEAGER04
C--   Large&Yeager04:
             dzTmp = (zwln-psimh)/karman
             usn   = wspeed(i,j,bi,bj)/(exf_one + rdn(i,j)*dzTmp )
#else
C--   Large&Pond1981:
c           rd(i,j)= rdn(i,j)/(exf_one - rdn(i,j)/karman*psimh )
c           usn    = sh(i,j,bi,bj)*rd(i,j)/rdn(i,j)
C     ML: the original formulation above is replaced below to be
C     similar to largeyeager04, but does not give the same results, strange
             usn   = sh(i,j,bi,bj)/(exf_one - rdn(i,j)/karman*psimh)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
             usm   = MAX(usn, umin)

C-   Update the 10m, neutral stability transfer coefficients (momentum)
#ifdef  ALLOW_DRAG_LARGEYEAGER09
C     Large and Yeager (2009), J.Clim equation 11a/b
             tmpbulk =  cdrag_1/usm + cdrag_2 + cdrag_3*usm
     &            + cdrag_8 * usm**6
             tmpbulk = exf_scal_BulkCdn * (
     &              ( halfRL - SIGN(halfRL, usm-umax) )*tmpbulk
     &            + ( halfRL + SIGN(halfRL, usm-umax) )*cdragMax
     &            )
#else
             tmpbulk  = exf_scal_BulkCdn *
     &            ( cdrag_1/usm + cdrag_2 + cdrag_3*usm )
#endif
             rdn(i,j)   = SQRT(tmpbulk)
#ifdef ALLOW_BULK_LARGEYEAGER04
             rd(i,j)    = rdn(i,j)/(exf_one + rdn(i,j)*dzTmp)
#else
             rd(i,j)    = rdn(i,j)/(exf_one - rdn(i,j)/karman*psimh)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
             ustar(i,j) = rd(i,j)*sh(i,j,bi,bj)
C-   Coeff:
#ifdef EXF_CALC_ATMRHO
             tau(i,j)   = atmrho_loc(i,j)*rd(i,j)*wspeed(i,j,bi,bj)
#else
             tau(i,j)   = atmrho*rd(i,j)*wspeed(i,j,bi,bj)
#endif
            ENDIF

C-   Update the 10m, neutral stability transfer coefficients (sens&evap)
            rhn = (exf_one-stable)*cstanton_1 + stable*cstanton_2
            ren = cDalton

C-   Shift all coefficients to the measurement height and stability.
            rh = rhn/(exf_one + rhn*(ztln-psixh)/karman)
            re = ren/(exf_one + ren*(ztln-psixh)/karman)

C--  Update ustar, tstar, qstar using updated, shifted coefficients.
            qstar(i,j) = re*delq(i,j)
            tstar(i,j) = rh*deltap(i,j)

           ENDIF
          ENDDO
         ENDDO
C end of iteration loop
        ENDDO
        DO j = 1,sNy
         DO i = 1,sNx
          IF ( atemp(i,j,bi,bj) .NE. 0. _d 0 ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
           ikey_1 = i + (j-1)*sNx + (ikey-1)*sNx*sNy
CADJ STORE qstar(i,j)    = comlev1_exf_1, key = ikey_1
CADJ STORE tstar(i,j)    = comlev1_exf_1, key = ikey_1
CADJ STORE tau  (i,j)    = comlev1_exf_1, key = ikey_1
CADJ STORE rd   (i,j)    = comlev1_exf_1, key = ikey_1
#endif /* ALLOW_AUTODIFF_TAMC */

C-   Turbulent Fluxes
           hs(i,j,bi,bj) = atmcp*tau(i,j)*tstar(i,j)
           hl(i,j,bi,bj) = flamb*tau(i,j)*qstar(i,j)
#ifndef EXF_READ_EVAP
C   change sign and convert from kg/m^2/s to m/s via rhoConstFresh
c          evap(i,j,bi,bj) = -recip_rhonil*tau(i,j)*qstar(i,j)
           evap(i,j,bi,bj) = -recip_rhoConstFresh*tau(i,j)*qstar(i,j)
C   but older version was using rhonil instead:
c           evap(i,j,bi,bj) = -recip_rhonil*tau(i,j)*qstar(i,j)
#endif
           IF ( useAtmWind .AND. useRelativeWind ) THEN
            tmpbulk =  tau(i,j)*rd(i,j)
            ustress(i,j,bi,bj) = tmpbulk* ( uwind(i,j,bi,bj) -
     &           0.5 _d 0 * (uVel(i,j,ks,bi,bj)+uVel(i+1,j,ks,bi,bj)) )
            vstress(i,j,bi,bj) = tmpbulk* ( vwind(i,j,bi,bj) -
     &           0.5 _d 0 * (vVel(i,j,ks,bi,bj)+vVel(i,j+1,ks,bi,bj)) )
           ELSEIF ( useAtmWind ) THEN
c           ustress(i,j,bi,bj) = tau(i,j)*rd(i,j)*ws*cw(i,j,bi,bj)
c           vstress(i,j,bi,bj) = tau(i,j)*rd(i,j)*ws*sw(i,j,bi,bj)
C- jmc: below is how it should be written ; different from above when
C       both wind-speed & 2 compon. of the wind are specified, and in
C-      this case, formula below is better.
            tmpbulk =  tau(i,j)*rd(i,j)
            ustress(i,j,bi,bj) = tmpbulk*uwind(i,j,bi,bj)
            vstress(i,j,bi,bj) = tmpbulk*vwind(i,j,bi,bj)
           ENDIF

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          ELSE
           IF ( useAtmWind ) ustress(i,j,bi,bj) = 0. _d 0
           IF ( useAtmWind ) vstress(i,j,bi,bj) = 0. _d 0
           hflux  (i,j,bi,bj) = 0. _d 0
           evap   (i,j,bi,bj) = 0. _d 0
           hs     (i,j,bi,bj) = 0. _d 0
           hl     (i,j,bi,bj) = 0. _d 0

c IF ( ATEMP(i,j,bi,bj) .NE. 0. )
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* ALLOW_ATM_TEMP */
#endif /* ALLOW_BULKFORMULAE */

      RETURN
      END
