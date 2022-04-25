#include "THSICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_EXF
#include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: THSICE_GET_EXF
C     !INTERFACE:
      SUBROUTINE THSICE_GET_EXF(
     I                  bi, bj, it2,
     I                  iMin,iMax, jMin,jMax,
     I                  icFlag, hSnow1, tsfCel,
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
# include "THSICE_SIZE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     bi,bj       :: tile indices
C     it          :: solv4temp iteration
C     iMin,iMax   :: computation domain: 1rst index range
C     jMin,jMax   :: computation domain: 2nd  index range
C     icFlag     :: True= get fluxes at this location ; False= do nothing
C     hSnow1       :: snow height [m]
C     tsfCel      :: surface (ice or snow) temperature (oC)
C     flxExcSw    :: net (downward) surface heat flux, except short-wave [W/m2]
C     dFlxdT      :: deriv of flx with respect to Tsf    [W/m/K]
C     evapLoc     :: surface evaporation (>0 if evaporate) [kg/m2/s]
C     dEvdT       :: deriv of evap. with respect to Tsf  [kg/m2/s/K]
C     myTime      :: current Time of simulation [s]
C     myIter      :: current Iteration number in simulation
C     myThid      :: my Thread Id number
      INTEGER bi, bj
      INTEGER it2
      INTEGER iMin, iMax
      INTEGER jMin, jMax
      _RL     icFlag  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL     hSnow1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
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
#ifdef ALLOW_DOWNWARD_RADIATION

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     === Local variables ===
C     hsLocal, hlLocal :: sensible & latent heat flux over sea-ice
C             t0       :: virtual temperature (K)
C             ssq      :: saturation specific humidity (kg/kg)
C             deltap   :: potential temperature diff (K)
      _RL hsLocal
      _RL hlLocal
      INTEGER iter
      INTEGER i, j
      _RL czol
      _RL wsm                ! limited wind speed [m/s] (> umin)
      _RL t0                 ! virtual temperature [K]
C     copied from exf_bulkformulae:
C     these need to be 2D-arrays for vectorizing code
C     turbulent temperature scale [K]
      _RL tstar (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     turbulent humidity scale  [kg/kg]
      _RL qstar (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     friction velocity [m/s]
      _RL ustar (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     neutral, zref (=10m) values of rd
      _RL rdn   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL rd    (1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! = sqrt(Cd)          [-]
      _RL rh    (1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! = Ch / sqrt(Cd)     [-]
      _RL re    (1-OLx:sNx+OLx,1-OLy:sNy+OLy) ! = Ce / sqrt(Cd)     [-]
C     specific humidity difference [kg/kg]
      _RL delq  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL deltap(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef EXF_CALC_ATMRHO
C     local atmospheric density [kg/m^3]
      _RL atmrho_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif
C
      _RL ssq
      _RL ren, rhn           ! neutral, zref (=10m) values of re, rh
      _RL usn, usm           ! neutral, zref (=10m) wind-speed (+limited)
      _RL stable             ! = 1 if stable ; = 0 if unstable
C     stability parameter at zwd [-] (=z/Monin-Obuklov length)
      _RL huol
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
      _RL dflhdT
      _RL dfshdT
      _RL dflwupdT
C     emissivities, called emittance in exf
      _RL     emiss
C     Tsf    :: surface temperature [K]
C     Ts2    :: surface temperature square [K^2]
      _RL Tsf
      _RL Ts2
C     latent heat of evaporation or sublimation [J/kg]
      _RL lath
      _RL qsat_fac
      _RL qsat_exp
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ticekey, ikeyexf, ikeynb, ikey_5
#endif
#ifdef ALLOW_DBUG_THSICE
      LOGICAL dBugFlag
      INTEGER stdUnit
#endif

C     == external functions ==

c     _RL       exf_BulkqSat
c     external  exf_BulkqSat
c     _RL       exf_BulkCdn
c     external  exf_BulkCdn
c     _RL       exf_BulkRhn
c     external  exf_BulkRhn

C     == end of interface ==

C-    Define grid-point location where to print debugging values
#include "THSICE_DEBUG.h"

#ifdef ALLOW_DBUG_THSICE
      dBugFlag = debugLevel.GE.debLevC
      stdUnit = standardMessageUnit
#endif

C--   Set surface parameters :
      zwln = LOG(hu/zref)
      ztln = LOG(ht/zref)
      czol = hu*karman*gravity_mks
      ren  = cDalton
C     more abbreviations
      lath     = flamb+flami
      qsat_fac = cvapor_fac_ice
      qsat_exp = cvapor_exp_ice

C     initialisation of local arrays
      DO j = 1-OLy,sNy+OLy
       DO i = 1-OLx,sNx+OLx
        tstar(i,j)  = 0. _d 0
        qstar(i,j)  = 0. _d 0
        ustar(i,j)  = 0. _d 0
        rdn(i,j)    = 0. _d 0
        rd(i,j)     = 0. _d 0
        rh(i,j)     = 0. _d 0
        re(i,j)     = 0. _d 0
        delq(i,j)   = 0. _d 0
        deltap(i,j) = 0. _d 0
       ENDDO
      ENDDO
C
#ifdef ALLOW_AUTODIFF_TAMC
      act1 = bi - myBxLo(myThid)
      max1 = myBxHi(myThid) - myBxLo(myThid) + 1
      act2 = bj - myByLo(myThid)
      max2 = myByHi(myThid) - myByLo(myThid) + 1
      act3 = myThid - 1
      max3 = nTx*nTy
      act4 = ikey_dynamics - 1
      ticekey = (act1 + 1) + act2*max1
     &                     + act3*max1*max2
     &                     + act4*max1*max2*max3
      ikeyexf = (ticekey-1)*MaxTsf + it2
#endif
      DO j=jMin,jMax
       DO i=iMin,iMax
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_DBUG_THSICE
        IF ( dBug(i,j,bi,bj) .AND. (icFlag(i,j).GT.0. _d 0) )
     &    WRITE(stdUnit,'(A,2I4,2I2,2F12.6)')
     &    'ThSI_GET_EXF: i,j,atemp,lwd=',
     &           i,j,bi,bj, atemp(i,j,bi,bj),lwdown(i,j,bi,bj)
#endif

C--   Use atmospheric state to compute surface fluxes.
        IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &       (atemp(i,j,bi,bj).NE.0. _d 0) ) THEN
         IF ( hSnow1(i,j).GT.3. _d -1 ) THEN
          emiss = snow_emissivity
         ELSE
          emiss = ice_emissivity
         ENDIF
C     copy a few variables to names used in bulkf_formula_lay
         Tsf         = tsfCel(i,j)+cen2kel
         Ts2         = Tsf*Tsf
C     wind speed
         wsm         = sh(i,j,bi,bj)
C--   air - surface difference of temperature & humidity
c        tmpbulk= exf_BulkqSat(Tsf)
c        ssq         = saltsat*tmpbulk/atmrho
         tmpbulk     = qsat_fac*EXP(-qsat_exp/Tsf)
#ifdef EXF_CALC_ATMRHO
         atmrho_loc(i,j) = apressure(i,j,bi,bj) /
     &                  (287.04 _d 0*atemp(i,j,bi,bj)
     &                  *(1. _d 0 + humid_fac*aqh(i,j,bi,bj)))
         ssq         = tmpbulk/atmrho_loc(i,j)
#else
         ssq         = tmpbulk/atmrho
#endif
         deltap(i,j) = atemp(i,j,bi,bj) + gamma_blk*ht - Tsf
         delq(i,j)   = aqh(i,j,bi,bj) - ssq
C     Do the part of the output variables that do not depend
C     on the ice here to save a few re-computations
C     This is not yet dEvdT, but just a cheap way to save a 2D-field
C     for ssq and recomputing Ts2 lateron
         dEvdT(i,j)  = ssq*qsat_exp/Ts2
         flwup       = emiss*stefanBoltzmann*Ts2*Ts2
         dflwupdT    = emiss*stefanBoltzmann*Ts2*Tsf * 4. _d 0
c        flwNet_dwn  =       lwdown(i,j,bi,bj) - flwup
C-    assume long-wave albedo = 1 - emissivity :
         flwNet_dwn  = emiss*lwdown(i,j,bi,bj) - flwup
C--   This is not yet the total derivative with respect to surface temperature
         dFlxdT(i,j)   = -dflwupdT
C--   This is not yet the Net downward radiation excluding shortwave
         flxExcSw(i,j) = flwNet_dwn
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( useStabilityFct_overIce ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
C--
         IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &        (atemp(i,j,bi,bj).NE.0. _d 0) ) THEN
C--   Compute the turbulent surface fluxes (function of stability).

C             Initial guess: z/l=0.0; hu=ht=hq=z
C             Iterations:    converge on z/l and hence the fluxes.

          t0         = atemp(i,j,bi,bj)*
     &         (exf_one + humid_fac*aqh(i,j,bi,bj))
          stable     = exf_half + SIGN(exf_half, deltap(i,j))
c         tmpbulk    = exf_BulkCdn(sh(i,j,bi,bj))
          wsm        = sh(i,j,bi,bj)
          tmpbulk    = cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
          IF (tmpbulk.NE.0.) THEN
           rdn(i,j)   = SQRT(tmpbulk)
          ELSE
           rdn(i,j)   = 0. _d 0
          ENDIF
C--  initial guess for exchange other coefficients:
c         rhn        = exf_BulkRhn(stable)
          rhn        = (exf_one-stable)*cstanton_1 + stable*cstanton_2
C--  calculate turbulent scales
          ustar(i,j) = rdn(i,j)*wsm
          tstar(i,j) = rhn*deltap(i,j)
          qstar(i,j) = ren*delq(i,j)
         ENDIF
        ENDDO
       ENDDO

C     start iteration
       DO iter = 1,niter_bulk
#ifdef ALLOW_AUTODIFF_TAMC
        ikeynb = (ikeyexf-1)*niter_bulk + iter
CADJ STORE ustar = comlev1_thsice_exf_niter, key = ikeynb, kind = isbyte
CADJ STORE qstar = comlev1_thsice_exf_niter, key = ikeynb, kind = isbyte
CADJ STORE tstar = comlev1_thsice_exf_niter, key = ikeynb, kind = isbyte
#endif
        DO j=jMin,jMax
         DO i=iMin,iMax
          IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &         (atemp(i,j,bi,bj).NE.0. _d 0) ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
           ikey_5 = iter
     &            + niter_bulk*(i-1)
     &            + niter_bulk*sNx*(j-1)
     &            + (ikeynb-1)*sNx*sNy
C     Not sure why this has to be done here and not outside the ij-loop.
CADJ STORE rdn(i,j) = comlev1_thsice_5, key = ikey_5, kind = isbyte
#endif

           t0     = atemp(i,j,bi,bj)*
     &          (exf_one + humid_fac*aqh(i,j,bi,bj))
           huol   = (tstar(i,j)/t0 +
     &               qstar(i,j)/(exf_one/humid_fac+aqh(i,j,bi,bj))
     &              )*czol/(ustar(i,j)*ustar(i,j))
#ifdef ALLOW_BULK_LARGEYEAGER04
C-    Large&Yeager_2004 code:
           huol   = MIN( MAX(-10. _d 0,huol), 10. _d 0 )
#else
C-    Large&Pond_1981 code (zolmin default = -100):
           huol   = MAX(huol,zolmin)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
           htol   = huol*ht/hu
           hqol   = huol*hq/hu
           stable = exf_half + SIGN(exf_half, huol)

C     Evaluate all stability functions assuming hq = ht.
#ifdef ALLOW_BULK_LARGEYEAGER04
C-    Large&Yeager_2004 code:
           xsq    = SQRT( ABS(exf_one - huol*16. _d 0) )
#else
C-    Large&Pond_1981 code:
           xsq    = MAX(SQRT(ABS(exf_one - huol*16. _d 0)),exf_one)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
           x      = SQRT(xsq)
           psimh  = -psim_fac*huol*stable
     &             + (exf_one-stable)
     &             *( LOG( (exf_one + exf_two*x + xsq)
     &                    *(exf_one+xsq)*0.125 _d 0 )
     &                -exf_two*ATAN(x) + exf_half*pi )
#ifdef ALLOW_BULK_LARGEYEAGER04
C-    Large&Yeager_2004 code:
           xsq    = SQRT( ABS(exf_one - htol*16. _d 0) )
#else
C-    Large&Pond_1981 code:
           xsq    = MAX(SQRT(ABS(exf_one - htol*16. _d 0)),exf_one)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
           psixh  = -psim_fac*htol*stable
     &            + (exf_one-stable)
     &              *exf_two*LOG( exf_half*(exf_one+xsq) )

C     Shift wind speed using old coefficient
#ifdef ALLOW_BULK_LARGEYEAGER04
C--   Large&Yeager04:
           usn    = wspeed(i,j,bi,bj)
     &           /( exf_one + rdn(i,j)*(zwln-psimh)/karman )
#else
C--   Large&Pond1981:
           usn   = sh(i,j,bi,bj)/(exf_one - rdn(i,j)/karman*psimh)
#endif /* ALLOW_BULK_LARGEYEAGER04 */
           usm    = MAX(usn, umin)

C-    Update the 10m, neutral stability transfer coefficients
c          tmpbulk= exf_BulkCdn(usm)
           tmpbulk= cdrag_1/usm + cdrag_2 + cdrag_3*usm
           rdn(i,j) = SQRT(tmpbulk)
c          rhn    = exf_BulkRhn(stable)
           rhn    = (exf_one-stable)*cstanton_1 + stable*cstanton_2

C     Shift all coefficients to the measurement height and stability.
#ifdef ALLOW_BULK_LARGEYEAGER04
           rd(i,j)= rdn(i,j)/( exf_one + rdn(i,j)*(zwln-psimh)/karman )
#else
           rd(i,j)= rdn(i,j)/( exf_one - rdn(i,j)/karman*psimh )
#endif /* ALLOW_BULK_LARGEYEAGER04 */
           rh(i,j)= rhn/( exf_one + rhn*(ztln-psixh)/karman )
           re(i,j)= ren/( exf_one + ren*(ztln-psixh)/karman )

C     Update ustar, tstar, qstar using updated, shifted coefficients.
           ustar(i,j)  = rd(i,j)*sh(i,j,bi,bj)
           qstar(i,j)  = re(i,j)*delq(i,j)
           tstar(i,j)  = rh(i,j)*deltap(i,j)
          ENDIF
C     end i/j-loops
         ENDDO
        ENDDO
C     end iteration loop
       ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     these stores avoid recomputing this routine in the ad-code (not
C     strictly necessary)
CADJ STORE rd    = comlev1_thsice_exf, key = ikeyexf, kind = isbyte
CADJ STORE rh    = comlev1_thsice_exf, key = ikeyexf, kind = isbyte
CADJ STORE re    = comlev1_thsice_exf, key = ikeyexf, kind = isbyte
CADJ STORE qstar = comlev1_thsice_exf, key = ikeyexf, kind = isbyte
CADJ STORE tstar = comlev1_thsice_exf, key = ikeyexf, kind = isbyte
#endif
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &        (atemp(i,j,bi,bj).NE.0. _d 0) ) THEN
#ifdef EXF_CALC_ATMRHO
          tau     = atmrho_loc(i,j)*rd(i,j)*wspeed(i,j,bi,bj)
#else
          tau     = atmrho*rd(i,j)*wspeed(i,j,bi,bj)
#endif
          evapLoc(i,j)  = -tau*qstar(i,j)
          hlLocal       = -lath*evapLoc(i,j)
          hsLocal       = atmcp*tau*tstar(i,j)
c         ustress = tau*rd(i,j)*UwindSpeed
c         vstress = tau*rd(i,j)*VwindSpeed

C---  surf.Temp derivative of turbulent Fluxes
C     complete computation of dEvdT
          dEvdT(i,j)    = (tau*re(i,j))*dEvdT(i,j)
          dflhdT        = -lath*dEvdT(i,j)
          dfshdT        = -atmcp*tau*rh(i,j)
C--   Update total derivative with respect to surface temperature
          dFlxdT(i,j)   = dFlxdT(i,j)   + dfshdT  + dflhdT
C--   Update net downward radiation excluding shortwave
          flxExcSw(i,j) = flxExcSw(i,j) + hsLocal + hlLocal

         ENDIF
        ENDDO
       ENDDO
      ELSE
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Compute the turbulent surface fluxes using fixed transfert Coeffs
C     with no stability dependence ( useStabilityFct_overIce = false )
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &        (atemp(i,j,bi,bj).NE.0. _d 0) ) THEN
          wsm           = sh(i,j,bi,bj)
#ifdef EXF_CALC_ATMRHO
          tau           = atmrho_loc(i,j)*exf_iceCe*wsm
#else
          tau           = atmrho*exf_iceCe*wsm
#endif
          evapLoc(i,j)  = -tau*delq(i,j)
          hlLocal       = -lath*evapLoc(i,j)
#ifdef EXF_CALC_ATMRHO
          hsLocal       = atmcp*atmrho_loc(i,j)
     &                                *exf_iceCh*wsm*deltap(i,j)
#else
          hsLocal       = atmcp*atmrho*exf_iceCh*wsm*deltap(i,j)
#endif
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,'(A,4F12.6)')
     &      'ThSI_GET_EXF: wsm,hl,hs,Lw=',
     &                     wsm,hlLocal,hsLocal,flxExcSw(i,j)
#endif
C---  surf.Temp derivative of turbulent Fluxes
C     complete computation of dEvdT
          dEvdT(i,j)    = tau*dEvdT(i,j)
          dflhdT        = -lath*dEvdT(i,j)
#ifdef EXF_CALC_ATMRHO
          dfshdT        = -atmcp*atmrho_loc(i,j)*exf_iceCh*wsm
#else
          dfshdT        = -atmcp*atmrho*exf_iceCh*wsm
#endif
C--   Update total derivative with respect to surface temperature
          dFlxdT(i,j)   = dFlxdT(i,j)   + dfshdT  + dflhdT
C--   Update net downward radiation excluding shortwave
          flxExcSw(i,j) = flxExcSw(i,j) + hsLocal + hlLocal
#ifdef ALLOW_DBUG_THSICE
          IF ( dBug(i,j,bi,bj) ) WRITE(stdUnit,'(A,4F12.6)')
     &      'ThSI_GET_EXF: flx,dFlxdT,evap,dEvdT',
     &       flxExcSw(i,j), dFlxdT(i,j), evapLoc(i,j),dEvdT(i,j)
#endif
         ENDIF
        ENDDO
       ENDDO
C     endif useStabilityFct_overIce
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      DO j=jMin,jMax
       DO i=iMin,iMax
        IF ( (icFlag(i,j).GT.0. _d 0) .AND.
     &       (atemp(i,j,bi,bj).LE.0. _d 0) ) THEN
C--   in case atemp is zero:
         flxExcSw(i,j) = 0. _d 0
         dFlxdT  (i,j) = 0. _d 0
         evapLoc (i,j) = 0. _d 0
         dEvdT   (i,j) = 0. _d 0
        ENDIF
       ENDDO
      ENDDO

#else /* ALLOW_DOWNWARD_RADIATION */
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: DOWNWARD_RADIATION undef'
#endif /* ALLOW_DOWNWARD_RADIATION */
#else /* ALLOW_ATM_TEMP */
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: ATM_TEMP undef'
#endif /* ALLOW_ATM_TEMP */
#ifdef EXF_READ_EVAP
      STOP 'ABNORMAL END: S/R THSICE_GET_EXF: EXF_READ_EVAP defined'
#endif /* EXF_READ_EVAP */
#endif /* ALLOW_EXF */

      RETURN
      END
