#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_THERMODYNAMICS
C     !INTERFACE:
      SUBROUTINE SHELFICE_THERMODYNAMICS(
     I                        myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *=============================================================*
C     | S/R  SHELFICE_THERMODYNAMICS
C     | o shelf-ice main routine.
C     |   compute temperature and (virtual) salt flux at the
C     |   shelf-ice ocean interface
C     |
C     | stresses at the ice/water interface are computed in separate
C     | routines that are called from mom_fluxform/mom_vecinv
C     *=============================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SHELFICE.h"
#ifdef ALLOW_CTRL
# include "CTRL_SIZE.h"
# include "CTRL.h"
# include "CTRL_DUMMY.h"
# ifdef ALLOW_GENTIM2D_CONTROL
#  include "CTRL_GENARR.h"
# endif
#endif /* ALLOW_CTRL */
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES :
C     === Local variables ===
C     i,j,k,kp1,bi,bj  :: loop counters
C     tLoc, sLoc, pLoc :: local in-situ temperature, salinity, pressure
C     theta/saltFreeze :: temperature and salinity of water at the
C                         ice-ocean interface (at the freezing point)
C     freshWaterFlux   :: local variable for fresh water melt flux due
C                         to melting in kg/m^2/s
C                         (negative density x melt rate)
C     convertFW2SaltLoc:: local copy of convertFW2Salt
C     cFac             :: 1 for conservative form, 0, otherwise
C     rFac             :: realFreshWaterFlux factor
C     dFac             :: 0 for diffusive heat flux (Holland and Jenkins, 1999,
C                           eq21)
C                         1 for advective and diffusive heat flux (eq22, 26, 31)
C     fwflxFac         :: only effective for dFac=1, 1 if we expect a melting
C                         fresh water flux, 0 otherwise
C     rFWinBL          :: = 1 when realFreshWaterFlux is used with BoundaryLayer
C     auxiliary variables and abbreviations:
C     a0, a1, a2, b0, c0
C     eps1, eps2, eps3, eps3a, eps4, eps6, eps7, eps8, epsq
C     aqe, bqe, cqe, discrim, recip_aqe
C     drKp1, recip_drLoc
      INTEGER i,j,k,kp1
      INTEGER bi,bj
      _RL tLoc(1:sNx,1:sNy)
      _RL sLoc(1:sNx,1:sNy)
      _RL pLoc(1:sNx,1:sNy)
      _RL uLoc(1:sNx+1,1:sNy+1)
      _RL vLoc(1:sNx+1,1:sNy+1)
      _RL velSq(1:sNx,1:sNy)
      _RL tTMP, sTMP, uTMP, vTMP, vSqTmp
      _RL thetaFreeze, saltFreeze, recip_Cp
      _RL freshWaterFlux
#ifdef ALLOW_ISOMIP_TD
      _RL convertFW2SaltLoc
#endif
      _RL a0, a1, a2, b0, c0
      _RL eps1, eps2, eps3, eps3a, eps4, eps6, eps7, eps8, epsq
      _RL cFac, rFac, dFac, fwflxFac, rFWinBL
      _RL aqe, bqe, cqe, discrim, recip_aqe
      _RL drKp1, recip_drLoc
      _RL recip_latentHeat
      _RL tmpFac

#ifdef SHI_ALLOW_GAMMAFRICT
      _RL shiPr, shiSc, shiLo, recip_shiKarman, shiTwoThirds
      _RL gammaTmoleT, gammaTmoleS, gammaTurb, gammaTurbConst
      _RL ustar, ustarSq, etastar
      _RL u_tmp, v_tmp
      PARAMETER ( shiTwoThirds = 0.66666666666666666666666666667D0 )
#ifdef ALLOW_DIAGNOSTICS
      _RL uStarDiag(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif /* ALLOW_DIAGNOSTICS */
#endif /*SHI_ALLOW_GAMMAFRICT */

#ifndef ALLOW_OPENAD
      _RL SW_TEMP
      EXTERNAL SW_TEMP
#endif

#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
      _RL xx_shifwflx_loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SHI_ALLOW_GAMMAFRICT
      INTEGER ikey
# endif
#endif
CEOP
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef SHI_ALLOW_GAMMAFRICT
#ifdef ALLOW_AUTODIFF
C     re-initialize here again, curtesy to TAF
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          shiTransCoeffT(i,j,bi,bj) = SHELFICEheatTransCoeff
          shiTransCoeffS(i,j,bi,bj) = SHELFICEsaltTransCoeff
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF */
      IF ( SHELFICEuseGammaFrict ) THEN
C     Implement friction velocity-dependent transfer coefficient
C     of Holland and Jenkins, JPO, 1999
       recip_shiKarman= 1. _d 0 / 0.4 _d 0
       shiLo = 0. _d 0
       shiPr = shiPrandtl**shiTwoThirds
       shiSc = shiSchmidt**shiTwoThirds
cph      shiPr = (viscArNr(1)/diffKrNrT(1))**shiTwoThirds
cph      shiSc = (viscArNr(1)/diffKrNrS(1))**shiTwoThirds
       gammaTmoleT = 12.5 _d 0 * shiPr - 6. _d 0
       gammaTmoleS = 12.5 _d 0 * shiSc - 6. _d 0
C     instead of etastar = sqrt(1+zetaN*ustar./(f*Lo*Rc))
       etastar = 1. _d 0
       gammaTurbConst  = 1. _d 0 / (2. _d 0 * shiZetaN*etastar)
     &      - recip_shiKarman
      ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */

      recip_latentHeat = 0. _d 0
      IF ( SHELFICElatentHeat .NE. 0. _d 0 )
     &     recip_latentHeat = 1. _d 0/SHELFICElatentHeat
C     are we doing the conservative form of Jenkins et al. (2001)?
      recip_Cp = 1. _d 0 / HeatCapacity_Cp
      cFac = 0. _d 0
      IF ( SHELFICEconserve ) cFac = 1. _d 0
C     with "real fresh water flux" (affecting ETAN), there is more to modify
      rFac = 1. _d 0
      IF ( SHELFICEconserve .AND. useRealFreshWaterFlux ) rFac = 0. _d 0
      rFWinBL = 0. _d 0
      IF ( SHI_withBL_realFWflux ) rFWinBL = 1. _d 0
C     heat flux into the ice shelf, default is diffusive flux
C     (Holland and Jenkins, 1999, eq.21)
      dFac = 0. _d 0
      IF ( SHELFICEadvDiffHeatFlux ) dFac = 1. _d 0
      fwflxFac = 0. _d 0
C     linear dependence of freezing point on salinity
      a0 = -0.0575   _d  0
      a1 =  0.0      _d -0
      a2 =  0.0      _d -0
      c0 =  0.0901   _d  0
      b0 =  -7.61    _d -4
#ifdef ALLOW_ISOMIP_TD
      IF ( useISOMIPTD ) THEN
C     non-linear dependence of freezing point on salinity
       a0 = -0.0575   _d  0
       a1 = 1.710523  _d -3
       a2 = -2.154996 _d -4
       b0 = -7.53     _d -4
       c0 = 0. _d 0
      ENDIF
      convertFW2SaltLoc = convertFW2Salt
C     hardcoding this value here is OK because it only applies to ISOMIP
C     where this value is part of the protocol
      IF ( convertFW2SaltLoc .EQ. -1. ) convertFW2SaltLoc = 33.4 _d 0
#endif /* ALLOW_ISOMIP_TD */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy,sNy+OLy
         DO i = 1-OLx,sNx+OLx
          shelfIceHeatFlux      (i,j,bi,bj) = 0. _d 0
          shelfIceFreshWaterFlux(i,j,bi,bj) = 0. _d 0
          shelficeForcingT      (i,j,bi,bj) = 0. _d 0
          shelficeForcingS      (i,j,bi,bj) = 0. _d 0
#if (defined SHI_ALLOW_GAMMAFRICT && defined ALLOW_DIAGNOSTICS)
          uStarDiag             (i,j,bi,bj) = 0. _d 0
#endif /* SHI_ALLOW_GAMMAFRICT and ALLOW_DIAGNOSTICS */
#ifdef ALLOW_GENTIM2D_CONTROL
          xx_shifwflx_loc       (i,j,bi,bj) = 0. _d 0
#endif /* ALLOW_GENTIM2D_CONTROL */
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#if (defined ALLOW_CTRL && defined ALLOW_GENTIM2D_CONTROL)
      IF ( useCTRL ) THEN
       DO iarr = 1, maxCtrlTim2D
        IF (xx_gentim2d_file(iarr)(1:11).EQ.'xx_shifwflx') THEN
         DO bj = myByLo(myThid),myByHi(myThid)
          DO bi = myBxLo(myThid),myBxHi(myThid)
           DO j = 1,sNy
            DO i = 1,sNx
             xx_shifwflx_loc(i,j,bi,bj)=xx_gentim2d(i,j,bi,bj,iarr)
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDIF
#endif

#ifdef ALLOW_SHELFICE_REMESHING
      IF ( SHI_update_kTopC ) THEN
C--   Deal with ice-shelf edge advance or retreat when allowing ice-shelf
C     mass to change. Since current ice-shelf representation is missing
C     a fractional ice-shelf cover representation, this turns into making
C     or removing a "thin ice-shelf" grid-cell. Here we update "kTopC"
C     as ice-shelf mass appears or vanishes in any surface grid-cell.
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
           DO i = 1-OLx,sNx+OLx
             IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &            shelficeMass(i,j,bi,bj).GT.zeroRL ) THEN
               kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
             ELSE
               kTopC(i,j,bi,bj) = 0
             ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE kTopC        = comlev1, key=ikey_dynamics
# endif
#endif /* ALLOW_SHELFICE_REMESHING */
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE shelficeMass = comlev1, key=ikey_dynamics, kind=isbyte
#endif

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SHI_ALLOW_GAMMAFRICT
        ikey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
# endif /* SHI_ALLOW_GAMMAFRICT */
#endif /* ALLOW_AUTODIFF_TAMC */

C--   make local copies of temperature, salinity and depth (pressure in deci-bar)
C--   underneath the ice
        DO j = 1, sNy
         DO i = 1, sNx
          k         = MAX(1,kTopC(i,j,bi,bj))
C-    original (inaccurate) pLoc expression below:
C     assumes rhoConst*gravity*SItodBar=1 + missing both "shelficeLoadAnomaly"
C      static contribution and etaN dynamical contribution:
c         pLoc(i,j) = ABS(R_shelfIce(i,j,bi,bj))
C-    new and accurate pLoc expression (just the weight of the ice):
          pLoc(i,j) = shelficeMass(i,j,bi,bj)*gravity*SItodBar
          tLoc(i,j) = theta(i,j,k,bi,bj)
          sLoc(i,j) = MAX(salt(i,j,k,bi,bj), zeroRL)
          velSq(i,j)= 0.
         ENDDO
        ENDDO
        DO j = 1, sNy+1
         DO i = 1, sNx+1
          uLoc(i,j) = 0.
          vLoc(i,j) = 0.
         ENDDO
        ENDDO
#ifdef SHI_ALLOW_GAMMAFRICT
        IF ( SHELFICEuseGammaFrict .AND. SHELFICE_oldCalcUStar ) THEN
C-    Original averaging expression for uStar:
         DO j = 1, sNy
          DO i = 1, sNx
           k = MAX(1,kTopC(i,j,bi,bj))
           uLoc(i,j) = recip_hFacC(i,j,k,bi,bj) * halfRL *
     &         ( uVel(i,  j,k,bi,bj) * _hFacW(i,  j,k,bi,bj)
     &         + uVel(i+1,j,k,bi,bj) * _hFacW(i+1,j,k,bi,bj) )
           vLoc(i,j) = recip_hFacC(i,j,k,bi,bj) * halfRL *
     &         ( vVel(i,j,  K,bi,bj) * _hFacS(i,j,  K,bi,bj)
     &         + vVel(i,j+1,k,bi,bj) * _hFacS(i,j+1,k,bi,bj) )
           velSq(i,j) = uLoc(i,j)*uLoc(i,j)+vLoc(i,j)*vLoc(i,j)
          ENDDO
         ENDDO
        ELSEIF ( SHELFICEuseGammaFrict ) THEN
C-    New (more accurate) averaging expression for uStar:
         DO j = 1, sNy
          DO i = 1, sNx
           uLoc(i,j) = 0.
           vLoc(i,j) = 0.
           velSq(i,j) = 0.
           k = MAX(1,kTopC(i,j,bi,bj))
           tmpFac = _hFacW(i,  j,k,bi,bj) + _hFacW(i+1,j,k,bi,bj)
           IF ( tmpFac.GT.0. _d 0 )
     &     velSq(i,j) = (
     &     uVel( i, j,k,bi,bj)*uVel( i, j,k,bi,bj)*_hFacW( i, j,k,bi,bj)
     &   + uVel(i+1,j,k,bi,bj)*uVel(i+1,j,k,bi,bj)*_hFacW(i+1,j,k,bi,bj)
     &                  )/tmpFac
           tmpFac = _hFacS(i,j,  K,bi,bj) + _hFacS(i,j+1,k,bi,bj)
           IF ( tmpFac.GT.0. _d 0 )
     &     velSq(i,j) = velSq(i,j) + (
     &     vVel(i, j, K,bi,bj)*vVel(i, j, K,bi,bj)*_hFacS(i, j, K,bi,bj)
     &   + vVel(i,j+1,k,bi,bj)*vVel(i,j+1,k,bi,bj)*_hFacS(i,j+1,k,bi,bj)
     &                               )/tmpFac
          ENDDO
         ENDDO
        ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */

        IF ( SHELFICEBoundaryLayer ) THEN
C--   average over boundary layer width
         DO j = 1, sNy
          DO i = 1, sNx
           k   = kTopC(i,j,bi,bj)
           IF ( k .NE. 0 .AND. k .LT. Nr ) THEN
C           This local variable assignment/reassignment is simply to
C           avoid TAF recomputations... Oh the games we play...
            tTMP   = tLoc (i,j)
            sTMP   = sLoc (i,j)
            uTMP   = uLoc (i,j)
            vTMP   = vLoc (i,j)
            vSqTMP = velSq(i,j)

            kp1 = MIN(Nr,k+1)
C--   overlap into lower cell
            drKp1 = drF(k)*( 1. _d 0 - _hFacC(i,j,k,bi,bj) )
C--   lower cell may not be as thick as required
            drKp1 = MIN( drKp1, drF(kp1) * _hFacC(i,j,kp1,bi,bj) )
            drKp1 = MAX( drKp1, 0. _d 0 )
            recip_drLoc = 1. _d 0 /
     &           ( drF(k)*_hFacC(i,j,k,bi,bj) + drKp1 )
            tTMP = ( tTMP * drF(k)*_hFacC(i,j,k,bi,bj)
     &           + theta(i,j,kp1,bi,bj) *drKp1 )
     &           * recip_drLoc
            sTMP = ( sTMP * drF(k)*_hFacC(i,j,k,bi,bj)
     &           + MAX(salt(i,j,kp1,bi,bj), zeroRL) * drKp1 )
     &           * recip_drLoc
            uTMP = ( uTMP * drF(k)*_hFacC(i,j,k,bi,bj)
     &           + drKp1 * recip_hFacC(i,j,kp1,bi,bj) * halfRL *
     &           ( uVel(i,  j,kp1,bi,bj) * _hFacW(i,  j,kp1,bi,bj)
     &           + uVel(i+1,j,kp1,bi,bj) * _hFacW(i+1,j,kp1,bi,bj) )
     &           ) * recip_drLoc
            vTMP = ( vTMP * drF(k)*_hFacC(i,j,k,bi,bj)
     &           + drKp1 * recip_hFacC(i,j,kp1,bi,bj) * halfRL *
     &           ( vVel(i,j,  kp1,bi,bj) * _hFacS(i,j,  kp1,bi,bj)
     &           + vVel(i,j+1,kp1,bi,bj) * _hFacS(i,j+1,kp1,bi,bj) )
     &           ) * recip_drLoc
            vSqTMP = uTMP*uTMP + vTMP*vTMP

            tLoc (i,j) = tTMP
            sLoc (i,j) = sTMP
            uLoc (i,j) = uTMP
            vLoc (i,j) = vTMP
            velSq(i,j) = vSqTMP
           ENDIF
          ENDDO
         ENDDO
        ENDIF

#ifdef SHI_ALLOW_GAMMAFRICT
        IF ( SHI_withBL_uStarTopDz ) THEN
C--  TOPDR averages U/V over boundary layer at U/V points, then averages
C    (as opposed to averaging horizontally then vertically)
C    Average at u- and v- points over deltaR. use later to override uLoc/vLoc
         DO j = 1, sNy+1
          DO i = 1, sNx+1
           k = kSurfW(i,j,bi,bj)
           IF (k.LT.Nr) THEN
            kp1 = k+1
            drKp1 = drF(k)*( 1. _d 0 - _hFacW(i,j,k,bi,bj) )
            drKp1 = MIN( drKp1, drF(kp1) * _hFacW(i,j,kp1,bi,bj) )
            drKp1 = MAX( drKp1, 0. _d 0 )
            recip_drLoc = 1. _d 0
     &                  / ( drF(k)*_hFacW(i,j,k,bi,bj) + drKp1 )
            uLoc(i,j) = ( drF(k)*_hFacW(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)
     &                  + drKp1*uVel(i,j,kp1,bi,bj)
     &                  )*recip_drLoc
c           u_topDr(i,j) =
c    &           ( uVel(i,j,k,bi,bj)*drF(k)*_hFacW(i,j,k,bi,bj)
c    &           + uVel(i,j,kp1,bi,bj)*drKp1
c    &           )*recip_drLoc
           ELSEIF (K.EQ.Nr) THEN
            uLoc(i,j) = uVel(i,j,k,bi,bj)
           ELSE
            uLoc(i,j) = 0. _d 0
           ENDIF

           k = kSurfS(i,j,bi,bj)
           IF (k.LT.Nr) THEN
            kp1 = k+1
            drKp1 = drF(k)*( 1. _d 0 - _hFacS(i,j,k,bi,bj) )
            drKp1 = MIN( drKp1, drF(kp1) * _hFacS(i,j,kp1,bi,bj) )
            drKp1 = MAX( drKp1, 0. _d 0 )
            recip_drLoc = 1. _d 0
     &                  / ( drF(k)*_hFacS(i,j,k,bi,bj) + drKp1 )
            vLoc(i,j) = ( drF(k)*_hFacS(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)
     &                  + drKp1*vVel(i,j,kp1,bi,bj)
     &                  )*recip_drLoc
c           v_topDr(i,j) =
c    &           ( vVel(i,j,k,bi,bj)*drF(k)*_hFacS(i,j,k,bi,bj)
c    &           + vVel(i,j,kp1,bi,bj)*drKp1
c    &           )*recip_drLoc
           ELSEIF (K.EQ.Nr) THEN
            vLoc(i,j) = vVel(i,j,k,bi,bj)
           ELSE
            vLoc(i,j) = 0. _d 0
           ENDIF

          ENDDO
         ENDDO
         DO j = 1, sNy
          DO i = 1, sNx
           u_tmp = halfRL*( uLoc(i,j) + uLoc(i+1,j) )
           v_tmp = halfRL*( vLoc(i,j) + vLoc(i,j+1) )
           velSq(i,j) = u_tmp*u_tmp + v_tmp*v_tmp
          ENDDO
         ENDDO
        ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */

C--   turn potential temperature into in-situ temperature relative
C--   to the surface
        DO j = 1, sNy
         DO i = 1, sNx
#ifdef ALLOW_OPENAD
          CALL SW_TEMP(sLoc(i,j),tLoc(i,j),pLoc(i,j),zeroRL,tLoc(i,j))
#else
          tLoc(i,j) = SW_TEMP(sLoc(i,j),tLoc(i,j),pLoc(i,j),zeroRL)
#endif
         ENDDO
        ENDDO

#ifdef SHI_ALLOW_GAMMAFRICT
        IF ( SHELFICEuseGammaFrict ) THEN
         DO j = 1, sNy
          DO i = 1, sNx
           k = kTopC(i,j,bi,bj)
           IF ( k .NE. 0 .AND. pLoc(i,j) .GT. 0. _d 0 ) THEN
            ustarSq = shiCdragfld(i,j,bi,bj) * MAX( 1.D-6, velSq(i,j) )
            ustar   = SQRT(ustarSq)
#ifdef ALLOW_DIAGNOSTICS
            uStarDiag(i,j,bi,bj) = ustar
#endif /* ALLOW_DIAGNOSTICS */
C     instead of etastar = sqrt(1+zetaN*ustar./(f*Lo*Rc))
C           etastar = 1. _d 0
C           gammaTurbConst  = 1. _d 0 / (2. _d 0 * shiZetaN*etastar)
C    &           - recip_shiKarman
            IF ( fCori(i,j,bi,bj) .NE. 0. _d 0 ) THEN
             gammaTurb = LOG( ustarSq * shiZetaN * etastar**2
     &            / ABS(fCori(i,j,bi,bj) * 5.0 _d 0 * shiKinVisc))
     &            * recip_shiKarman
     &            + gammaTurbConst
C     Do we need to catch the unlikely case of very small ustar
C     that can lead to negative gammaTurb?
C            gammaTurb = MAX(0.D0, gammaTurb)
            ELSE
             gammaTurb = gammaTurbConst
            ENDIF
            shiTransCoeffT(i,j,bi,bj) = MAX( zeroRL,
     &           ustar/(gammaTurb + gammaTmoleT) )
            shiTransCoeffS(i,j,bi,bj) = MAX( zeroRL,
     &           ustar/(gammaTurb + gammaTmoleS) )
           ENDIF
          ENDDO
         ENDDO
        ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */

#ifdef ALLOW_AUTODIFF_TAMC
# ifdef SHI_ALLOW_GAMMAFRICT
CADJ STORE shiTransCoeffS(:,:,bi,bj) = comlev1_bibj,
CADJ &     key=ikey, byte=isbyte
CADJ STORE shiTransCoeffT(:,:,bi,bj) = comlev1_bibj,
CADJ &     key=ikey, byte=isbyte
# endif /* SHI_ALLOW_GAMMAFRICT */
#endif /* ALLOW_AUTODIFF_TAMC */
#ifdef ALLOW_ISOMIP_TD
        IF ( useISOMIPTD ) THEN
         DO j = 1, sNy
          DO i = 1, sNx
           k = kTopC(i,j,bi,bj)
           IF ( k .NE. 0 .AND. pLoc(i,j) .GT. 0. _d 0 ) THEN
C--   Calculate freezing temperature as a function of salinity and pressure
            thetaFreeze =
     &           sLoc(i,j) * ( a0 + a1*sqrt(sLoc(i,j)) + a2*sLoc(i,j) )
     &           + b0*pLoc(i,j) + c0
C--   Calculate the upward heat and  fresh water fluxes
            shelfIceHeatFlux(i,j,bi,bj) = maskC(i,j,k,bi,bj)
     &           * shiTransCoeffT(i,j,bi,bj)
     &           * ( tLoc(i,j) - thetaFreeze )
     &           * HeatCapacity_Cp*rUnit2mass
#ifdef ALLOW_GENTIM2D_CONTROL
     &           - xx_shifwflx_loc(i,j,bi,bj)*SHELFICElatentHeat
#endif /*  ALLOW_GENTIM2D_CONTROL */
C     upward heat flux into the shelf-ice implies basal melting,
C     thus a downward (negative upward) fresh water flux (as a mass flux),
C     and vice versa
            shelfIceFreshWaterFlux(i,j,bi,bj) =
     &           - shelfIceHeatFlux(i,j,bi,bj)
     &           *recip_latentHeat
C--   compute surface tendencies
            shelficeForcingT(i,j,bi,bj) =
     &           - shelfIceHeatFlux(i,j,bi,bj)
     &           *recip_Cp*mass2rUnit
     &           - cFac * shelfIceFreshWaterFlux(i,j,bi,bj)*mass2rUnit
     &           * ( thetaFreeze - tLoc(i,j) )
            shelficeForcingS(i,j,bi,bj) =
     &           shelfIceFreshWaterFlux(i,j,bi,bj) * mass2rUnit
     &           * ( cFac*sLoc(i,j) + (1. _d 0-cFac)*convertFW2SaltLoc )
C--   stress at the ice/water interface is computed in separate
C     routines that are called from mom_fluxform/mom_vecinv
           ELSE
            shelfIceHeatFlux      (i,j,bi,bj) = 0. _d 0
            shelfIceFreshWaterFlux(i,j,bi,bj) = 0. _d 0
            shelficeForcingT      (i,j,bi,bj) = 0. _d 0
            shelficeForcingS      (i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ELSE
#else
        IF ( .TRUE. ) THEN
#endif /* ALLOW_ISOMIP_TD */
C     use BRIOS thermodynamics, following Hellmers PhD thesis:
C     Hellmer, H., 1989, A two-dimensional model for the thermohaline
C     circulation under an ice shelf, Reports on Polar Research, No. 60
C     (in German).

         DO j = 1, sNy
          DO i = 1, sNx
           k    = kTopC(i,j,bi,bj)
           IF ( k .NE. 0 .AND. pLoc(i,j) .GT. 0. _d 0 ) THEN
C     heat flux into the ice shelf, default is diffusive flux
C     (Holland and Jenkins, 1999, eq.21)
            thetaFreeze = a0*sLoc(i,j)+c0+b0*pLoc(i,j)
            fwflxFac    = 0. _d 0
            IF ( tLoc(i,j) .GT. thetaFreeze ) fwflxFac = dFac
C     a few abbreviations
            eps1 = rUnit2mass*HeatCapacity_Cp
     &           *shiTransCoeffT(i,j,bi,bj)
            eps2 = rUnit2mass*SHELFICElatentHeat
     &           *shiTransCoeffS(i,j,bi,bj)

C     solve quadratic equation for salinity at shelfice-ocean interface
C     note: this part of the code is not very intuitive as it involves
C     many arbitrary abbreviations that were introduced to derive the
C     correct form of the quadratic equation for salinity. The abbreviations
C     are explained in the documentation.
C
C     eps3a was introduced as a constant variant of eps3 to avoid AD of
C     code of type (pLoc-const)/pLoc
            eps3a = rhoShelfIce*SHELFICEheatCapacity_Cp
     &           * SHELFICEkappa *  ( 1. _d 0 - dFac )
            eps3 = eps3a/pLoc(i,j)
            eps4 = b0*pLoc(i,j) + c0
            eps6 = eps4 - tLoc(i,j)
            eps7 = eps4 - SHELFICEthetaSurface
            eps8 = rUnit2mass*SHELFICEheatCapacity_Cp
     &           *shiTransCoeffS(i,j,bi,bj) * fwflxFac
            epsq = eps1*eps6 + eps3*eps7
C     coefficients of the quadratic equations for saltFreeze
            aqe = a0*(eps1+eps3-eps8)
            recip_aqe = 0. _d 0
            IF ( aqe .NE. 0. _d 0 ) recip_aqe = 0.5 _d 0/aqe
            bqe = epsq - eps2 + eps8*( a0*sLoc(i,j) - eps7 )
     &           - a0*(eps1+eps3)*SHELFICEsalinity
            cqe = ( eps2 + eps8*eps7 )*sLoc(i,j) - epsq*SHELFICEsalinity
            discrim = bqe*bqe - 4. _d 0*aqe*cqe
#undef ALLOW_SHELFICE_DEBUG
#ifdef ALLOW_SHELFICE_DEBUG
            IF ( discrim .LT. 0. _d 0 ) THEN
             print *, 'ml-shelfice: discrim = ', discrim,aqe,bqe,cqe
             print *, 'ml-shelfice: pLoc    = ', pLoc(i,j)
             print *, 'ml-shelfice: tLoc    = ', tLoc(i,j)
             print *, 'ml-shelfice: sLoc    = ', sLoc(i,j)
             print *, 'ml-shelfice: tsurface= ',
     &            SHELFICEthetaSurface
             print *, 'ml-shelfice: eps1    = ', eps1
             print *, 'ml-shelfice: eps2    = ', eps2
             print *, 'ml-shelfice: eps3    = ', eps3
             print *, 'ml-shelfice: eps4    = ', eps4
             print *, 'ml-shelfice: eps6    = ', eps6
             print *, 'ml-shelfice: eps7    = ', eps7
             print *, 'ml-shelfice: eps8    = ', eps8
             print *, 'ml-shelfice: rU2mass = ', rUnit2mass
             print *, 'ml-shelfice: rhoIce  = ', rhoShelfIce
             print *, 'ml-shelfice: cFac    = ', cFac
             print *, 'ml-shelfice: Cp_W    = ', HeatCapacity_Cp
             print *, 'ml-shelfice: Cp_I    = ',
     &            SHELFICEHeatCapacity_Cp
             print *, 'ml-shelfice: gammaT  = ',
     &            SHELFICEheatTransCoeff
             print *, 'ml-shelfice: gammaS  = ',
     &            SHELFICEsaltTransCoeff
             print *, 'ml-shelfice: lat.heat= ',
     &            SHELFICElatentHeat
             STOP 'ABNORMAL END in S/R SHELFICE_THERMODYNAMICS'
            ENDIF
#endif /* ALLOW_SHELFICE_DEBUG */
            saltFreeze = (- bqe - SQRT(discrim))*recip_aqe
            IF ( saltFreeze .LT. 0. _d 0 )
     &           saltFreeze = (- bqe + SQRT(discrim))*recip_aqe
            thetaFreeze = a0*saltFreeze + eps4
#ifdef SHI_SALTBAL_FWFLX
C--   upward fresh water flux due to melting (in kg/m^2/s)
cph change to identical form
cph            freshWaterFlux = rUnit2mass
cph     &           * shiTransCoeffS(i,j,bi,bj)
cph     &           * ( saltFreeze - sLoc(i,j) ) / saltFreeze
            freshWaterFlux = rUnit2mass * shiTransCoeffS(i,j,bi,bj)
C     Catch unlikely case of saltFreeze = 0 (often when sLoc=0)
            IF ( saltFreeze .NE. 0. _d 0 )
     &           freshWaterFlux = freshWaterFlux
     &           * ( 1. _d 0 - sLoc(i,j) / saltFreeze )
#else /* ndef SHI_SALTBAL_FWFLX */
C     This formulation of fwflux, derived from the heat balance equation
C     instead of the salt balance equation, can handle the case when the
C     salinity of the ocean, boundary layer, and ice are identical.
C     Default thermodynamics specify a linear temperature gradient
C     through the ice (Holland and Jenkins, 1999, Section 2).
C     Here, via eps3=0 and fwFlxFac, we also cover the case of a nonlinear
C     temperature gradient through the ice (Holland and Jenkins, 1999,
C     Section 3), but only for melting (Eq. 31 of Holland and Jenkins,
C     1999). fwFlxFac = 0 for freezing and 1 for melting conditions.
            freshWaterFlux = (
     &             eps1*( thetaFreeze - tLoc(i,j) )
     &           + eps3*( thetaFreeze - SHELFICEthetaSurface )
     &           ) / ( SHELFICElatentHeat
     &                + fwFlxFac * SHELFICEheatCapacity_Cp*
     &                  (thetaFreeze - SHELFICEthetaSurface) )
#endif /* ndef SHI_SALTBAL_FWFLX */

#ifdef ALLOW_GENTIM2D_CONTROL
            freshWaterFlux = freshWaterFlux + xx_shifwflx_loc(i,j,bi,bj)
#endif /* ALLOW_GENTIM2D_CONTROL */
C--   Calculate the upward heat and fresh water fluxes;
C--   MITgcm sign conventions: downward (negative) fresh water flux
C--   implies melting and due to upward (positive) heat flux
            shelfIceHeatFlux(i,j,bi,bj) =
     &           ( eps3
     &           - freshWaterFlux*SHELFICEheatCapacity_Cp*fwflxFac )
     &           * ( thetaFreeze - SHELFICEthetaSurface )
     &           -  cFac*freshWaterFlux*( SHELFICElatentHeat
     &             - HeatCapacity_Cp*( thetaFreeze - rFac*tLoc(i,j) ) )
            shelfIceFreshWaterFlux(i,j,bi,bj) = freshWaterFlux
C--   compute surface tendencies
C--   DNG: correction to use cell value for flux rather than BL values
C--        in order to conserve salt and temp even with real FW Flux
            shelficeForcingT(i,j,bi,bj) =
     &           ( shiTransCoeffT(i,j,bi,bj)
     &           - cFac*freshWaterFlux*mass2rUnit
     &           )*( thetaFreeze - tLoc(i,j) )
     &           - rFWinBL*freshWaterFlux*mass2rUnit
     &            *( tLoc(i,j) - theta(i,j,k,bi,bj) )
            shelficeForcingS(i,j,bi,bj) =
     &           ( shiTransCoeffS(i,j,bi,bj)
     &           - cFac*freshWaterFlux*mass2rUnit
     &           )*( saltFreeze - sLoc(i,j) )
     &           - rFWinBL*freshWaterFlux*mass2rUnit
     &            *( sLoc(i,j) - salt(i,j,k,bi,bj) )
           ELSE
            shelfIceHeatFlux      (i,j,bi,bj) = 0. _d 0
            shelfIceFreshWaterFlux(i,j,bi,bj) = 0. _d 0
            shelficeForcingT      (i,j,bi,bj) = 0. _d 0
            shelficeForcingS      (i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
C     endif (not) useISOMIPTD
        ENDIF
C     end bi,bj loops
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF_TAMC
C     Avoid recomputing most of the routine body in the AD-code
CADJ STORE shelfIceFreshWaterFlux
CADJ &     = comlev1, key=ikey_dynamics, kind=isbyte
#endif

      IF (SHELFICEMassStepping) THEN
       CALL SHELFICE_STEP_ICEMASS( myTime, myIter, myThid )
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL_RS(shelfIceFreshWaterFlux,'SHIfwFlx',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL_RS(shelfIceHeatFlux,      'SHIhtFlx',
     &      0,1,0,1,1,myThid)
C     SHIForcT (Ice shelf forcing for theta [W/m2], >0 increases theta)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(shelficeForcingT,tmpFac,1,
     &      'SHIForcT',0,1,0,1,1,myThid)
C     SHIForcS (Ice shelf forcing for salt [g/m2/s], >0 increases salt)
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL(shelficeForcingS,tmpFac,1,
     &      'SHIForcS',0,1,0,1,1,myThid)
C     Transfer coefficients
       CALL DIAGNOSTICS_FILL(shiTransCoeffT,'SHIgammT',
     &      0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(shiTransCoeffS,'SHIgammS',
     &      0,1,0,1,1,myThid)

C     Friction velocity
#ifdef SHI_ALLOW_GAMMAFRICT
       IF ( SHELFICEuseGammaFrict ) THEN
        CALL DIAGNOSTICS_FILL(uStarDiag,'SHIuStar',0,1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(shiCDragFld,'SHICDrag',
     &      0,1,0,1,1,myThid)
       ENDIF
#endif /* SHI_ALLOW_GAMMAFRICT */
#ifdef ALLOW_SHELFICE_REMESHING
       CALL DIAGNOSTICS_FILL_RS( R_shelfIce, 'SHIRshel',
     &                           0, 1, 0, 1, 1, myThid )
#endif
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_SHELFICE */
      RETURN
      END
