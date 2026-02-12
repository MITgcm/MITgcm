#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_KPP
# include "KPP_OPTIONS.h"
#endif
#if ( defined GM_READ_K3D_REDI || defined GM_VISBECK_VARIABLE_K \
      || defined ALLOW_GM_LEITH_QG || defined GM_BATES_K3D )
# define GM_USE_K3D_REDI
#endif
C no GM_GEOM_VARIABLE here since it is at a different location
#if ( defined GM_READ_K3D_GM   || defined GM_VISBECK_VARIABLE_K \
      || defined ALLOW_GM_LEITH_QG || defined GM_BATES_K3D )
# define GM_USE_K3D_GM
#endif

CBOP
C     !ROUTINE: GMREDI_CALC_TENSOR
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_TENSOR(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_TENSOR
C     | o Calculate tensor elements for GM/Redi tensor.
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"
#ifdef ALLOW_CTRL
# include "CTRL_FIELDS.h"
#endif
#ifdef ALLOW_KPP
# include "KPP.h"
#endif

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif /* ALLOW_AUTODIFF_TAMC */

C     !INPUT/OUTPUT PARAMETERS:
C     bi, bj    :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: My Thread Id. number
C
      INTEGER iMin,iMax,jMin,jMax
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
C     maskFk    :: 2-D mask for vertical interface k (between level k-1 & k)
      INTEGER i,j,k
      _RS maskFk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL SlopeSqr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL taperFct(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ldd97_LrhoC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ldd97_LrhoW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ldd97_LrhoS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Cspd, LrhoInf, LrhoSup, fCoriLoc, rDepth
      _RL Kgm_tmp, Kredi_tmp, isopycK, bolus_K

      INTEGER kLow_W (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kLow_S (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locMixLayer(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL baseSlope  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hTransLay  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recipLambda(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER km1
#if ( defined GM_NON_UNITY_DIAGONAL || defined GM_EXTRA_DIAGONAL )
      INTEGER kp1
      _RL maskp1
#endif
#ifdef GM_USE_K3D_REDI
      _RL locK3dRedi(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#endif
#ifdef GM_USE_K3D_GM
      _RL locK3dGM(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#else
      _RL locK3dGM(1)
#endif

#ifdef GM_VISBECK_VARIABLE_K
      INTEGER ks
      _RL dSigmaH, dSigmaR, Sloc, rTop
c     _RL M2loc
      _RL recipMaxSlope
      _RL deltaH, integrDepth
      _RL N2loc, SNloc
#endif /* GM_VISBECK_VARIABLE_K */

#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  doDiagRediFlx
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
# if ( defined GM_NON_UNITY_DIAGONAL || defined GM_EXTRA_DIAGONAL )
      _RL dTdz
      _RL tmp1k(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# endif
#endif /* ALLOW_DIAGNOSTICS */
#ifdef ALLOW_AUTODIFF_TAMC
C     tkey :: tape key (depends on tiles)
C     kkey :: tape key (depends on levels and tiles)
      INTEGER tkey, kkey
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_AUTODIFF_TAMC
      tkey = bi + (bj-1)*nSx + (ikey_dynamics-1)*nSx*nSy
#endif /* ALLOW_AUTODIFF_TAMC */

#ifdef ALLOW_DIAGNOSTICS
      doDiagRediFlx = .FALSE.
      IF ( useDiagnostics ) THEN
        doDiagRediFlx = DIAGNOSTICS_IS_ON('GM_KuzTz', myThid )
        doDiagRediFlx = doDiagRediFlx .OR.
     &                  DIAGNOSTICS_IS_ON('GM_KvzTz', myThid )
      ENDIF
#endif

#ifdef GM_VISBECK_VARIABLE_K
      recipMaxSlope = 0. _d 0
      IF ( GM_Visbeck_maxSlope.GT.0. _d 0 ) THEN
        recipMaxSlope = 1. _d 0 / GM_Visbeck_maxSlope
      ENDIF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        VisbeckK(i,j,bi,bj) = 0. _d 0
       ENDDO
      ENDDO
#endif

C--   set ldd97_Lrho (for tapering scheme ldd97):
      IF ( GM_taper_scheme.EQ.'ldd97' .OR.
     &     GM_taper_scheme.EQ.'fm07' ) THEN
       Cspd = 2. _d 0
       LrhoInf = 15. _d 3
       LrhoSup = 100. _d 3
C-     Tracer point location (center):
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (fCori(i,j,bi,bj).NE.0.) THEN
           ldd97_LrhoC(i,j) = Cspd/ABS(fCori(i,j,bi,bj))
         ELSE
           ldd97_LrhoC(i,j) = LrhoSup
         ENDIF
         ldd97_LrhoC(i,j) = MAX(LrhoInf,MIN(ldd97_LrhoC(i,j),LrhoSup))
        ENDDO
       ENDDO
C-     U point location (West):
       DO j=1-OLy,sNy+OLy
        kLow_W(1-OLx,j) = 0
        ldd97_LrhoW(1-OLx,j) = LrhoSup
        DO i=1-OLx+1,sNx+OLx
         kLow_W(i,j) = MIN(kLowC(i-1,j,bi,bj),kLowC(i,j,bi,bj))
         fCoriLoc = op5*(fCori(i-1,j,bi,bj)+fCori(i,j,bi,bj))
         IF ( fCoriLoc.NE.zeroRL ) THEN
           ldd97_LrhoW(i,j) = Cspd/ABS(fCoriLoc)
         ELSE
           ldd97_LrhoW(i,j) = LrhoSup
         ENDIF
         ldd97_LrhoW(i,j) = MAX(LrhoInf,MIN(ldd97_LrhoW(i,j),LrhoSup))
        ENDDO
       ENDDO
C-     V point location (South):
       DO i=1-OLx+1,sNx+OLx
         kLow_S(i,1-OLy) = 0
         ldd97_LrhoS(i,1-OLy) = LrhoSup
       ENDDO
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx,sNx+OLx
         kLow_S(i,j) = MIN(kLowC(i,j-1,bi,bj),kLowC(i,j,bi,bj))
         fCoriLoc = op5*(fCori(i,j-1,bi,bj)+fCori(i,j,bi,bj))
         IF ( fCoriLoc.NE.zeroRL ) THEN
           ldd97_LrhoS(i,j) = Cspd/ABS(fCoriLoc)
         ELSE
           ldd97_LrhoS(i,j) = LrhoSup
         ENDIF
         ldd97_LrhoS(i,j) = MAX(LrhoInf,MIN(ldd97_LrhoS(i,j),LrhoSup))
        ENDDO
       ENDDO
      ELSE
C-    Just initialize to zero (not use anyway)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          ldd97_LrhoC(i,j) = 0. _d 0
          ldd97_LrhoW(i,j) = 0. _d 0
          ldd97_LrhoS(i,j) = 0. _d 0
        ENDDO
       ENDDO
      ENDIF

#ifdef GM_BOLUS_ADVEC
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         GM_PsiX(i,j,k,bi,bj)  = 0. _d 0
         GM_PsiY(i,j,k,bi,bj)  = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
#endif /* GM_BOLUS_ADVEC */
#ifdef GM_USE_K3D_REDI
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locK3dRedi(i,j,k) =
# ifdef GM_READ_K3D_REDI
     &       GM_inpK3dRedi(i,j,k,bi,bj)
# else
     &       GM_isopycK*GM_isoFac1d(k)*GM_isoFac2d(i,j,bi,bj)
# endif
        ENDDO
       ENDDO
      ENDDO
#endif /* GM_USE_K3D_REDI */
#ifdef GM_USE_K3D_GM
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locK3dGM(i,j,k) =
# ifdef GM_READ_K3D_GM
     &       GM_inpK3dGM(i,j,k,bi,bj)
# else
     &       GM_background_K*GM_bolFac1d(k)*GM_bolFac2d(i,j,bi,bj)
# endif
        ENDDO
       ENDDO
      ENDDO
#else
      locK3dGM(1) = 0. _d 0
#endif /* GM_USE_K3D_GM */
#ifdef ALLOW_AUTODIFF
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         Kwx(i,j,k,bi,bj)  = 0. _d 0
         Kwy(i,j,k,bi,bj)  = 0. _d 0
         Kwz(i,j,k,bi,bj)  = 0. _d 0
         Kux(i,j,k,bi,bj)  = 0. _d 0
         Kvy(i,j,k,bi,bj)  = 0. _d 0
# ifdef GM_EXTRA_DIAGONAL
         Kuz(i,j,k,bi,bj)  = 0. _d 0
         Kvz(i,j,k,bi,bj)  = 0. _d 0
# endif
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AUTODIFF */

C--   Initialise Mixed Layer related array:
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         hTransLay(i,j) = R_low(i,j,bi,bj)
         baseSlope(i,j) =  0. _d 0
         recipLambda(i,j) = 0. _d 0
         locMixLayer(i,j) = 0. _d 0
       ENDDO
      ENDDO
#ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locMixLayer(i,j) = KPPhbl(i,j,bi,bj)
        ENDDO
       ENDDO
      ELSE
#else
      IF ( .TRUE. ) THEN
#endif
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locMixLayer(i,j) = hMixLayer(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef GM_BATES_K3D
      IF (GM_useBatesK3d) THEN
C     Calculate the 3D diffusivity as per Bates et al. (JPO, 2014)
        CALL TIMER_START('GMREDI_CALC_BATES_K [GMREDI_CALC_TENSOR]',
     &                    myThid)
        CALL GMREDI_CALC_BATES_K(
     I       iMin, iMax, jMin, jMax,
     I       sigmaX, sigmaY, sigmaR,
     I       bi, bj, myTime, myIter, myThid )
        CALL TIMER_STOP('GMREDI_CALC_BATES_K [GMREDI_CALC_TENSOR]',
     &                  myThid)
# ifndef GM_BATES_PASSIVE
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           locK3dRedi(i,j,k) = locK3dRedi(i,j,k)
     &          + GM_BatesK3d(i,j,k,bi,bj)*GM_isoFac_calcK
           locK3dGM(i,j,k)   = locK3dGM(i,j,k)
     &          + GM_BatesK3d(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
# endif
      ENDIF
#endif

#ifdef ALLOW_GM_LEITH_QG
# ifdef ALLOW_AUTODIFF
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          GM_LeithQG_K(i,j,k,bi,bj) = 0. _d 0
        ENDDO
       ENDDO
      ENDDO
# endif
      IF ( GM_useLeithQG ) THEN
C   Use Leith QG viscosity in GMRedi scheme
        CALL GMREDI_CALC_QGLEITH(
     O                GM_LeithQG_K(1-OLx,1-OLy,1,bi,bj),
     I                bi, bj, myTime, myIter, myThid )
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           locK3dRedi(i,j,k) = locK3dRedi(i,j,k)
     &          + GM_LeithQG_K(i,j,k,bi,bj)*GM_isoFac_calcK
           locK3dGM(i,j,k)   = locK3dGM(i,j,k)
     &          + GM_LeithQG_K(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_GM_LEITH_QG */

#ifdef GM_GEOM_VARIABLE_K
C     Calculate the kgm as per the GEOMETRIC formulation
C        (e.g. Mak et al, 2018; see also Marshall et al, 2012)
      IF ( GM_useGEOM ) THEN
        DO k=1,Nr
          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              GEOM_K3d(i,j,k,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
        CALL GMREDI_CALC_GEOM(
     I              sigmaX, sigmaY, sigmaR,
     I              bi, bj, myTime, myIter, myThid )
      ENDIF
#endif /* GM_GEOM_VARIABLE_K */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- 1rst loop on k : compute Tensor Coeff. at W points.

      DO k=Nr,2,-1

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
#ifdef ALLOW_AUTODIFF
         SlopeX(i,j)       = 0. _d 0
         SlopeY(i,j)       = 0. _d 0
         dSigmaDx(i,j)    = 0. _d 0
         dSigmaDy(i,j)     = 0. _d 0
         dSigmaDr(i,j)     = 0. _d 0
         SlopeSqr(i,j)     = 0. _d 0
         taperFct(i,j)     = 0. _d 0
#endif /* ALLOW_AUTODIFF */
         maskFk(i,j)       = maskC(i,j,k-1,bi,bj)*maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO

       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
C      Gradient of Sigma at rVel points
         dSigmaDx(i,j)=op25*( sigmaX(i+1,j,k-1)+sigmaX(i,j,k-1)
     &                       +sigmaX(i+1,j, k )+sigmaX(i,j, k )
     &                      )*maskFk(i,j)
         dSigmaDy(i,j)=op25*( sigmaY(i,j+1,k-1)+sigmaY(i,j,k-1)
     &                       +sigmaY(i,j+1, k )+sigmaY(i,j, k )
     &                      )*maskFk(i,j)
c        dSigmaDr(i,j)=sigmaR(i,j,k)
        ENDDO
       ENDDO

#ifdef GM_VISBECK_VARIABLE_K
# ifdef ALLOW_AUTODIFF_TAMC
CADJ INCOMPLETE dSigmaDr
# endif
       IF ( GM_Visbeck_alpha.GT.zeroRL .AND.
     &      -rC(k-1).LT.GM_Visbeck_depth ) THEN

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          dSigmaDr(i,j) = MAX( gravitySign*sigmaR(i,j,k), zeroRL )
         ENDDO
        ENDDO

C--     Depth average of f/sqrt(Ri) = M^2/N^2 * N
C       M^2 and N^2 are horizontal & vertical gradient of buoyancy.

C       Calculate terms for mean Richardson number which is used
C       in the "variable K" parameterisaton:
C       compute depth average from surface down to the bottom or
C       GM_Visbeck_depth, whatever is the shallower.

        rTop = rF(1)
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          IF ( maskFk(i,j).NE.zeroRS ) THEN
           ks = kSurfC(i,j,bi,bj)
C-      If dry-cell @ top (e.g., under ice-shelf), integrate over Visbeck_depth
C       as relative to top solid BC (as below) or stop integral if deeper (in
C       absolute sense) than Visbeck_depth (just comment "rTop =" line below):
           rTop = Ro_surf(i,j,bi,bj)
           integrDepth = rTop - rC( kLowC(i,j,bi,bj) )
C-      in 2 steps to avoid mix of RS & RL type in min fct. arguments
           integrDepth = MIN( integrDepth, GM_Visbeck_depth )
C-      to recover "old-visbeck" form with Visbeck_minDepth = Visbeck_depth
           integrDepth = MAX( integrDepth, GM_Visbeck_minDepth )
C       Distance between level center above and the integration depth
           deltaH = integrDepth - rTop + rC(k-1)
C       If negative then we are below the integration level
C       (cannot be the case with 2 conditions on maskC & -rC(k-1))
C       If positive we limit this to the distance from center above
           deltaH = MIN( deltaH, drC(k) )
C       Now we convert deltaH to a non-dimensional fraction
           deltaH = deltaH/( integrDepth - rTop + rC(ks) )

C--      compute: ( M^2 * S )^1/2   (= S*N since S=M^2/N^2 )
C        a 5 points average gives a more "homogeneous" formulation
C        (same stencil and same weights as for dSigmaH calculation)
           dSigmaR = ( dSigmaDr(i,j)*4. _d 0
     &               + dSigmaDr(i-1,j)
     &               + dSigmaDr(i+1,j)
     &               + dSigmaDr(i,j-1)
     &               + dSigmaDr(i,j+1)
     &               )/( 4. _d 0
     &                 + maskFk(i-1,j)
     &                 + maskFk(i+1,j)
     &                 + maskFk(i,j-1)
     &                 + maskFk(i,j+1)
     &                 )
           dSigmaH = dSigmaDx(i,j)*dSigmaDx(i,j)
     &             + dSigmaDy(i,j)*dSigmaDy(i,j)
           IF ( dSigmaH .GT. 0. _d 0 ) THEN
             dSigmaH = SQRT( dSigmaH )
# ifdef ALLOW_AUTODIFF_TAMC
C     This gets rid of a TAF-recomputation warning for dSigmaH that is
C     not associated with any actual recomputations. TAF is probably
C     just confused by the cascade of if-statements.
           ENDIF
           IF ( dSigmaH .GT. 0. _d 0 ) THEN
# endif
C-       compute slope, limited by GM_Visbeck_maxSlope:
             IF ( dSigmaR.GT.dSigmaH*recipMaxSlope ) THEN
              Sloc = dSigmaH / dSigmaR
             ELSE
              Sloc = GM_Visbeck_maxSlope
             ENDIF
c            M2loc = gravity*recip_rhoConst*dSigmaH
             N2loc = gravity*recip_rhoConst*dSigmaR
             IF ( N2loc.GT.0. _d 0 ) THEN
               SNloc = Sloc*SQRT(N2loc)
             ELSE
               SNloc = 0. _d 0
             ENDIF
           ELSE
             SNloc = 0. _d 0
           ENDIF
           VisbeckK(i,j,bi,bj) = VisbeckK(i,j,bi,bj)
     &       +deltaH*GM_Visbeck_alpha
     &              *GM_Visbeck_length*GM_Visbeck_length*SNloc
          ENDIF
         ENDDO
        ENDDO
       ENDIF
#endif /* GM_VISBECK_VARIABLE_K */
C     Z-Coords: change sign of vertical Sigma gradient (always downward)
C     to match stratification sign (i.e., positive if stratified)
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dSigmaDr(i,j) = gravitySign*sigmaR(i,j,k)
        ENDDO
       ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
       kkey = k + (tkey-1)*Nr
CADJ STORE dSigmaDx(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDy(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDr(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
# ifndef GM_EXCLUDE_FM07_TAP
CADJ STORE baseSlope(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE hTransLay(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE recipLambda(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
# endif
#endif /* ALLOW_AUTODIFF_TAMC */

C     set "rDepth" (= depth from the surface, in rUnit) for 'ldd97' tapering
       IF ( usingZcoords ) THEN
        rDepth = rF(1) - rF(k)
       ELSE
        rDepth = rF(k) - rF(Nr+1)
       ENDIF
C     Calculate slopes for use in tensor, taper and/or clip
       CALL GMREDI_SLOPE_LIMIT(
     O             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     U             hTransLay, baseSlope, recipLambda,
     U             dSigmaDr,
     I             dSigmaDx, dSigmaDy,
     I             ldd97_LrhoC, locMixLayer, rDepth, rF,
     I             kLowC(1-OLx,1-OLy,bi,bj),
     I             3, k, bi, bj, myTime, myIter, myThid )

#ifdef GMREDI_MASK_SLOPES
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
C      Mask Iso-neutral slopes
         SlopeX(i,j)   = SlopeX(i,j)*maskFk(i,j)
         SlopeY(i,j)   = SlopeY(i,j)*maskFk(i,j)
         SlopeSqr(i,j) = SlopeSqr(i,j)*maskFk(i,j)
        ENDDO
       ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE SlopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE SlopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE SlopeSqr(:,:)     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDr(:,:)     = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE taperFct(:,:)     = comlev1_bibj_k, key=kkey, byte=isbyte
#endif /* ALLOW_AUTODIFF_TAMC */

C     Components of Redi/GM tensor, first step with just the slope:
C     P-Coords: since slope comp. have same sign as in Z-coord, need to
C     flip sign of extra-diagonal component (see manual sec. 8.4.1.2.4)
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          Kwx(i,j,k,bi,bj) = -gravitySign*SlopeX(i,j)*taperFct(i,j)
          Kwy(i,j,k,bi,bj) = -gravitySign*SlopeY(i,j)*taperFct(i,j)
          Kwz(i,j,k,bi,bj) = SlopeSqr(i,j)*taperFct(i,j)
        ENDDO
       ENDDO

C-- end 1rst loop on vertical level index k
      ENDDO

#ifdef GM_VISBECK_VARIABLE_K
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE VisbeckK(:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
# endif
      IF ( GM_Visbeck_alpha.GT.zeroRL ) THEN
C-     Limit range that Kappa-GM can take
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         VisbeckK(i,j,bi,bj)=
     &       MIN( MAX( VisbeckK(i,j,bi,bj), GM_Visbeck_minVal_K ),
     &            GM_Visbeck_maxVal_K )
        ENDDO
       ENDDO
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          locK3dRedi(i,j,k) = locK3dRedi(i,j,k)
     &         + VisbeckK(i,j,bi,bj)*GM_isoFac_calcK
          locK3dGM(i,j,k)   = locK3dGM(i,j,k)
     &         + VisbeckK(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDIF
C     If you want to avoid recomputing the above loop, you can uncomment
C     the following lines.
c# ifdef ALLOW_AUTODIFF_TAMC
cCADJ STORE VisbeckK(:,:,bi,bj) = comlev1_bibj, key=tkey, byte=isbyte
c# endif
#endif /* GM_VISBECK_VARIABLE_K */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   Calculate Stream-Functions used in Advective Form:

#ifdef GM_BOLUS_ADVEC
      IF (GM_AdvForm) THEN
# ifdef GM_BOLUS_BVP
       IF (GM_UseBVP) THEN
        CALL GMREDI_CALC_PSI_BVP(
     I             bi, bj, iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             myThid )
       ELSE
# endif
# ifndef GM_BATES_PASSIVE
         IF ( .NOT.GM_useBatesK3d ) THEN
# endif
C          If using BatesK3d PsiX and PsiY are calculated in GMREDI_CALC_BATES_K
           CALL GMREDI_CALC_PSI_BOLUS(
     I              bi, bj, iMin, iMax, jMin, jMax,
     I              sigmaX, sigmaY, sigmaR, locK3dGM,
     I              ldd97_LrhoW, ldd97_LrhoS,
     I              myThid )
# ifndef GM_BATES_PASSIVE
         ENDIF
# endif
# ifdef GM_BOLUS_BVP
       ENDIF
# endif
      ENDIF
#endif /* GM_BOLUS_ADVEC */

#ifndef GM_EXCLUDE_SUBMESO
      IF ( GM_useSubMeso .AND. GM_AdvForm ) THEN
        CALL SUBMESO_CALC_PSI(
     I              bi, bj, iMin, iMax, jMin, jMax,
     I              sigmaX, sigmaY, sigmaR,
     I              locMixLayer,
     I              myIter, myThid )
c    U              locK3dRedi
      ENDIF
#endif /* ndef GM_EXCLUDE_SUBMESO */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Update components of Redi/GM tensor, second step: multiply by Diffusivity
      DO k=1,Nr
#ifdef ALLOW_AUTODIFF_TAMC
C to avoid few minor recomputations (with KAPREDI_CONTROL, KAPGM_CONTROL ?)
       kkey = k + (tkey-1)*Nr
CADJ STORE Kwx(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE Kwy(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE Kwz(:,:,k,bi,bj) = comlev1_bibj_k, key=kkey, byte=isbyte
#endif
       km1 = MAX(k-1,1)
       isopycK = GM_isopycK
     &         *(GM_isoFac1d(km1)+GM_isoFac1d(k))*op5
       bolus_K = GM_background_K
     &         *(GM_bolFac1d(km1)+GM_bolFac1d(k))*op5
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         Kredi_tmp =
#ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i,j,km1) + locK3dRedi(i,j,k) )
#else
     &      isopycK*GM_isoFac2d(i,j,bi,bj)
#endif
         Kgm_tmp = Kredi_tmp
#ifdef GM_USE_K3D_GM
     &    + GM_skewflx*op5*( locK3dGM(i,j,km1)+locK3dGM(i,j,k) )
#else
     &    + GM_skewflx*bolus_K*GM_bolFac2d(i,j,bi,bj)
#endif
#ifdef GM_GEOM_VARIABLE_K
     &    + GM_skewflx*GEOM_K3d(i,j,k,bi,bj)
#endif
         Kwx(i,j,k,bi,bj) = Kgm_tmp*Kwx(i,j,k,bi,bj)
         Kwy(i,j,k,bi,bj) = Kgm_tmp*Kwy(i,j,k,bi,bj)
         Kwz(i,j,k,bi,bj) = Kredi_tmp*Kwz(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics .AND. GM_taper_scheme.EQ.'fm07' ) THEN
       CALL DIAGNOSTICS_FILL( hTransLay, 'GM_hTrsL', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL( baseSlope, 'GM_baseS', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(recipLambda,'GM_rLamb', 0,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#if ( defined GM_NON_UNITY_DIAGONAL || defined GM_EXTRA_DIAGONAL )

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- 2nd  k loop : compute Tensor Coeff. at U point

# ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         locMixLayer(i,j) = ( KPPhbl(i-1,j,bi,bj)
     &                      + KPPhbl( i ,j,bi,bj) )*op5
        ENDDO
       ENDDO
      ELSE
# else
      IF ( .TRUE. ) THEN
# endif
       DO j=1-OLy,sNy+OLy
        DO i=2-OLx,sNx+OLx
         locMixLayer(i,j) = ( hMixLayer(i-1,j,bi,bj)
     &                      + hMixLayer( i ,j,bi,bj) )*op5
        ENDDO
       ENDDO
      ENDIF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         hTransLay(i,j) =  0.
         baseSlope(i,j) =  0.
         recipLambda(i,j)= 0.
       ENDDO
       DO i=2-OLx,sNx+OLx
         hTransLay(i,j) = MAX( R_low(i-1,j,bi,bj), R_low(i,j,bi,bj) )
       ENDDO
      ENDDO

      DO k=Nr,1,-1
       kp1 = MIN(Nr,k+1)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
# ifdef ALLOW_AUTODIFF_TAMC
       kkey = k + (tkey-1)*Nr
# endif

C     Gradient of Sigma at U points
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         dSigmaDx(i,j)=sigmaX(i,j,k)
     &                       *_maskW(i,j,k,bi,bj)
         dSigmaDy(i,j)=op25*( sigmaY(i-1,j+1,k)+sigmaY(i,j+1,k)
     &                       +sigmaY(i-1, j ,k)+sigmaY(i, j ,k)
     &                      )*_maskW(i,j,k,bi,bj)
         dSigmaDr(i,j)=op25*( sigmaR(i-1,j, k )+sigmaR(i,j, k )
     &                      +(sigmaR(i-1,j,kp1)+sigmaR(i,j,kp1))*maskp1
     &                      )*_maskW(i,j,k,bi,bj)*gravitySign
        ENDDO
       ENDDO

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDx(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDy(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDr(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#  ifndef GM_EXCLUDE_FM07_TAP
CADJ STORE baseSlope(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE hTransLay(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE recipLambda(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */

C     set "rDepth" (= depth from the surface, in rUnit) for 'ldd97' tapering
       IF ( usingZcoords ) THEN
        rDepth = rF(1) - rC(k)
       ELSE
        rDepth = rC(k) - rF(Nr+1)
       ENDIF
C     Calculate slopes for use in tensor, taper and/or clip
       CALL GMREDI_SLOPE_LIMIT(
     O             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     U             hTransLay, baseSlope, recipLambda,
     U             dSigmaDr,
     I             dSigmaDx, dSigmaDy,
     I             ldd97_LrhoW, locMixLayer, rDepth, rC,
     I             kLow_W,
     I             1, k, bi, bj, myTime, myIter, myThid )

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE taperFct(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */

# ifdef GM_NON_UNITY_DIAGONAL
c      IF ( GM_nonUnitDiag ) THEN
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kux(i,j,k,bi,bj) =
#  ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i-1,j,k) + locK3dRedi(i,j,k) )
#  else
     &      GM_isopycK*GM_isoFac1d(k)
     &        *op5*(GM_isoFac2d(i-1,j,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
     &     *taperFct(i,j)
         ENDDO
        ENDDO
#  if ( defined ALLOW_AUTODIFF_TAMC && defined GM_EXCLUDE_CLIPPING )
CADJ STORE Kux(:,:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kux(i,j,k,bi,bj) = MAX( Kux(i,j,k,bi,bj), GM_Kmin_horiz )
         ENDDO
        ENDDO
c      ENDIF
# endif /* GM_NON_UNITY_DIAGONAL */

# ifdef GM_EXTRA_DIAGONAL

#  ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE SlopeX(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif /* ALLOW_AUTODIFF_TAMC */
       IF ( GM_ExtraDiag ) THEN
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kuz(i,j,k,bi,bj) = -gravitySign*
#  ifdef GM_USE_K3D_REDI
     &     ( op5*( locK3dRedi(i-1,j,k) + locK3dRedi(i,j,k) )
#  else
     &     ( GM_isopycK*GM_isoFac1d(k)
     &        *op5*(GM_isoFac2d(i-1,j,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
#  ifdef GM_USE_K3D_GM
     &     - GM_skewflx*op5*( locK3dGM(i-1,j,k) + locK3dGM(i,j,k) )
#  else
     &     - GM_skewflx*GM_background_K*GM_bolFac1d(k)
     &        *op5*(GM_bolFac2d(i-1,j,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#  endif
#  ifdef GM_GEOM_VARIABLE_K
     &     - GM_skewflx*op25*( ( GEOM_K3d(i-1,j, k, bi,bj)
     &                         + GEOM_K3d( i, j, k, bi,bj) )
     &                       + ( GEOM_K3d(i-1,j,kp1,bi,bj)
     &                         + GEOM_K3d( i, j,kp1,bi,bj) ) )
#  endif
     &     )*SlopeX(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
c      ELSE
c       DO j=1-OLy+1,sNy+OLy-1
c        DO i=1-OLx+1,sNx+OLx-1
c         Kuz(i,j,k,bi,bj) = 0. _d 0
c        ENDDO
c       ENDDO
       ENDIF
# endif /* GM_EXTRA_DIAGONAL */

# ifdef ALLOW_DIAGNOSTICS
       IF (doDiagRediFlx) THEN
        km1 = MAX(k-1,1)
        DO j=1,sNy
         DO i=1,sNx+1
C         store in tmp1k Kuz_Redi
          tmp1k(i,j) = -gravitySign*
#  ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i-1,j,k)+ locK3dRedi(i,j,k) )
#  else
     &      GM_isopycK*GM_isoFac1d(k)
     &       *op5*(GM_isoFac2d(i-1,j,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
     &     *SlopeX(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
        DO j=1,sNy
         DO i=1,sNx+1
C-        Vertical gradients interpolated to U points
          dTdz = (
     &     +recip_drC(k)*
     &       ( maskC(i-1,j,km1,bi,bj)*maskC(i-1,j,k,bi,bj)*
     &           (theta(i-1,j,km1,bi,bj)-theta(i-1,j,k,bi,bj))
     &        +maskC( i ,j,km1,bi,bj)*maskC( i ,j,k,bi,bj)*
     &           (theta( i ,j,km1,bi,bj)-theta( i ,j,k,bi,bj))
     &       )
     &     +recip_drC(kp1)*
     &       ( maskC(i-1,j,k,bi,bj)*maskC(i-1,j,kp1,bi,bj)*
     &           (theta(i-1,j,k,bi,bj)-theta(i-1,j,kp1,bi,bj))
     &        +maskC( i ,j,k,bi,bj)*maskC( i ,j,kp1,bi,bj)*
     &           (theta( i ,j,k,bi,bj)-theta( i ,j,kp1,bi,bj))
     &       )      ) * 0.25 _d 0
           tmp1k(i,j) = dyG(i,j,bi,bj) * deepFacC(k)
     &                * drF(k) * _hFacW(i,j,k,bi,bj)
     &                * tmp1k(i,j) * dTdz
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmp1k, 'GM_KuzTz', k,1,2,bi,bj,myThid)
       ENDIF
# endif /* ALLOW_DIAGNOSTICS */

C-- end 2nd  loop on vertical level index k
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- 3rd  k loop : compute Tensor Coeff. at V point

# ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locMixLayer(i,j) = ( KPPhbl(i,j-1,bi,bj)
     &                      + KPPhbl(i, j ,bi,bj) )*op5
        ENDDO
       ENDDO
      ELSE
# else
      IF ( .TRUE. ) THEN
# endif
       DO j=2-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         locMixLayer(i,j) = ( hMixLayer(i,j-1,bi,bj)
     &                      + hMixLayer(i, j ,bi,bj) )*op5
        ENDDO
       ENDDO
      ENDIF
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         hTransLay(i,j) =  0.
         baseSlope(i,j) =  0.
         recipLambda(i,j)= 0.
       ENDDO
      ENDDO
      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
         hTransLay(i,j) = MAX( R_low(i,j-1,bi,bj), R_low(i,j,bi,bj) )
       ENDDO
      ENDDO

C     Gradient of Sigma at V points
      DO k=Nr,1,-1
       kp1 = MIN(Nr,k+1)
       maskp1 = 1. _d 0
       IF (k.GE.Nr) maskp1 = 0. _d 0
# ifdef ALLOW_AUTODIFF_TAMC
       kkey = k + (tkey-1)*Nr
# endif

       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         dSigmaDx(i,j)=op25*( sigmaX(i, j ,k) +sigmaX(i+1, j ,k)
     &                       +sigmaX(i,j-1,k) +sigmaX(i+1,j-1,k)
     &                      )*_maskS(i,j,k,bi,bj)
         dSigmaDy(i,j)=sigmaY(i,j,k)
     &                       *_maskS(i,j,k,bi,bj)
         dSigmaDr(i,j)=op25*( sigmaR(i,j-1, k )+sigmaR(i,j, k )
     &                      +(sigmaR(i,j-1,kp1)+sigmaR(i,j,kp1))*maskp1
     &                      )*_maskS(i,j,k,bi,bj)*gravitySign
        ENDDO
       ENDDO

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE dSigmaDx(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDy(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE dSigmaDr(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#  ifndef GM_EXCLUDE_FM07_TAP
CADJ STORE baseSlope(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE hTransLay(:,:)      = comlev1_bibj_k, key=kkey, byte=isbyte
CADJ STORE recipLambda(:,:)    = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
# endif /* ALLOW_AUTODIFF_TAMC */

C     set "rDepth" (= depth from the surface, in rUnit) for 'ldd97' tapering
       IF ( usingZcoords ) THEN
        rDepth = rF(1) - rC(k)
       ELSE
        rDepth = rC(k) - rF(Nr+1)
       ENDIF
C     Calculate slopes for use in tensor, taper and/or clip
       CALL GMREDI_SLOPE_LIMIT(
     O             SlopeX, SlopeY,
     O             SlopeSqr, taperFct,
     U             hTransLay, baseSlope, recipLambda,
     U             dSigmaDr,
     I             dSigmaDx, dSigmaDy,
     I             ldd97_LrhoS, locMixLayer, rDepth, rC,
     I             kLow_S,
     I             2, k, bi, bj, myTime, myIter, myThid )

# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE taperFct(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
# endif /* ALLOW_AUTODIFF_TAMC */

# ifdef GM_NON_UNITY_DIAGONAL
c      IF ( GM_nonUnitDiag ) THEN
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kvy(i,j,k,bi,bj) =
#  ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i,j-1,k) + locK3dRedi(i,j,k) )
#  else
     &      GM_isopycK*GM_isoFac1d(k)
     &        *op5*(GM_isoFac2d(i,j-1,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
     &     *taperFct(i,j)
         ENDDO
        ENDDO
#  if ( defined ALLOW_AUTODIFF_TAMC && defined GM_EXCLUDE_CLIPPING )
CADJ STORE Kvy(:,:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kvy(i,j,k,bi,bj) = MAX( Kvy(i,j,k,bi,bj), GM_Kmin_horiz )
         ENDDO
        ENDDO
c      ENDIF
# endif /* GM_NON_UNITY_DIAGONAL */

# ifdef GM_EXTRA_DIAGONAL

#  ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE SlopeY(:,:)       = comlev1_bibj_k, key=kkey, byte=isbyte
#  endif /* ALLOW_AUTODIFF_TAMC */
       IF ( GM_ExtraDiag ) THEN
        DO j=1-OLy+1,sNy+OLy-1
         DO i=1-OLx+1,sNx+OLx-1
          Kvz(i,j,k,bi,bj) = -gravitySign*
#  ifdef GM_USE_K3D_REDI
     &     ( op5*( locK3dRedi(i,j-1,k) + locK3dRedi(i,j,k) )
#  else
     &     ( GM_isopycK*GM_isoFac1d(k)
     &        *op5*(GM_isoFac2d(i,j-1,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
#  ifdef GM_USE_K3D_GM
     &     - GM_skewflx*op5*( locK3dGM(i,j-1,k) + locK3dGM(i,j,k) )
#  else
     &     - GM_skewflx*GM_background_K*GM_bolFac1d(k)
     &        *op5*(GM_bolFac2d(i,j-1,bi,bj)+GM_bolFac2d(i,j,bi,bj))
#  endif
#  ifdef GM_GEOM_VARIABLE_K
     &     - GM_skewflx*op25*( ( GEOM_K3d(i,j-1, k, bi,bj)
     &                         + GEOM_K3d(i, j,  k, bi,bj) )
     &                       + ( GEOM_K3d(i,j-1,kp1,bi,bj)
     &                         + GEOM_K3d(i, j, kp1,bi,bj) ) )
#  endif
     &     )*SlopeY(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
c      ELSE
c       DO j=1-OLy+1,sNy+OLy-1
c        DO i=1-OLx+1,sNx+OLx-1
c         Kvz(i,j,k,bi,bj) = 0. _d 0
c        ENDDO
c       ENDDO
       ENDIF
# endif /* GM_EXTRA_DIAGONAL */

# ifdef ALLOW_DIAGNOSTICS
       IF (doDiagRediFlx) THEN
        km1 = MAX(k-1,1)
        DO j=1,sNy+1
         DO i=1,sNx
C         store in tmp1k Kvz_Redi
          tmp1k(i,j) = -gravitySign*
#  ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i,j-1,k) + locK3dRedi(i,j,k) )
#  else
     &      GM_isopycK*GM_isoFac1d(k)
     &       *op5*(GM_isoFac2d(i,j-1,bi,bj)+GM_isoFac2d(i,j,bi,bj))
#  endif
     &     *SlopeY(i,j)*taperFct(i,j)
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx
C-        Vertical gradients interpolated to U points
          dTdz = (
     &     +recip_drC(k)*
     &       ( maskC(i,j-1,km1,bi,bj)*maskC(i,j-1,k,bi,bj)*
     &           (theta(i,j-1,km1,bi,bj)-theta(i,j-1,k,bi,bj))
     &        +maskC(i, j ,km1,bi,bj)*maskC(i, j ,k,bi,bj)*
     &           (theta(i, j ,km1,bi,bj)-theta(i, j ,k,bi,bj))
     &       )
     &     +recip_drC(kp1)*
     &       ( maskC(i,j-1,kp1,bi,bj)*maskC(i,j-1,k,bi,bj)*
     &           (theta(i,j-1,k,bi,bj)-theta(i,j-1,kp1,bi,bj))
     &        +maskC(i, j ,kp1,bi,bj)*maskC(i, j ,k,bi,bj)*
     &           (theta(i, j ,k,bi,bj)-theta(i, j ,kp1,bi,bj))
     &       )      ) * 0.25 _d 0
           tmp1k(i,j) = dxG(i,j,bi,bj) * deepFacC(k)
     &                * drF(k) * _hFacS(i,j,k,bi,bj)
     &                * tmp1k(i,j) * dTdz
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmp1k, 'GM_KvzTz', k,1,2,bi,bj,myThid)
       ENDIF
# endif /* ALLOW_DIAGNOSTICS */

C-- end 3rd  loop on vertical level index k
      ENDDO

#endif /* GM_NON_UNITY_DIAGONAL || GM_EXTRA_DIAGONAL */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifndef GM_NON_UNITY_DIAGONAL
C--   keep a simplified setting here (used to be inside gmredi_x/ytransport)
C     for backeard compatibility + for trying to generate a simpler adjoint code
      DO k=1,Nr
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          Kux(i,j,k,bi,bj) =
# ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i-1,j,k) + locK3dRedi(i,j,k) )
# else
     &      GM_isopycK
# endif
        ENDDO
       ENDDO
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
          Kvy(i,j,k,bi,bj) =
# ifdef GM_USE_K3D_REDI
     &      op5*( locK3dRedi(i,j-1,k) + locK3dRedi(i,j,k) )
# else
     &      GM_isopycK
# endif
        ENDDO
       ENDDO
      ENDDO
#endif /* ndef GM_NON_UNITY_DIAGONAL */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL GMREDI_DIAGNOSTICS_FILL( bi, bj, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_GMREDI */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
C     !ROUTINE: GMREDI_CALC_TENSOR_DUMMY
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_TENSOR_DUMMY(
     I             iMin, iMax, jMin, jMax,
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_TENSOR_DUMMY
C     | o Calculate tensor elements for GM/Redi tensor.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin,iMax,jMin,jMax
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
      INTEGER i, j, k

      DO k=1,Nr
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         Kwx(i,j,k,bi,bj) = 0. _d 0
         Kwy(i,j,k,bi,bj) = 0. _d 0
         Kwz(i,j,k,bi,bj) = 0. _d 0
         Kux(i,j,k,bi,bj) = 0. _d 0
         Kvy(i,j,k,bi,bj) = 0. _d 0
# ifdef GM_EXTRA_DIAGONAL
         Kuz(i,j,k,bi,bj) = 0. _d 0
         Kvz(i,j,k,bi,bj) = 0. _d 0
# endif
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_GMREDI */

      RETURN
      END
