#include "GMREDI_OPTIONS.h"

CBOP
C !ROUTINE: GMREDI_CALC_GEOM

C !INTERFACE: ==========================================================
      SUBROUTINE GMREDI_CALC_GEOM(
     I             sigmaX, sigmaY, sigmaR,
     I             bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_GEOM
C     | Calculate GM coefficient with the GEOMETRIC prescription
C     | GEOM_K3d is located at the grid-cell vertical interface.
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "GMREDI.h"

C !INPUT PARAMETERS: ===================================================
C     sigmaXYR  :: density gradient variables
C     bi, bj    :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: My Thread Id. number
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef GM_GEOM_VARIABLE_K
C !INOUT PARAMETERS: ===================================================
C  GEOM_K3d     :: GEOMETRIC K-GM (GEOMETRIC in horizontal,
C                             structure function in vertical)
C  GEOM_EKE     :: GEOMETRIC parameterised energy
C              not really an output, passed around as common block

C !LOCAL VARIABLES: ====================================================
C     i,j,k           :: Loop counters
C     Slope(XY)       :: isopycnal slopes
C     dSigmaD(xyr)    :: derivative of density variables
C     dSigma(HR)      :: variables to compute isopycnal slopes
C     (MN)2loc        :: local M^2 and N^2
C     S(N)loc         :: local M^2/N^2 and M^2/N
C     (S)Nloc_zint    :: depth integrated M^2/N and N
C                       (for calculating trd_ene_gen and trd_ene_wav)
C     trd_ene_*       :: trends for energy
C                        gen, adv, lap, dis
C     ene_*           :: intermediate variables for computing trends
C     depth*          :: various depth variables for computation purposes
C     c1, c_ros[XYEN] :: long Rossby wave phase speeds for trd_ene_wav
C     UV_depth_avg    :: depth-avg flow for                trd_ene_adv
C     ab0, ab1        :: Adams-Bashforth weights

      INTEGER i,j,k
      INTEGER kSurf
      _RL dSigmaDx
      _RL dSigmaDy
      _RL dSigmaDr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL dSigmaH, dSigmaR
      _RL N2loc, SNloc, Sloc
      _RL SNloc3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N2loc3D(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

      _RL recipMaxSlope
      _RL c_dum, fp_im, fm_ip
      _RL ab0, ab1, fCoriFac

C     metric variables
      _RL recip_depthW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_depthS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
C     depth at C-points in meters
      _RL depthC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     hFac of the W-control volumes
      _RL hFac

C     general variables
      _RL deltaTloc

C     local version of current rhs of eddy energy equation
      _RL ene_rhs_now(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vert_struc_func(1-OLx:sNx+OLx,1-OLy:sNy+OLy, Nr)

      _RL SNloc_zint (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  Nloc_zint (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL trd_ene_gen(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL trd_ene_dis(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL trd_ene_adv(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL trd_ene_wav(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL trd_ene_lap(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL U_depth_avg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL V_depth_avg(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL c1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL c_rosX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL c_rosY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL c_rosE

      _RL ene_adv_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_adv_y(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_wav_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_wav_y(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_lap_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_lap_y(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      kSurf = 1
      IF ( usingPCoords ) kSurf = Nr
      deltaTloc = dtTracerLev(kSurf)
      recipMaxSlope = 0. _d 0
      IF ( GM_maxSlope.GT.0. _d 0 ) THEN
        recipMaxSlope = 1. _d 0 / GM_maxSlope
      ENDIF

C--   initialise some variables for calculations

C     depths for doing depth-averaging
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        recip_depthW(i,j) = 0.0 _d 0
        recip_depthS(i,j) = 0.0 _d 0
        c_dum = rSurfW(i,j,bi,bj) - rLowW(i,j,bi,bj)
        IF ( c_dum .GT. zeroRL ) recip_depthW(i,j) = 1. _d 0 / c_dum
        c_dum = rSurfS(i,j,bi,bj) - rLowS(i,j,bi,bj)
        IF ( c_dum .GT. zeroRL ) recip_depthS(i,j) = 1. _d 0 / c_dum
C     convert to meters in the case of p-coordinates
        depthC(i,j) = ( Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj) )
     &              * rUnit2mass*recip_rhoConst
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C   TODO: should the calculations be based on the tapered slopes?
C         here it is just the raw stuff
C         if being moved then this routine might want to be after
C         the SLOPE_LIMIT routine

C     initialisations, put this in a loop if need be
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        SNloc_zint(i,j) = 0.0 _d 0
        Nloc_zint(i,j)  = 0.0 _d 0
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         vert_struc_func(i,j,k) = 1.0 _d 0
         N2loc3D(i,j,k)         = 0.0 _d 0
         SNloc3D(i,j,k)         = 0.0 _d 0
        ENDDO
       ENDDO
      ENDDO
C-- 1st k loop : compute vertical structure to be used later
      DO k=Nr,2,-1

C     For stable conditions sigmaR<0 for z-coordinates, but >0 for p-coords
C     => change sign of vertical Sigma gradient (always downward)
C     to match stratification sign (i.e., positive if stratified)
C     For p-coordinates: convert r-units to z
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dSigmaDr(i,j) = MAX( gravitySign*sigmaR(i,j,k), zeroRL )
     &        * mass2rUnit * rhoConst
        ENDDO
       ENDDO

C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         IF ( maskC(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN

C      Compute -N^2 rho_0 / g via averaging, on vertical interfaces
          dSigmaR = (dSigmaDr(i,j) * 4.0 _d 0
     &                + maskC(i-1,j,k,bi,bj)*dSigmaDr(i-1,j)
     &                + maskC(i+1,j,k,bi,bj)*dSigmaDr(i+1,j)
     &                + maskC(i,j-1,k,bi,bj)*dSigmaDr(i,j-1)
     &                + maskC(i,j+1,k,bi,bj)*dSigmaDr(i,j+1)
     &                 ) / (4.0 _d 0
     &                    + maskC(i-1,j,k,bi,bj)
     &                    + maskC(i+1,j,k,bi,bj)
     &                    + maskC(i,j-1,k,bi,bj)
     &                    + maskC(i,j+1,k,bi,bj)
     &                     )
C      Compute M^2 rho_0 / g on vertical interfaces
          dSigmaDx = op25 * (sigmaX(i+1,j,k-1) + sigmaX(i,j,k-1)
     &                    +  sigmaX(i+1,j,k  ) + sigmaX(i,j,k  )
     &                       ) *  maskC(i,j,k,bi,bj)
          dSigmaDy = op25 * (sigmaY(i,j+1,k-1) + sigmaY(i,j,k-1)
     &                    +  sigmaY(i,j+1,k  ) + sigmaY(i,j,k  )
     &                       ) *  maskC(i,j,k,bi,bj)
          dSigmaH = SQRT(dSigmaDx * dSigmaDx
     &                 + dSigmaDy * dSigmaDy)
          IF ( dSigmaH .GT. 0. _d 0 ) THEN
           IF ( dSigmaR .GT. dSigmaH*recipMaxSlope ) THEN
            Sloc = dSigmaH / dSigmaR
           ELSE
            Sloc = GM_maxSlope
           ENDIF
           N2loc3D(i,j,k) = gravity * recip_rhoConst * dSigmaR
           SNloc3D(i,j,k) = Sloc * SQRT(N2loc3D(i,j,k))
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      IF ( GEOM_vert_struc ) THEN
       k=2
C      avoid division by zero
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         vert_struc_func(i,j,k) = MAX(N2loc3D(i,j,k),
     &                                GEOM_vert_struc_min)
        ENDDO
       ENDDO
C      Cap the (N^2 / N^2_surf) between something (1 and 0.1 default)
       DO k=Nr,2,-1
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          vert_struc_func(i,j,k) =
     &         MAX(MIN(GEOM_vert_struc_max,
     &                 N2loc3D(i,j,k)/vert_struc_func(i,j,2)),
     &             GEOM_vert_struc_min)
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C-- 2nd k loop : compute the rest of the GEOMETRIC stuff
      DO k=Nr,2,-1

C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         IF ( maskC(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
          N2loc = N2loc3D(i,j,k)
          SNloc = SNloc3D(i,j,k)
C     hFac now contains the coordinate factor rUnit2mass*recip_rhoConst
C     ( = 1 for z-coordinates and 1/(gravity*rhoConst for p-coordinates)
          hFac = MIN( halfRS, _hFacC(i,j,k-1,bi,bj) )
     &         + MIN( halfRS, _hFacC(i,j,k  ,bi,bj) )
     &                    * rUnit2mass*recip_rhoConst
          SNloc_zint(i,j) = SNloc_zint(i,j)
     &                    + SNloc * drC(k) * hFac
     &                    * vert_struc_func(i,j,k)
          Nloc_zint(i,j)  = Nloc_zint(i,j) + SQRT(N2loc)
     &                    * drC(k) * hFac
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     work out the implied long Rossby phase speeds at T pts
C     average it onto UV points later

      fCoriFac = 0.0 _d 0
      IF ( usingCartesianGrid .AND. f0 .NE. 0.0 _d 0 )
     &     fCoriFac = beta / (f0 * f0)

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C       only compute the Rossby phase speed if deep enough
        IF (depthC(i,j) .GT. 300.0 _d 0) THEN
         c1(i,j) = MIN(10.0 _d 0, Nloc_zint(i,j) / PI)
     &              * maskC(i,j,kSurf,bi,bj)

C       bound the f factors away from zero (inside Eq +/- 5degN/S),
C       compute the Rossby phase speed, and bound above by
C       1st baroclinic equatorial Rossby wave phase speed
         IF ( .NOT.usingCartesianGrid ) THEN
          fCoriFac = MAX( ABS(fCori(i,j,bi,bj)), 1.2676 _d -05 )
          fCoriFac = fCoriCos(i,j,bi,bj) / ( rSphere * fCoriFac ** 2 )
         ENDIF
         c_rosE = - MIN( c1(i,j)/3.0 _d 0, c1(i,j)*c1(i,j)*fCoriFac )
C     rotating EW Rossby velocity to XY for the grid, c_rosN=0 by definition
C     (trivial rotation for Cartesian plane)
         c_rosX(i,j) =  angleCosC(i,j,bi,bj)*c_rosE
c    &                + angleSinC(i,j,bi,bj)*c_rosN
         c_rosY(i,j) = -angleSinC(i,j,bi,bj)*c_rosE
c    &                 +angleCosC(i,j,bi,bj)*c_rosN
        ELSE
         c_rosX(i,j) = 0.0 _d 0
         c_rosY(i,j) = 0.0 _d 0
         c1    (i,j) = 0.0 _d 0
        ENDIF
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- compute kgm as per GEOMETRIC in horizontal, then extend
C-- vertically by structure function if required
C-- do other tapering things here

C     1. compute the 2d k_GEOM first
C        bound the denominator from below (choice following NEMO)

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C     for cubed sphere setups, depthC can be zero in the halo, so we
C     include it in the masking
        IF ( maskC(i,j,kSurf,bi,bj)*depthC(i,j) .GT. 0. _d 0 ) THEN
         GEOM_K3d(i,j,kSurf,bi,bj) = GEOM_alpha * GEOM_EKE(i,j,bi,bj)
     &        / MAX( SNloc_zint(i,j), 1. _d -7 * depthC(i,j) )
        ENDIF
       ENDDO
      ENDDO

C     2. cap k_GEOM from above! (stop it being too big)

      IF ( GEOM_alpha .NE. 0.0 _d 0 ) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         GEOM_K3d(i,j,kSurf,bi,bj) =
     &        MIN( GEOM_K3d(i,j,kSurf,bi,bj), GEOM_maxVal_K )
        ENDDO
       ENDDO
      ENDIF

C     3. taper it according to depth to kill k_GEOM in shallow regions

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        GEOM_K3d(i,j,kSurf,bi,bj) = GEOM_K3d(i,j,kSurf,bi,bj)
     &       * GEOM_taper(i,j,bi,bj) * maskC(i,j,kSurf,bi,bj)
       ENDDO
      ENDDO

C     4. extend the (tapered) k_GEOM in depth

      DO k=Nr,2,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         GEOM_K3d(i,j,k,bi,bj) = vert_struc_func(i,j,k)
     &                      * GEOM_K3d(i,j,kSurf,bi,bj)
     &                      * maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     5. cap k_GEOM from below (do not strictly need this)

      IF ( GEOM_alpha .NE. 0.0 _d 0 ) THEN
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GEOM_K3d(i,j,k,bi,bj) =
     &         MAX( GEOM_K3d(i,j,k,bi,bj), GEOM_minVal_K )
         ENDDO
        ENDDO
       ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   time step the energy
C     allocate and initialise
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        trd_ene_gen(i,j) = 0.0 _d 0
       ENDDO
      ENDDO

C     loop over k
      DO k=Nr,2,-1

C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
       DO j=2-OLy,sNy+OLy-1
        DO i=2-OLx,sNx+OLx-1
         IF ( maskC(i,j,k,bi,bj) .GT. 0. _d 0 ) THEN
          hFac = MIN( halfRS, _hFacC(i,j,k-1,bi,bj) )
     &         + MIN( halfRS, _hFacC(i,j,k  ,bi,bj) )
          SNloc = SNloc3D(i,j,k)
          trd_ene_gen(i,j) = trd_ene_gen(i,j)
     &                     + GEOM_K3d(i,j,k,bi,bj)
     &                     * SNloc * SNloc
     &                     * drC(k) * hFac
     &                     * rUnit2mass*recip_rhoConst
         ENDIF
        ENDDO
       ENDDO
      ENDDO
C ----------------------------------------------
C ---- start advection of energy ---------------
C ----------------------------------------------

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        trd_ene_dis(i,j) = 0.0 _d 0
        trd_ene_adv(i,j) = 0.0 _d 0
        trd_ene_wav(i,j) = 0.0 _d 0
        trd_ene_lap(i,j) = 0.0 _d 0

        U_depth_avg(i,j) = 0.0 _d 0
        V_depth_avg(i,j) = 0.0 _d 0

        ene_adv_x(i,j)   = 0.0 _d 0
        ene_adv_y(i,j)   = 0.0 _d 0
        ene_wav_x(i,j)   = 0.0 _d 0
        ene_wav_y(i,j)   = 0.0 _d 0
        ene_lap_x(i,j)   = 0.0 _d 0
        ene_lap_y(i,j)   = 0.0 _d 0
        ene_rhs_now(i,j) = 0.0 _d 0
       ENDDO
      ENDDO

C ----  form the energy fluxes

C     1. form the depth-AVG velocity
      DO k= 1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         U_depth_avg(i,j) = U_depth_avg(i,j)
     &                    + uVel(i,j,k,bi,bj) * drF(k)
     &                    * hFacW(i,j,k,bi,bj) * recip_depthW(i,j)
         V_depth_avg(i,j) = V_depth_avg(i,j)
     &                    + vVel(i,j,k,bi,bj) * drF(k)
     &                    * hFacS(i,j,k,bi,bj) * recip_depthS(i,j)
        ENDDO
       ENDDO
      ENDDO

C     2. form the advective tendency in each direction
      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
C-    X direction:
C ---- second ordered centred difference
c       ene_adv_x(i,j) = -halfRL
c    &    *( U_depth_avg(i,j) * dyG(i,j,bi,bj)
c    &       *( GEOM_EKE( i ,j,bi,bj) - GEOM_EKE(i-1,j,bi,bj) )
c    &     + U_depth_avg(i+1,j) * dyG(i+1,j,bi,bj)
c    &       *( GEOM_EKE(i+1,j,bi,bj) - GEOM_EKE( i ,j,bi,bj) )
c    &     )
C ---- 1st order upwinding
        fp_im = U_depth_avg( i ,j) + ABS( U_depth_avg( i ,j) )
        fm_ip = U_depth_avg(i+1,j) - ABS( U_depth_avg(i+1,j) )
        ene_adv_x(i,j) = -halfRL
     &    *( fp_im * dyG(i,j,bi,bj)
     &       *( GEOM_EKE( i ,j,bi,bj) - GEOM_EKE(i-1,j,bi,bj) )
     &     + fm_ip * dyG(i+1,j,bi,bj)
     &       *( GEOM_EKE(i+1,j,bi,bj) - GEOM_EKE( i ,j,bi,bj) )
     &     )
C ---- 1st order upwinding wave stuff
        c_dum = ( c_rosX(i-1,j) + c_rosX(i,j) )*halfRL
        fp_im = ( c_dum + ABS( c_dum ) )*maskW( i ,j,kSurf,bi,bj)
        c_dum = ( c_rosX(i,j) + c_rosX(i+1,j) )
        fm_ip = ( c_dum - ABS( c_dum ) )*maskW(i+1,j,kSurf,bi,bj)
        ene_wav_x(i,j) = -halfRL
     &    *( fp_im * dyG(i,j,bi,bj)
     &       *( GEOM_EKE( i ,j,bi,bj) - GEOM_EKE(i-1,j,bi,bj) )
     &     + fm_ip * dyG(i+1,j,bi,bj)
     &       *( GEOM_EKE(i+1,j,bi,bj) - GEOM_EKE( i ,j,bi,bj) )
     &     )
C-    Y direction:
C ---- second ordered centred difference
c       ene_adv_y(i,j) = -halfRL
c    &    *( V_depth_avg(i,j) * dxG(i,j,bi,bj)
c    &       *( GEOM_EKE(i, j ,bi,bj) - GEOM_EKE(i,j-1,bi,bj) )
c    &     + V_depth_avg(i,j+1) * dxG(i,j+1,bi,bj)
c    &       *( GEOM_EKE(i,j+1,bi,bj) - GEOM_EKE(i, j ,bi,bj) )
c    &     )
C ---- 1st order upwinding
        fp_im = V_depth_avg(i, j ) + ABS( V_depth_avg(i, j ) )
        fm_ip = V_depth_avg(i,j+1) - ABS( V_depth_avg(i,j+1) )
        ene_adv_y(i,j) = -halfRL
     &    *( fp_im * dxG(i,j,bi,bj)
     &       *( GEOM_EKE(i, j ,bi,bj) - GEOM_EKE(i,j-1,bi,bj) )
     &     + fm_ip * dxG(i,j+1,bi,bj)
     &       *( GEOM_EKE(i,j+1,bi,bj) - GEOM_EKE(i, j ,bi,bj) )
     &     )
C ---- 1st order upwinding wave stuff
        c_dum = ( c_rosY(i-1,j) + c_rosY(i,j) )*halfRL
        fp_im = ( c_dum + ABS( c_dum ) )*maskS(i, j ,kSurf,bi,bj)
        c_dum = ( c_rosY(i,j) + c_rosY(i,j+1) )
        fm_ip = ( c_dum - ABS( c_dum ) )*maskS(i,j+1,kSurf,bi,bj)
        ene_wav_y(i,j) = -halfRL
     &    *( fp_im * dxG(i,j,bi,bj)
     &       *( GEOM_EKE(i, j ,bi,bj) - GEOM_EKE(i,j-1,bi,bj) )
     &     + fm_ip * dxG(i,j+1,bi,bj)
     &       *( GEOM_EKE(i,j+1,bi,bj) - GEOM_EKE(i, j ,bi,bj) )
     &     )
       ENDDO
      ENDDO

C     3. form the diffusive fluxes in each direction
      DO j=1-OLy,sNy+OLy
       DO i=2-OLx,sNx+OLx
        ene_lap_x(i,j) = -GEOM_diffKh_EKE * dyG(i,j,bi,bj)
     &       * ( GEOM_EKE(i,j,bi,bj) - GEOM_EKE(i-1,j,bi,bj) )
     &       * recip_dxC(i,j,bi,bj)*maskW(i,j,kSurf,bi,bj)
     &       * cosFacU(j,bi,bj)
       ENDDO
      ENDDO
      DO j=2-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        ene_lap_y(i,j) = -GEOM_diffKh_EKE * dxG(i,j,bi,bj)
     &       * ( GEOM_EKE(i,j,bi,bj) - GEOM_EKE(i,j-1,bi,bj) )
     &       * recip_dyC(i,j,bi,bj)*maskS(i,j,kSurf,bi,bj)
#ifdef ISOTROPIC_COS_SCALING
     &       *cosFacV(j,bi,bj)
#endif
       ENDDO
      ENDDO

C     4. sum or form tendencies on cell
      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
        trd_ene_dis(i,j) = - GEOM_lmbda * GEOM_EKE(i,j,bi,bj)
     &                                          *maskInC(i,j,bi,bj)
C     Add advective contribution and divide by grid cell area
        trd_ene_adv(i,j) = ( ene_adv_x(i,j) + ene_adv_y(i,j) )
     &                     * recip_rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
        trd_ene_wav(i,j) = ( ene_wav_x(i,j) + ene_wav_y(i,j) )
     &                     * recip_rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
C     Form diffusive tendency from (minus) flux divergence
        trd_ene_lap(i,j) = -( ene_lap_x(i+1,j) - ene_lap_x(i,j)
     &                      + ene_lap_y(i,j+1) - ene_lap_y(i,j) )
     &                      *recip_rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
C       form the RHS
        ene_rhs_now(i,j) = trd_ene_gen(i,j)
     &                   + trd_ene_dis(i,j)
     &                   + trd_ene_adv(i,j)
     &                   + trd_ene_wav(i,j)
     &                   + trd_ene_lap(i,j)
       ENDDO
      ENDDO

C     At this point, the new ene_rhs_now is defined and correctly
C     computed for all i,j except for 1-OLx/y and sNx/y+Olx/y, and we do
C     not need any exchange. We do need to exchange the solution GEOM_EKE
C     in S/R GMREDI_DO_EXCH.

C     third: time stepping
      IF ( GEOM_startAB .EQ. 0 ) THEN
       ab0 =  1.0 _d 0
       ab1 =  0.0 _d 0
C-    moved to gmredi_do_exch.F (outside bi,bj loop):
c       GEOM_startAB = 1
      ELSE
       ab0 =  1.5 _d 0 + abEps
       ab1 = -0.5 _d 0 - abEps
      ENDIF
      DO j=2-OLy,sNy+OLy-1
       DO i=2-OLx,sNx+OLx-1
        GEOM_EKE(i,j,bi,bj) = GEOM_EKE(i,j,bi,bj)
     &            + deltaTloc * (
     &            + ab0 * ene_rhs_now(i,j)
     &            + ab1 * GEOM_gEKE_Nm1(i,j,bi,bj)
     &                        )
       ENDDO
      ENDDO
C ----------------------------------------------
C ---- end advection of energy -----------------
C ----------------------------------------------

C     update rhs fields for the next time step
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        GEOM_gEKE_Nm1(i,j,bi,bj) = ene_rhs_now(i,j)
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   diagnostics and restarts

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(vert_struc_func,
     &                       'GEOMstru', 0,Nr,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(trd_ene_gen,
     &                       'GEOMEgen', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(trd_ene_dis,
     &                       'GEOMEdis', 0,1,2,bi,bj,myThid)
C        advective trends are dE/dt + -(u - c) dot grad E
       CALL DIAGNOSTICS_FILL(trd_ene_adv,
     &                       'GEOMEadv', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(trd_ene_wav,
     &                       'GEOMEwav', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(trd_ene_lap,
     &                       'GEOMElap', 0,1,2,bi,bj,myThid)
       CALL DIAGNOSTICS_FILL(c1,
     &                       'GEOM_c1 ', 0,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_GEOM_VARIABLE_K */

      RETURN
      END
