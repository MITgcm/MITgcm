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
C     | GEOMK is located at the cell centre.
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
C     sigmaXYR  :: density variables
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

C !INOUT PARAMETERS: ==================================================
C  GEOMK        :: GEOMETRIC kgm (GEOMETRIC in horizontal,
C                             structure function in vertical)
C  GEOM_ene     :: GEOMETRIC parameterised energy
C              not really an output, passed around as common block

C !LOCAL VARIABLES: ====================================================
C     i,j,k        :: Loop counters
C     Slope(XY)    :: isopycnal slopes
C     dSigmaD(xyr) :: derivative of density variables
C     dSigma(HR)   :: variables to compute isopycnal slopes
C     (MN)2loc     :: local M^2 and N^2
C     S(N)loc      :: local M^2/N^2 and M^2/N
C     (S)Nloc_zint :: depth integrated M^2/N and N
C                       (for calculating trd_ene_gen and trd_ene_wav)
C     trd_ene_*    :: trends for energy
C                       gen, adv, lap, dis
C     ene_*        :: intermediate variables for computing trends
C     depth*       :: various depth variables for computation purposes
C     c1, c_ros    :: long Rossby wave phase speeds for trd_ene_wav
C     UV_depth_avg :: depth-avg flow for                trd_ene_adv

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
      _RL c_ros_u, fm_ui, fp_ui

C     metric variables
      _RL recip_depthW(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_depthS(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL depthC      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL depth_taper (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     general variables, overwritten every time step
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

      _RL c1   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL c_ros(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      _RL ene_adv_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_adv_y(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_wav_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_lap_x(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ene_lap_y(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      kSurf = 1
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
        IF ( rLowW(i,j,bi,bj) .NE. 0. _d 0 )
     &       recip_depthW(i,j) = 1. _d 0 / ABS(rLowW(i,j,bi,bj))
        IF ( rLowS(i,j,bi,bj) .NE. 0. _d 0 )
     &       recip_depthS(i,j) = 1. _d 0 / ABS(rLowS(i,j,bi,bj))
        depthC(i,j)      = ABS(R_low(i,j,bi,bj))
        depth_taper(i,j) = 0.0 _d 0
       ENDDO
      ENDDO

C     temporary variable for depth tapering later
      DO j=1-OLy+1,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
        depth_taper(i,j) = (0.5 _d 0 *
     &       (1.0 _d 0 - TANH(
     &       (-depthC(i,j) + 800.0 _d 0) / 300.0 _d 0
     &       ) ) ) ** 2
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

       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         dSigmaDr(i,j) = min(sigmaR(i,j,k), 0.0 _d 0)
        ENDDO
       ENDDO

C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF(maskC(i,j,k,bi,bj) .GT. 0.0 _d 0) THEN

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
          dSigmaH = sqrt(dSigmaDx * dSigmaDx
     &                 + dSigmaDy * dSigmaDy)
          IF(dSigmaH .GT. 0.0 _d 0) THEN
           IF(abs(dSigmaR) .GT. dSigmaH*recipMaxSlope) THEN
            Sloc = dSigmaH / abs(dSigmaR)
           ELSE
            Sloc = abs(GM_maxSlope)
           ENDIF
           N2loc3D(i,j,k) = abs(gravity * recip_rhoConst * dSigmaR)
           SNloc3D(i,j,k) = Sloc * SQRT(N2loc3D(i,j,k))
          ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      IF ( GEOM_vert_struc ) THEN
       k=2
C     avoid division by zero
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         vert_struc_func(i,j,k) = MAX(N2loc3D(i,j,k), 
     &                                GEOM_vert_struc_min)
        ENDDO
       ENDDO
C     Cap the (N^2 / N^2_surf) between something (1 and 0.1 default)
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
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF(maskC(i,j,k,bi,bj) .GT. 0.0 _d 0) THEN
          N2loc = N2loc3D(i,j,k)
          SNloc = SNloc3D(i,j,k)

          SNloc_zint(i,j) = SNloc_zint(i,j)
     &                    + SNloc * drC(k) * hFacC(i,j,k,bi,bj)
     &                    * vert_struc_func(i,j,k)
          Nloc_zint(i,j)  = Nloc_zint(i,j) + SQRT(N2loc)
     &                    * drC(k) * hFacC(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     work out the implied long Rossby phase speeds at T pts
C     average it onto U points later
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C         only compute the Rossby phase speed if deep enough
        IF (depthC(i,j) .GT. 300.0 _d 0) THEN
         c1(i,j) = min(10.0 _d 0, Nloc_zint(i,j) / PI)
     &              * maskC(i,j,kSurf,bi,bj)
        ELSE
         c1(i,j) = 0.0 _d 0
        ENDIF
        IF (usingCartesianGrid) THEN
         c_ros(i,j) = c1(i,j) * c1(i,j) * beta / (f0 * f0)
        ELSE IF (usingSphericalPolarGrid) THEN
         c_ros(i,j) = c1(i,j) ** 2  * fCoriCos(i,j,bi,bj)
     &              / ( rSphere * fCori(i,j,bi,bj) ** 2 )
        ELSE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C switch off wave advection for now in curvilinear grid options
C TODO: better to make it be aware of rotations
         c_ros(i,j) = 0.0 _d 0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        ENDIF
        c_ros(i,j) = -min( c1(i,j) / 3.0 _d 0, c_ros(i,j) )
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- compute kgm as per GEOMETRIC in horizontal, then extend
C-- vertically by structure function if required
C-- do other tapering things here

C   TODO: cap GEOM_ene from below?

C     1. compute the 2d k_GEOM first
C        bound the denominator from below (choice following NEMO)

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C     for cubed sphere setups, depthC can be zero in the halo, so we
C     include it in the masking
        IF (maskC(i,j,kSurf,bi,bj)*depthC(i,j) .GT. 0.0 _d 0) THEN
         GEOMK(i,j,kSurf,bi,bj) = GEOM_alpha * GEOM_ene(i,j,bi,bj)
     &        / MAX( SNloc_zint(i,j), 1. _d -7 * depthC(i,j) )
        ENDIF
       ENDDO
      ENDDO

C     2. cap k_GEOM from above! (stop it being too big)

      IF (GEOM_alpha .NE. 0.0 _d 0) THEN
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         GEOMK(i,j,kSurf,bi,bj) =
     &        MIN(GEOMK(i,j,kSurf,bi,bj), GEOM_maxVal_K)
        ENDDO
       ENDDO
      ENDIF

C     3. taper it according to depth to kill k_GEOM in shallow regions

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        GEOMK(i,j,kSurf,bi,bj) = GEOMK(i,j,kSurf,bi,bj)
     &       * depth_taper(i,j) * maskC(i,j,kSurf,bi,bj)
       ENDDO
      ENDDO

C     4. extend the (tapered) k_GEOM in depth

      DO k=Nr,2,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         GEOMK(i,j,k,bi,bj) = vert_struc_func(i,j,k)
     &                      * GEOMK(i,j,kSurf,bi,bj)
     &                      * maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     5. cap k_GEOM from below (do not strictly need this)

      IF (GEOM_alpha .NE. 0.0 _d 0) THEN
       DO k=1,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          GEOMK(i,j,k,bi,bj) = MAX(GEOMK(i,j,k,bi,bj),GEOM_minVal_K)
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
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         IF(maskC(i,j,k,bi,bj) .GT. 0.0 _d 0) THEN
          SNloc = SNloc3D(i,j,k)
          trd_ene_gen(i,j) = trd_ene_gen(i,j)
     &                     + GEOMK(i,j,k,bi,bj)
     &                     * SNloc * SNloc
     &                     * drC(k) * hFacC(i,j,k,bi,bj)
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
        ene_lap_x(i,j)   = 0.0 _d 0
        ene_lap_y(i,j)   = 0.0 _d 0
       ENDDO
      ENDDO

CCCCCCCCCCCCCCCCCCCCCCCCCC
C     TODO: note for the 1-pixel energy test for advection,
C
C           1. re-define GEOM_ene(idum,jdum,bi,bj), such as below
C           2. make ene_rhs_now(i,j) = -trd_ene_adv(i,j) or the wav one
C              (so everything else still calculated, but only advection does anything)

!      if (energy_init .EQ. 1) then
!        GEOM_ene(:, :, bi, bj) = 0.0 _d 0
!        GEOM_ene(10, 5, bi, bj) = 1.0 _d 0
!        GEOM_ene(5, 25, bi, bj) = 1.0 _d 0
!        GEOM_ene(30, 17, bi, bj) = 1.0 _d 0
!      endif
CCCCCCCCCCCCCCCCCCCCCCCCCC

C ----  form the energy fluxes

C       first: form the depth-AVG velocity
      DO k=Nr,2,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         U_depth_avg(i, j) = U_depth_avg(i,j)
     &                     + uVel(i,j,k,bi,bj) * drF(k)
     &                     * hFacW(i,j,k,bi,bj)
     &                     * recip_depthW(i,j)
         V_depth_avg(i, j) = V_depth_avg(i,j)
     &                     + vVel(i,j,k,bi,bj) * drF(k)
     &                     * hFacS(i,j,k,bi,bj)
     &                     * recip_depthS(i,j)
        ENDDO
       ENDDO
      ENDDO

C       second: form the tendencies
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
        IF ( maskC(i-1,j,kSurf,bi,bj)*dxG(i-1,j,bi,bj) .GT. 0.0 _d 0
     & .AND. maskC(i  ,j,kSurf,bi,bj)*dxG(i  ,j,bi,bj) .GT. 0.0 _d 0
     &       ) THEN
C ---- second ordered centre difference
!              ene_adv_x(i,j) = -U_depth_avg(i,j)
!     &          * (dxG(i-1,j,bi,bj) * GEOM_ene(i  ,j,bi,bj)
!     &          +  dxG(i  ,j,bi,bj) * GEOM_ene(i-1,j,bi,bj))
!     &          / (dxG(i  ,j,bi,bj) + dxG(i-1,j,bi,bj))
C ---- 1st order upwinding
         fp_ui = U_depth_avg(i,j) + ABS( U_depth_avg(i,j) )
         fm_ui = U_depth_avg(i,j) - ABS( U_depth_avg(i,j) )
         ene_adv_x(i,j) = 0.5 * (fm_ui * GEOM_ene(i  ,j,bi,bj)
     &                        +  fp_ui * GEOM_ene(i-1,j,bi,bj))
C ---- 1st order upwinding wave stuff
         c_ros_u = 0.5 * ( c_ros(i-1,j) + c_ros(i,j) )
         fp_ui = c_ros_u + ABS( c_ros_u )
         fm_ui = c_ros_u - ABS( c_ros_u )
         ene_wav_x(i,j) = 0.5 * (fm_ui * GEOM_ene(i  ,j,bi,bj)
     &                        +  fp_ui * GEOM_ene(i-1,j,bi,bj))
         ene_lap_x(i,j) = GEOM_ene_kappa * (2.0 _d 0)
     &        * (GEOM_ene(i,j,bi,bj) - GEOM_ene(i-1,j,bi,bj))
     &        / (dxG(i,j,bi,bj) + dxG(i-1,j,bi,bj))
        ENDIF
       ENDDO
      ENDDO

      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx
        IF ( maskC(i,j-1,kSurf,bi,bj)*dyG(i,j-1,bi,bj) .GT. 0.0 _d 0
     & .AND. maskC(i,j  ,kSurf,bi,bj)*dyG(i,j  ,bi,bj) .GT. 0.0 _d 0
     &       ) THEN
C ---- second ordered centre difference
!              ene_adv_y(i, j) = -V_depth_avg(i,j)
!     &          * (dyG(i,j-1,bi,bj) * GEOM_ene(i,j  ,bi,bj)
!     &          +  dyG(i,j  ,bi,bj) * GEOM_ene(i,j-1,bi,bj))
!     &          / (dyG(i,j  ,bi,bj) + dyG(i,j-1,bi,bj))
C ---- 1st order upwinding
         fp_ui = V_depth_avg(i,j) + ABS( V_depth_avg(i,j) )
         fm_ui = V_depth_avg(i,j) - ABS( V_depth_avg(i,j) )
         ene_adv_y(i,j) = 0.5 * (fm_ui * GEOM_ene(i,j  ,bi,bj)
     &                        +  fp_ui * GEOM_ene(i,j-1,bi,bj))
         ene_lap_y(i, j) = GEOM_ene_kappa * (2.0 _d 0)
     &        * (GEOM_ene(i,j,bi,bj) - GEOM_ene(i,j-1,bi,bj))
     &        / (dyG(i,j,bi,bj) + dyG(i,j-1,bi,bj))
        ENDIF
       ENDDO
      ENDDO

      DO j=1-OLy+1,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
        trd_ene_dis(i,j) = - GEOM_lmbda * GEOM_ene(i,j,bi,bj)
        trd_ene_adv(i,j) =
     &           (ene_adv_x(i+1,j) - ene_adv_x(i,j))
     &           * recip_dxG(i,j,bi,bj)
     &          +(ene_adv_y(i,j+1) - ene_adv_y(i,j))
     &           * recip_dyG(i,j,bi,bj)
C        add in a bit to take into account linear free surface
c         IF(implicitFreeSurface) THEN
c          IF(maskC(i,j,kSurf,bi,bj) .GT. 0.0 _d 0) THEN
c           trd_ene_adv(i,j) = wVel(i,j,kSurf,bi,bj) * GEOM_ene(i,j)
c     &                         / depthC(i,j)
c          ENDIF
c         ENDIF
        trd_ene_wav(i,j) = -(ene_wav_x(i+1,j) - ene_wav_x(i,j))
     &                      * recip_dxG(i,j,bi,bj)
        trd_ene_lap(i,j) =
     &           (ene_lap_x(i+1,j) - ene_lap_x(i,j))
     &           * recip_dxG(i,j,bi,bj)
     &          +(ene_lap_y(i,j+1) - ene_lap_y(i,j))
     &           * recip_dyG(i,j,bi,bj)
C           form the RHS
        ene_rhs_now(i,j) = trd_ene_gen(i,j)
     &                   + trd_ene_dis(i,j)
     &                   - trd_ene_adv(i,j)
     &                   + trd_ene_wav(i,j)
     &                   + trd_ene_lap(i,j)
!        ene_rhs_now(i,j) = - trd_ene_adv(i,j)

       ENDDO
      ENDDO

C     At this point, the new ene_rhs_now is defined and correctly
C     computed for all i,j except for 1-OLx/y and sNx/y+Olx/y, and we do
C     not need any exchange. We od need to exchange the solution GEOM_ene
C     in S/R GMREDI_DO_EXCH.

C     third: time stepping
      IF (energy_init .EQ. 0 ) THEN
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         GEOM_ene(i,j,bi,bj) = GEOM_ene(i,j,bi,bj)
     &            + deltaTloc * ene_rhs_now(i,j)
        ENDDO
       ENDDO
       energy_init = 1
      ELSEIF ( energy_init .EQ. 1 ) THEN
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         GEOM_ene(i,j,bi,bj) = GEOM_ene(i,j,bi,bj)
     &             + deltaTloc * (
     &             +(3.0 _d 0 / 2.0 _d 0) * ene_rhs_now(i,j)
     &             -(1.0 _d 0 / 2.0 _d 0) * ene_rhs_nm1(i,j,bi,bj)
     &        )
        ENDDO
       ENDDO
       energy_init = 2
      ELSE
       DO j=1-OLy+1,sNy+OLy-1
        DO i=1-OLx+1,sNx+OLx-1
         GEOM_ene(i,j,bi,bj) = GEOM_ene(i,j,bi,bj)
     &             + deltaTloc * (
     &             +(23.0 _d 0 / 12.0 _d 0) * ene_rhs_now(i,j)
     &             -( 4.0 _d 0 /  3.0 _d 0) * ene_rhs_nm1(i,j,bi,bj)
     &             +( 5.0 _d 0 / 12.0 _d 0) * ene_rhs_nm2(i,j,bi,bj)
     &                        )
        ENDDO
       ENDDO
      ENDIF
C ----------------------------------------------
C ---- end advection of energy -----------------
C ----------------------------------------------

C       update rhs fields for the next time step
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        ene_rhs_nm2(i,j,bi,bj) = ene_rhs_nm1(i,j,bi,bj)
        ene_rhs_nm1(i,j,bi,bj) = ene_rhs_now(i,j)
       ENDDO
      ENDDO


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   diagnostics and restarts

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
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
       CALL DIAGNOSTICS_FILL(c_ros,
     &                       'GEOMcros', 0,1,2,bi,bj,myThid)
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_GEOM_VARIABLE_K */

      RETURN
      END
