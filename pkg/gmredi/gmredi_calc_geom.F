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
C  GEOMK        :: GEOMETRIC kgm (GEOMETRIC          in horizontal, 
C                             structure function in vertical)
C  GEOM_ene     :: GEOMETRIC parameterised energy
C  GEOM_ene_old ::     -          -          -
C              not really an output, passed around as common block

C !LOCAL VARIABLES: ====================================================
C     TODO: add commnents of variables
C     TODO: have some aux variables for checking purposes, leaving in for now
C           but delete it later when having production code
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

      INTEGER i,j,k
      _RL dSigmaDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dSigmaDr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      
      _RL dSigmaH, dSigmaR
      _RL N2loc, SNloc, Sloc

      _RL recipMaxSlope
      _RL c_ros_u, fm_ui, fp_ui

C     metric variables, computed once only
      logical, save :: metric_init = .false.
      _RL, save :: surface_area, domain_vol, max_depth
      _RL, save :: depthW(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL, save :: depthS(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL, save :: depthC(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL, save :: depth_taper(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      
C     general variables, overwritten every time step
      _RL :: energy_int, energy_avg

      _RL vert_struc_func(1-OLx:sNx+OLx, 1-OLy:sNy+OLy, Nr)
      
      _RL SNloc_zint(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL  Nloc_zint(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL trd_ene_gen(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL trd_ene_dis(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL trd_ene_adv(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL trd_ene_wav(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL trd_ene_lap(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      
      _RL U_depth_avg(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL V_depth_avg(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      
      _RL c1   (1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL c_ros(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      
      _RL ene_adv_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL ene_adv_y(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL ene_wav_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL ene_lap_x(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)
      _RL ene_lap_y(1-OLx:sNx+OLx, 1-OLy:sNy+OLy)

CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      recipMaxSlope = 0. _d 0
      IF ( GM_maxSlope.GT.0. _d 0 ) THEN
        recipMaxSlope = 1. _d 0 / GM_maxSlope
      ENDIF

C--   TODO: do these not already exist and get passed round?
C--   initialise some variables for calculations, only does it once
      IF( .NOT. metric_init ) THEN

C       surface area
        surface_area = 0.0 _d 0
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            surface_area = surface_area 
     &                   + dxG(i,j,bi,bj) * dyG(i,j,bi,bj)
     &                   * maskC(i,j,1,bi,bj)
          ENDDO
        ENDDO
        
C       domain volume
        domain_vol = 0.0 _d 0
        max_depth = 0.0 _d 0
        DO k=1,Nr
          DO j=1,sNy
            DO i=1,sNx
              domain_vol = domain_vol 
     &                   + dxG(i,j,bi,bj) * dyG(i,j,bi,bj)
     &                   * drF(k) * hFacC(i,j,k,bi,bj)
            ENDDO
          ENDDO
          max_depth = max_depth + drF(k)
        ENDDO

C       depths for doing depth-averaging
        depthW = 0.0 _d 0
        depthS = 0.0 _d 0
        depthC = 0.0 _d 0

        DO k=1,Nr
          DO j=1-OLy, sNy+OLy
            DO i=1-OLx, sNx+OLx
              depthW(i,j) = depthW(i,j) + drF(k) * hFacW(i,j,k,bi,bj)
            ENDDO
          ENDDO
          
          DO j=1-OLy, sNy+OLy
            DO i=1-OLx, sNx+OLx
              depthS(i,j) = depthS(i,j) + drF(k) * hFacS(i,j,k,bi,bj)
            ENDDO
          ENDDO

          DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              depthC(i,j) = depthC(i,j) + drF(k) * hFacC(i,j,k,bi,bj)
            ENDDO
          ENDDO
        ENDDO

C       temporary variable for depth tapering later
        depth_taper = 0.0 _d 0
        
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            depth_taper(i,j) = (0.5 _d 0 *
     &          (1.0 _d 0 - TANH( 
     &                (-depthC(i,j) + 800.0 _d 0) / 300.0 _d 0
     &                          )
     &          )
     &                         ) ** 2
          ENDDO
        ENDDO
        
C       initialise energy or not
        IF( (GEOM_pickup_read_mdsio .OR. GEOM_pickup_read_mnc) ) THEN
          WRITE(*,'(2A,I4)') 'GMREDI_CALC_GEOM: ',
     &        'read pickup option on, no init of GEOM_ene_old'
          IF( nIter0.EQ.0 ) THEN
            WRITE(*,'(2A,I4)') 'GMREDI_CALC_GEOM: ',
     &         'WARNING! nIter0 = 0, force energy initialisation...'
            energy_init = 0
          ENDIF
        ELSE
          WRITE(*,'(2A,I4)') 'GMREDI_CALC_GEOM: ',
     &        'initialising GEOM_ene_old'
          energy_init = 0
        ENDIF

        IF( energy_init == 0 ) THEN
          IF(ene_local) THEN
            DO j=1-OLy, sNy+OLy
              DO i=1-OLx, sNx+OLx
                GEOM_ene_old(i,j,bi,bj) = ene_init * max_depth
     &                                  * maskC(i,j,1,bi,bj)
              ENDDO
            ENDDO
          ELSE
            DO j=1-OLy, sNy+OLy
              DO i=1-OLx, sNx+OLx
                GEOM_ene_old(i,j,bi,bj) = ene_init * domain_vol
     &                                  * maskC(i,j,1,bi,bj)
              ENDDO
            ENDDO
         ENDIF
        ENDIF
        IF (usingCurvilinearGrid) THEN
          WRITE(*,'(2A,I4)') 'GMREDI_CALC_GEOM: ',
     &         'WARNING! grid opt for wav adv not supported...'
        ENDIF
       
        metric_init = .true.
        
      ENDIF
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- TODO: potentially move the energy calculation into here given
C         the dSigmaDx calculations are repeated
C   TODO: should the calculations be based on the tapered slopes?
C         here it is just the raw stuff
C         if being moved then this routine might want to be after
C         the SLOPE_LIMIT routine

C     initialisations, put this in a loop if need be
      SNloc_zint = 0.0 _d 0
      Nloc_zint  = 0.0 _d 0
      vert_struc_func = 1.0 _d 0

C-- 1st k loop : compute vertical structure to be used later
      DO k=Nr,2,-1
      
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            dSigmaDx(i,j) = op25 * (sigmaX(i+1,j,k-1) + sigmaX(i,j,k-1)
     &                           +  sigmaX(i+1,j,k  ) + sigmaX(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
            dSigmaDy(i,j) = op25 * (sigmaY(i,j+1,k-1) + sigmaY(i,j,k-1)
     &                           +  sigmaY(i,j+1,k  ) + sigmaY(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
          ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dSigmaDr(i,j) = min(sigmaR(i,j,k), 0.0 _d 0)
          ENDDO
        ENDDO
       
C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
        DO j=1-OLy+1, sNy+OLy-1
          DO i=1-OLx+1, sNx+OLx-1
            IF(maskC(i,j,k,bi,bj) /= 0.0 _d 0) THEN

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
              dSigmaH = sqrt(dSigmaDx(i,j) * dSigmaDx(i,j)
     &                     + dSigmaDy(i,j) * dSigmaDy(i,j))
              IF(dSigmaH > 0.0 _d 0) THEN
                IF(abs(dSigmaR) > dSigmaH*recipMaxSlope) THEN
                  Sloc = dSigmaH / abs(dSigmaR)
                ELSE
                  Sloc = abs(GM_maxSlope)
                ENDIF
                N2loc = abs(gravity * recip_rhoConst * dSigmaR)
                SNloc = Sloc * sqrt(N2loc)
              ELSE
                SNloc = 0.0 _d 0
              ENDIF
              IF(vert_struc) THEN
                vert_struc_func(i,j,k) = N2loc
              ELSE
                vert_struc_func(i,j,k) = 1.0 _d 0
              ENDIF
            ENDIF
          ENDDO
        ENDDO   
      ENDDO

C-- 2nd k loop : compute the rest of the GEOMETRIC stuff
      DO k=Nr,2,-1
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            dSigmaDx(i,j) = op25 * (sigmaX(i+1,j,k-1) + sigmaX(i,j,k-1)
     &                           +  sigmaX(i+1,j,k  ) + sigmaX(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
            dSigmaDy(i,j) = op25 * (sigmaY(i,j+1,k-1) + sigmaY(i,j,k-1)
     &                           +  sigmaY(i,j+1,k  ) + sigmaY(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
          ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dSigmaDr(i,j) = min(sigmaR(i,j,k), 0.0 _d 0)
          ENDDO
        ENDDO
       
C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
        DO j=1-OLy+1, sNy+OLy-1
          DO i=1-OLx+1, sNx+OLx-1
            IF(maskC(i,j,k,bi,bj) /= 0.0 _d 0) THEN
C      Compute -N^2 rho_0 / g via averaging, on vertical interfaces
              dSigmaR = (dSigmaDr(i,j) * 4.0 _d 0
     &                + maskC(i-1,j,k,bi,bj)*dSigmaDr(i-1,j)
     &                + maskC(i+1,j,k,bi,bj)*dSigmaDr(i+1,j)
     &                + maskC(i,j-1,k,bi,bj)*dSigmaDr(i,j-1)
     &                + maskC(i,j+1,k,bi,bj)*dSigmaDr(i,j+1)
     &                ) / (4.0 _d 0
     &                  + maskC(i-1,j,k,bi,bj)
     &                  + maskC(i+1,j,k,bi,bj)
     &                  + maskC(i,j-1,k,bi,bj)
     &                  + maskC(i,j+1,k,bi,bj)
     &                    )
C      Compute M^2 rho_0 / g on vertical interfaces
              dSigmaH = sqrt(dSigmaDx(i,j) * dSigmaDx(i,j)
     &                     + dSigmaDy(i,j) * dSigmaDy(i,j))
              IF(dSigmaH > 0.0 _d 0) THEN
                IF(abs(dSigmaR) > dSigmaH*recipMaxSlope) THEN
                  Sloc = dSigmaH / abs(dSigmaR)
                ELSE
                  Sloc = abs(GM_maxSlope)
                ENDIF
                  N2loc = abs(gravity * recip_rhoConst * dSigmaR)
                  SNloc = Sloc * sqrt(N2loc)
              ELSE
                SNloc = 0.0 _d 0
              ENDIF
              
              IF(vert_struc) THEN
C      Cap the (N^2 / N^2_surf) between something (1 and 0.1 default)
                vert_struc_func(i,j,k) = 
     &           max(min(vert_struc_max, 
     &                   vert_struc_func(i,j,k)/vert_struc_func(i,j,2)),
     &               vert_struc_min)
              ELSE
                vert_struc_func(i,j,k) = 1.0 _d 0
              ENDIF
              SNloc_zint(i,j) = SNloc_zint(i,j) 
     &                        + SNloc * drC(k) * hFacC(i,j,k,bi,bj)
     &                        * vert_struc_func(i,j,k)
              Nloc_zint(i,j)  = Nloc_zint(i,j) + sqrt(N2loc)
     &                        * drC(k) * hFacC(i,j,k,bi,bj)
            ENDIF
          ENDDO
        ENDDO   
      ENDDO
      
      IF(.not. ene_local) THEN
C       Horizontal integral
        SNloc = 0.0 _d 0
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            SNLoc = SNLoc + SNloc_zint(i,j) 
     &                    * dxG(i,j,bi,bj) * dyG(i,j,bi,bj)
     &                    * maskC(i,j,1,bi,bj)
          ENDDO
        ENDDO
        SNloc_zint = SNloc / surface_area
      ENDIF
      
C     work out the implied long Rossby phase speeds at T pts
C     average it onto U points later
      DO j=1-OLy, sNy+OLy
        DO i=1-OLx, sNx+OLx
C         only compute the Rossby phase speed if deep enough
          IF (depthC(i,j) > 300.0 _d 0) THEN
            c1(i,j) = min(10.0 _d 0, Nloc_zint(i,j) / PI) 
     &              * maskC(i,j,1,bi,bj)
          ELSE
            c1(i,j) = 0.0 _d 0
          ENDIF
          IF (usingCartesianGrid) THEN
            c_ros(i,j) = c1(i,j) * c1(i,j) * beta / (f0 * f0)
          ELSE IF (usingSphericalPolarGrid) THEN
            c_ros(i,j) = c1(i,j) ** 2  * fCoriCos(i,j,bi,bj)
     &                 / ( rSphere * fCori(i,j,bi,bj) ** 2 )
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

C   TODO: cap GEOM_ene_old from below?

C     1. compute the 2d k_GEOM first
C        bound the denominator from below (choice following NEMO)
     
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          IF(maskC(i,j,k,bi,bj) /= 0.0 _d 0) THEN
            GEOMK(i,j,1,bi,bj) = GEOM_alpha
     &                         * GEOM_ene_old(i,j,bi,bj) 
     &                         / max(        SNloc_zint(i,j), 
     &                               1. _d -7 * depthC(i,j)
     &                               )
          ENDIF
          IF(.not. ene_local) THEN
            GEOMK(i,j,1,bi,bj) = GEOMK(i,j,1,bi,bj) / surface_area
          ENDIF
        ENDDO
      ENDDO
      
C     2. cap k_GEOM from above! (stop it being too big)
       
      IF(GEOM_alpha /= 0.0 _d 0) THEN
        GEOMK(:,:,1,bi,bj) = min(GEOMK(:,:,1,bi,bj), GEOM_maxVal_K)
      ENDIF
      
C     3. taper it according to depth to kill k_GEOM in shallow regions
     
      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          GEOMK(i,j,1,bi,bj) = GEOMK(i,j,1,bi,bj) * depth_taper(i,j)
     &                       * maskC(i,j,1,bi,bj)
        ENDDO
      ENDDO
      
C     4. extend the (tapered) k_GEOM in depth

      DO k=Nr,2,-1
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            GEOMK(i,j,k,bi,bj) = vert_struc_func(i,j,k) 
     &                         * GEOMK(i,j,1,bi,bj)
     &                         * maskC(i,j,k,bi,bj)
          ENDDO
        ENDDO
      ENDDO
      
C     5. cap k_GEOM from below (don't strictly need this)
       
      IF(GEOM_alpha /= 0.0 _d 0) THEN
        GEOMK(:,:,:,bi,bj) = max(GEOMK(:,:,:,bi,bj),GEOM_minVal_K)
      ENDIF
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   time step the energy
C     allocate and initialise
      trd_ene_gen = 0.0 _d 0

C     loop over k
      DO k=Nr,2,-1
        DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
            dSigmaDx(i,j) = op25 * (sigmaX(i+1,j,k-1) + sigmaX(i,j,k-1)
     &                           +  sigmaX(i+1,j,k  ) + sigmaX(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
            dSigmaDy(i,j) = op25 * (sigmaY(i,j+1,k-1) + sigmaY(i,j,k-1)
     &                           +  sigmaY(i,j+1,k  ) + sigmaY(i,j,k  )
     &                         ) *  maskC(i,j,k,bi,bj)
          ENDDO
        ENDDO
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            dSigmaDr(i,j) = min(sigmaR(i,j,k), 0.0 _d 0)
          ENDDO
        ENDDO
       
C      NOTE: Ignores boundary cells (convenient with the 5-point filter used)
        DO j=1-OLy+1, sNy+OLy-1
          DO i=1-OLx+1, sNx+OLx-1
            IF(maskC(i,j,k,bi,bj) /= 0.0 _d 0) THEN

C      Compute -N^2 rho_0 / g via averaging, on vertical interfaces
              dSigmaR = (dSigmaDr(i,j) * 4.0 _d 0
     &                + maskC(i-1,j,k,bi,bj)*dSigmaDr(i-1,j)
     &                + maskC(i+1,j,k,bi,bj)*dSigmaDr(i+1,j)
     &                + maskC(i,j-1,k,bi,bj)*dSigmaDr(i,j-1)
     &                + maskC(i,j+1,k,bi,bj)*dSigmaDr(i,j+1)
     &                  ) / (4.0 _d 0
     &                    + maskC(i-1,j,k,bi,bj)
     &                    + maskC(i+1,j,k,bi,bj)
     &                    + maskC(i,j-1,k,bi,bj)
     &                    + maskC(i,j+1,k,bi,bj)
     &                      )
C      Compute M^2 rho_0 / g on vertical interfaces
              dSigmaH = sqrt(dSigmaDx(i,j) * dSigmaDx(i,j)
     &                     + dSigmaDy(i,j) * dSigmaDy(i,j))
              IF(dSigmaH > 0.0 _d 0) THEN
                IF(abs(dSigmaR) > dSigmaH*recipMaxSlope) THEN
                  Sloc = dSigmaH / abs(dSigmaR)
                ELSE
                  Sloc = abs(GM_maxSlope)
                ENDIF
                N2loc = abs(gravity * recip_rhoConst * dSigmaR)
                SNloc = Sloc * sqrt(N2loc)
              ELSE
                SNloc = 0.0 _d 0
              ENDIF
              trd_ene_gen(i,j) = trd_ene_gen(i,j)
     &                         + GEOMK(i,j,k,bi,bj) 
     &                         * SNloc * SNloc 
     &                         * drC(k) * hFacC(i,j,k,bi,bj)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C ---------------------------------------------- 
C ---- start advection of energy ---------------
C ----------------------------------------------
      
      trd_ene_dis = 0.0 _d 0
      trd_ene_adv = 0.0 _d 0
      trd_ene_wav = 0.0 _d 0
      trd_ene_lap = 0.0 _d 0

      U_depth_avg = 0.0 _d 0
      V_depth_avg = 0.0 _d 0

      ene_adv_x = 0.0 _d 0
      ene_adv_y = 0.0 _d 0
      ene_wav_x = 0.0 _d 0
      ene_lap_x = 0.0 _d 0
      ene_lap_y = 0.0 _d 0
 
CCCCCCCCCCCCCCCCCCCCCCCCCC     
C     TODO: note for the 1-pixel energy test for advection,
C
C           1. re-define GEOM_ene_old(idum,jdum,bi,bj), such as below
C           2. make ene_rhs_now(i,j,bi,bj) = -trd_ene_adv(i,j) or the wav one
C              (so everything else still calculated, but only advection does anything)

!      if (energy_init .EQ. 1) then
!        GEOM_ene_old(:, :, bi, bj) = 0.0 _d 0
!        GEOM_ene_old(10, 5, bi, bj) = 1.0 _d 0
!        GEOM_ene_old(5, 25, bi, bj) = 1.0 _d 0
!        GEOM_ene_old(30, 17, bi, bj) = 1.0 _d 0
!      endif
CCCCCCCCCCCCCCCCCCCCCCCCCC
      
      IF(ene_local) THEN
       
C       update energy field  
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx     
            ene_rhs_nm2(i,j,bi,bj) = ene_rhs_nm1(i,j,bi,bj)
            ene_rhs_nm1(i,j,bi,bj) = ene_rhs_now(i,j,bi,bj)
          ENDDO
        ENDDO

C ----  form the energy fluxes

C       first: form the depth-AVG velocity 
        DO k=Nr,2,-1     
          DO j=1-OLy, sNy+OLy
            DO i=1-OLx+1, sNx+OLx
              IF(maskW(i,j,k,bi,bj) /= 0.0 _d 0) THEN
                U_depth_avg(i, j) = U_depth_avg(i,j)
     &                            + uVel(i,j,k,bi,bj) * drF(k)
     &                            * hFacW(i,j,k,bi,bj)
     &                            / depthW(i,j)
              ENDIF
            ENDDO
          ENDDO      
          DO j=1-OLy, sNy+OLy
            DO i=1-OLx, sNx+OLx
              IF(maskS(i,j,k,bi,bj) /= 0.0 _d 0) THEN
                V_depth_avg(i, j) = V_depth_avg(i,j)
     &                            + vVel(i,j,k,bi,bj) * drF(k)
     &                            * hFacS(i,j,k,bi,bj)
     &                            / depthS(i,j)
              ENDIF
            ENDDO
          ENDDO
        ENDDO
       
C       second: form the tendencies
        DO j=1-OLy, sNy+OLy
          DO i=1-OLx+1, sNx+OLx
            IF(    maskC(i-1,j,1,bi,bj) /= 0.0 _d 0
     &       .AND. maskC(i  ,j,1,bi,bj) /= 0.0 _d 0) THEN
C ---- second ordered centre difference
!              ene_adv_x(i,j) = -U_depth_avg(i,j)
!     &          * (dxG(i-1,j,bi,bj) * GEOM_ene_old(i  ,j,bi,bj)
!     &          +  dxG(i  ,j,bi,bj) * GEOM_ene_old(i-1,j,bi,bj))
!     &          / (dxG(i  ,j,bi,bj) + dxG(i-1,j,bi,bj))
C ---- 1st order upwinding
              fp_ui = U_depth_avg(i,j) + ABS( U_depth_avg(i,j) )
              fm_ui = U_depth_avg(i,j) - ABS( U_depth_avg(i,j) )
              ene_adv_x(i,j) = 0.5 * (fm_ui * GEOM_ene_old(i  ,j,bi,bj)
     &                             +  fp_ui * GEOM_ene_old(i-1,j,bi,bj))
C ---- 1st order upwinding wave stuff
              c_ros_u = 0.5 * ( c_ros(i-1,j) + c_ros(i,j) )
              fp_ui = c_ros_u + ABS( c_ros_u )
              fm_ui = c_ros_u - ABS( c_ros_u )
              ene_wav_x(i,j) = 0.5 * (fm_ui * GEOM_ene_old(i  ,j,bi,bj)
     &                             +  fp_ui * GEOM_ene_old(i-1,j,bi,bj))
              
              ene_lap_x(i,j) = ene_kappa * (2.0 _d 0)
     &          * (GEOM_ene_old(i,j,bi,bj) - GEOM_ene_old(i-1,j,bi,bj))
     &          / (dxG(i,j,bi,bj) + dxG(i-1,j,bi,bj))
            ENDIF
          ENDDO
        ENDDO
       
        DO j=1-OLy+1, sNy+OLy
          DO i=1-OLx, sNx+OLx
            IF(    maskC(i,j-1,1,bi,bj) /= 0.0 _d 0
     &       .AND. maskC(i,j  ,1,bi,bj) /= 0.0 _d 0) THEN
C ---- second ordered centre difference  
!              ene_adv_y(i, j) = -V_depth_avg(i,j)
!     &          * (dyG(i,j-1,bi,bj) * GEOM_ene_old(i,j  ,bi,bj)
!     &          +  dyG(i,j  ,bi,bj) * GEOM_ene_old(i,j-1,bi,bj))
!     &          / (dyG(i,j  ,bi,bj) + dyG(i,j-1,bi,bj)) 
C ---- 1st order upwinding
              fp_ui = V_depth_avg(i,j) + ABS( V_depth_avg(i,j) )
              fm_ui = V_depth_avg(i,j) - ABS( V_depth_avg(i,j) )
              ene_adv_y(i,j) = 0.5 * (fm_ui * GEOM_ene_old(i,j  ,bi,bj)
     &                             +  fp_ui * GEOM_ene_old(i,j-1,bi,bj))
              ene_lap_y(i, j) = ene_kappa * (2.0 _d 0)
     &          * (GEOM_ene_old(i,j,bi,bj) - GEOM_ene_old(i,j-1,bi,bj))
     &          / (dyG(i,j,bi,bj) + dyG(i,j-1,bi,bj))
            ENDIF
          ENDDO
        ENDDO

        DO j=1-OLy+1, sNy+OLy-1
          DO i=1-OLx+1, sNx+OLx-1
            trd_ene_dis(i,j) = - GEOM_lmbda * GEOM_ene_old(i,j,bi,bj)
            trd_ene_adv(i,j) = 
     &           (ene_adv_x(i+1,j) - ene_adv_x(i,j)) / dxG(i,j,bi,bj)
     &          +(ene_adv_y(i,j+1) - ene_adv_y(i,j)) / dyG(i,j,bi,bj)
C        add in a bit to take into account linear free surface         
c         IF(implicitFreeSurface) THEN
c          IF(maskC(i,j,1,bi,bj) /= 0.0 _d 0) THEN
c           trd_ene_adv(i,j) = wVel(i,j,1,bi,bj) * GEOM_ene_old(i,j)
c     &                         / depthC(i,j) 
c          ENDIF
c         ENDIF
            trd_ene_wav(i,j) = -(ene_wav_x(i+1,j) - ene_wav_x(i,j)) 
     &                       / dxG(i,j,bi,bj)
            trd_ene_lap(i,j) = 
     &           (ene_lap_x(i+1,j) - ene_lap_x(i,j)) / dxG(i,j,bi,bj)
     &          +(ene_lap_y(i,j+1) - ene_lap_y(i,j)) / dyG(i,j,bi,bj)
C           form the RHS
            ene_rhs_now(i,j,bi,bj) = trd_ene_gen(i,j)
     &                             + trd_ene_dis(i,j)
     &                             - trd_ene_adv(i,j)
     &                             + trd_ene_wav(i,j)
     &                             + trd_ene_lap(i,j)
!            ene_rhs_now(i,j,bi,bj) = - trd_ene_adv(i,j)
     
          ENDDO
        ENDDO
        
C       call exchange to make sure transmission occurs in mpi runs
        _EXCH_XY_RL( ene_rhs_now , myThid )

C       third: time stepping
        select case(energy_init)
          case(0)
            DO j=1-OLy+1, sNy+OLy-1
              DO i=1-OLx+1, sNx+OLx-1
                GEOM_ene(i,j,bi,bj) = GEOM_ene_old(i,j,bi,bj)
     &                              + deltaT * ene_rhs_now(i,j,bi,bj)
              ENDDO
            ENDDO
            energy_init = 1
          case(1)
            DO j=1-OLy+1, sNy+OLy-1
              DO i=1-OLx+1, sNx+OLx-1
                GEOM_ene(i,j,bi,bj) = GEOM_ene_old(i,j,bi,bj) 
     &             + deltaT * (
     &             +(3.0 _d 0 / 2.0 _d 0) * ene_rhs_now(i,j,bi,bj)
     &             -(1.0 _d 0 / 2.0 _d 0) * ene_rhs_nm1(i,j,bi,bj)
     &                        )
              ENDDO
            ENDDO
            energy_init = 2
          case default
            DO j=1-OLy+1, sNy+OLy-1
              DO i=1-OLx+1, sNx+OLx-1
                GEOM_ene(i,j,bi,bj) = GEOM_ene_old(i,j,bi,bj) 
     &             + deltaT * (
     &             +(23.0 _d 0 / 12.0 _d 0) * ene_rhs_now(i,j,bi,bj)
     &             -( 4.0 _d 0 /  3.0 _d 0) * ene_rhs_nm1(i,j,bi,bj)
     &             +( 5.0 _d 0 / 12.0 _d 0) * ene_rhs_nm2(i,j,bi,bj)
     &                        )
              ENDDO
            ENDDO
            energy_init = 3
        end select
C ----------------------------------------------       
C ---- end advection of energy -----------------
C ---------------------------------------------- 
      ELSE  
C       Horizontally integrate and time step the energy

C       form integrand
        energy_int = 0.0 _d 0
        DO j=1-OLy, sNy+OLy
          DO i=1-OLx, sNx+OLx
            energy_int = energy_int +
     &                   trd_ene_gen(i,j) 
     &                 * dxG(i,j,bi,bj) * dyG(i,j,bi,bj)
     &                 * maskC(i,j,1,bi,bj)
          ENDDO
        ENDDO
C       time step and allocate it as a field with same entries
        DO j=1-OLy, sNy+OLy
          DO i=1-OLx, sNx+OLx
            GEOM_ene(i,j,bi,bj) = 
     &           (deltaT * energy_int + GEOM_ene_old(1,1,bi,bj))
     &           / (1.0 _d 0 + GEOM_lmbda * deltaT)
          ENDDO
        ENDDO
        energy_init = 1
        IF(minval(GEOM_ene(1:sNx,1:sNy,bi,bj)) 
     &    /= maxval(GEOM_ene(1:sNx,1:sNy,bi,bj))) THEN
          STOP "Non constant energy in domain integrated case"
        ENDIF
      ENDIF
      
      
      DO j=1-OLy, sNy+OLy
        DO i=1-OLx, sNx+OLx
          GEOM_ene_old(i,j,bi,bj) = GEOM_ene(i,j,bi,bj)
        ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   diagnostics and restarts

C--   on screen diagnostics
      IF(mod(myTime, monitorFreq) == 0. _d 0) THEN

C       estimate of domain AVERAGED energy 
C       (m2/s2, but done per tile via multiplication by the number of tiles)
        energy_avg = 0. _d 0
        IF(ene_local) THEN
          DO j=1, sNy
            DO i=1, sNx
              energy_avg = energy_avg + GEOM_ene(i,j,bi,bj)
     &                   * dxG(i,j,bi,bj) * dyG(i,j,bi,bj)
     &                   * maskC(i,j,1,bi,bj)
            ENDDO
          ENDDO
          energy_avg = energy_avg / domain_vol * (nPx * nPy)
        ELSE
          energy_avg = GEOM_ene(1,1,bi,bj) / domain_vol
        ENDIF

        write(*,'(a,3(x,a,f11.5,a))'), "GMREDI_CALC_GEOM: tile diag"
     &   , " t       = ", myTime / (3600 * 24 * 360), " yr, "
     &   , " kgm_max = ", maxval(GEOMK(1:sNx,1:sNy,1,bi,bj))
     &   ,                    " m2/s, "
     &   , " eE_avg  = ", energy_avg, " m2/s2 "
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
       CALL DIAGNOSTICS_FILL(GEOMK(:,:,1,bi,bj),
     &                       'GEOMkap0', 0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(vert_struc_func, 
     &                       'GEOMstru', 0,Nr,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(depthC,
     &                       'depthC  ', 0,1,0,1,1,myThid)
       CALL DIAGNOSTICS_FILL(depth_taper,
     &                       'd_taper ', 0,1,0,1,1,myThid)
       IF(ene_local) THEN
         CALL DIAGNOSTICS_FILL(trd_ene_gen, 
     &                         'GEOMEgen', 0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(trd_ene_dis, 
     &                         'GEOMEdis', 0,1,0,1,1,myThid)
C        advective trends are dE/dt + -(u - c) dot grad E
         CALL DIAGNOSTICS_FILL(trd_ene_adv, 
     &                         'GEOMEadv', 0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(trd_ene_wav,
     &                         'GEOMEwav', 0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(trd_ene_lap, 
     &                         'GEOMElap', 0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(c1, 
     &                         'GEOM_c1 ', 0,1,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(c_ros, 
     &                         'GEOMcros', 0,1,0,1,1,myThid)
       ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* GM_GEOM_VARIABLE_K */

      RETURN
      END
