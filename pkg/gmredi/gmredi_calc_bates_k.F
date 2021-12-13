#include "GMREDI_OPTIONS.h"

C     !ROUTINE: GMREDI_CALC_BATES_K
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_BATES_K(
     I                  iMin, iMax, jMin, jMax,
     I                  sigmaX, sigmaY, sigmaR,
     I                  bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_BATES_K
C     | o Calculates the 3D diffusivity as per Bates et al. (2014)
C     *==========================================================*
C     \ev

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj    :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: My Thread Id. number

      INTEGER iMin,iMax,jMin,jMax
      _RL sigmaX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL sigmaR(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef GM_BATES_K3D

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,kk,m,kp1

C     update_modes :: Whether to update the eigenmodes
      LOGICAL update_modes

C     surfk  :: index of the depth of the surface layer
C     kLow_C :: Local version of the index of deepest wet grid cell on tracer grid
C     kLow_U :: Local version of the index of deepest wet grid cell on U grid
C     kLow_V :: Local version of the index of deepest wet grid cell on V grid
      INTEGER surfk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kLow_C(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kLow_U(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER kLow_V(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     N2loc  :: local N**2
C     slope  :: local slope
C     Req    :: local equatorial deformation radius (m)
C     deltaH :: local thickness of Eady integration (m)
C     g_reciprho_sq :: (gravity*recip_rhoConst)**2
C     M4loc  :: local M**4
C     maxDRhoDz :: maximum value of d(rho)/dz (derived from GM_Bates_minN2)
C     sigx   :: local d(rho)/dx
C     sigy   :: local d(rho)/dy
C     sigz   :: local d(rho)/dz
C     hsurf  :: local surface layer depth
C     small  :: a small number (to avoid floating point exceptions)
      _RL N2loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL slope
      _RL slopeC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Req
      _RL deltaH(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL g_reciprho_sq
      _RL M4loc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL M4onN2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL maxDRhoDz
      _RL sigx, sigy, sigz
      _RL hsurf, mskp1
      _RL small

C     dfdy    :: gradient of the Coriolis paramater, df/dy, 1/(m*s)
C     dfdx    :: gradient of the Coriolis paramater, df/dx, 1/(m*s)
C     Rurms  :: a local mixing length used in calculation of urms (m)
C     RRhines :: The Rhines scale (m)
C     Rmix    :: Mixing length
C     N2      :: Square of the buoyancy frequency (1/s**2)
C     N2W     :: Square of the buoyancy frequency (1/s**2) averaged to west of grid cell
C     N2S     :: Square of the buoyancy frequency (1/s**2) averaged to south of grid cell
C     N       :: Buoyancy frequency, SQRT(N2)
C     BVint   :: The vertical integral of N (m/s)
C     ubar    :: Zonal velocity on a tracer point (m/s)
C     Ubaro   :: Barotropic velocity (m/s)
      _RL dfdy(   1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dfdx(   1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dummy(  1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Rurms(  1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RRhines(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Rmix(   1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL N2(     1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N2W(    1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N2S(    1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL N(      1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL BVint(  1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Ubaro(  1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL ubar(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

      _RL tmpU(  1-OLx:sNx+OLx,1-OLy:sNy+OLy )
      _RL tmpV(  1-OLx:sNx+OLx,1-OLy:sNy+OLy )
      _RL uFldX( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr )
      _RL vFldY( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr )

C     Rmid     :: Rossby radius (m)
C     KPV      :: Diffusivity (m**2/s)
C     Kdqdx    :: diffusivity multiplied by zonal PV gradient
C     Kdqdy    :: diffusivity multiplied by meridional PV gradient
C     SlopeX   :: isopycnal slope in x direction
C     SlopeY   :: isopycnal slope in y direction
C     dSigmaDx :: sigmaX averaged onto tracer grid
C     dSigmaDy :: sigmaY averaged onto tracer grid
C     tfluxX   :: thickness flux in x direction
C     tfluxY   :: thickness flux in y direction
C     fCoriU   :: Coriolis parameter averaged to U points
C     fCoriV   :: Coriolis parameter averaged to V points
C     coriU    :: As fCoriU, but limited
C     coriV    :: As fCoriV, but limited
C     surfkz   :: Depth of surface layer (in r units)
      _RL Rmid(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KPV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Kdqdy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Kdqdx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL SlopeX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL SlopeY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dSigmaDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL dSigmaDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tfluxX(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL tfluxY(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL coriU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL coriV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fCoriU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL fCoriV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfkz(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     centreX,centreY :: used for calculating averages at centre of cell
C     numerator,denominator :: of the renormalisation factor
C     uInt      :: column integral of u velocity (sum u*dz)
C     vInt      :: column integral of v velocity (sum v*dz)
C     KdqdxInt  :: column integral of K*dqdx (sum K*dqdx*dz)
C     KdqdyInt  :: column integral of K*dqdy (sum K*dqdy*dz)
C     uKdqdyInt :: column integral of u*K*dqdy (sum u*K*dqdy*dz)
C     vKdqdxInt :: column integral of v*K*dqdx (sum v*K*dqdx*dz)
C     uXiyInt   :: column integral of u*Xiy (sum u*Xiy*dz)
C     vXixInt   :: column integral of v*Xix (sum v*Xix*dz)
C     Renorm    :: renormalisation factor at the centre of a cell
C     RenormU   :: renormalisation factor at the western face of a cell
C     RenormV   :: renormalisation factor at the southern face of a cell
      _RL centreX, centreY
      _RL numerator, denominator
      _RL uInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KdqdxInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KdqdyInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uKdqdyInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vKdqdxInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uXiyInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vXixInt(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Renorm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RenormU(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL RenormV(1-OLx:sNx+OLx,1-OLy:sNy+OLy)

C     gradqx   :: Potential vorticity gradient in x direction
C     gradqy   :: Potential vorticity gradient in y direction
C     XimX     :: Vertical integral of phi_m*K*gradqx
C     XimY     :: Vertical integral of phi_m*K*gradqy
C     cDopp    :: Quasi-Doppler shifted long Rossby wave speed (m/s)
C     umc      :: ubar-c (m/s)
C     eady     :: Eady growth rate (1/s)
C     urms     :: the rms eddy velocity (m/s)
C     supp     :: The suppression factor
C     ustar    :: The eddy induced velocity in the x direction
C     vstar    :: The eddy induced velocity in the y direction
C     Xix      :: Xi in the x direction (m/s**2)
C     Xiy      :: Xi in the y direction (m/s**2)
      _RL gradqx(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL gradqy(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL XimX(GM_Bates_NModes,1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL XimY(GM_Bates_NModes,1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL cDopp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL umc(  1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
      _RL eady( 1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL urms( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
      _RL supp( 1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
      _RL ustar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
      _RL vstar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
      _RL Xix(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      _RL Xiy(   1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
#ifdef GM_BATES_PASSIVE
C     psistar :: eddy induced streamfunction in the y direction
      _RL psistar(1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr)
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     ======================================
C     Initialise some variables
C     ======================================
      small = TINY(zeroRL)
      update_modes=.FALSE.
      IF ( DIFFERENT_MULTIPLE(GM_Bates_vecFreq,myTime,deltaTClock) )
     &     update_modes=.TRUE.

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        kLow_C(i,j) = kLowC(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
        kLow_U(i,j) = MIN( kLow_C(i,j), kLow_C(i-1,j) )
       ENDDO
      ENDDO
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx
        kLow_V(i,j) = MIN( kLow_C(i,j), kLow_C(i,j-1) )
       ENDDO
      ENDDO

C     Dummy values for the edges. This does not affect the results
C     but avoids problems when solving for the eigenvalues.
      i=1-OLx
      DO j=1-OLy,sNy+OLy
       kLow_U(i,j) = 0
      ENDDO
      j=1-OLy
      DO i=1-OLx,sNx+OLx
       kLow_V(i,j) = 0
      ENDDO

      g_reciprho_sq = (gravity*recip_rhoConst)**2
C     Gradient of Coriolis
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
        dfdx(i,j) = ( fCori(i,j,bi,bj)-fCori(i-1,j,bi,bj) )
     &              *recip_dxC(i,j,bi,bj)
        dfdy(i,j) = ( fCori(i,j,bi,bj)-fCori(i,j-1,bi,bj) )
     &              *recip_dyC(i,j,bi,bj)
       ENDDO
      ENDDO

C     Coriolis at U and V points enforcing a minimum value so
C     that it is defined at the equator
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx+1,sNx+OLx
C       Not limited
        fCoriU(i,j)= op5*( fCori(i,j,bi,bj)+fCori(i-1,j,bi,bj) )
C       Limited so that the inverse is defined at the equator
        coriU(i,j) = SIGN( MAX( ABS(fCoriU(i,j)),GM_Bates_minCori ),
     &                              fCoriU(i,j) )
       ENDDO
      ENDDO
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx,sNx+OLx
C       Not limited
        fCoriV(i,j)= op5*( fCori(i,j,bi,bj)+fCori(i,j-1,bi,bj) )
C       Limited so that the inverse is defined at the equator
        coriV(i,j) = SIGN( MAX( ABS(fCoriV(i,j)),GM_Bates_minCori ),
     &                              fCoriV(i,j) )
       ENDDO
      ENDDO
C     Some dummy values at the edges
      i=1-OLx
      DO j=1-OLy,sNy+OLy
       fCoriU(i,j)= fCori(i,j,bi,bj)
       coriU(i,j) = SIGN( MAX( ABS(fCori(i,j,bi,bj)),GM_Bates_minCori ),
     &                             fCori(i,j,bi,bj) )
      ENDDO
      j=1-OLy
      DO i=1-OLx,sNx+OLx
       fCoriV(i,j)= fCori(i,j,bi,bj)
       coriV(i,j) = SIGN( MAX( ABS(fCori(i,j,bi,bj)),GM_Bates_minCori ),
     &                             fCori(i,j,bi,bj) )
      ENDDO

C     Zeroing some cumulative fields
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        eady(i,j)   = zeroRL
        BVint(i,j)  = zeroRL
        Ubaro(i,j)  = zeroRL
        deltaH(i,j) = zeroRL
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         slopeC(i,j,k)=zeroRL
        ENDDO
       ENDDO
      ENDDO

C     initialise remaining 2d variables
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        Rurms(i,j)=zeroRL
        RRhines(i,j)=zeroRL
        Rmix(i,j)=zeroRL
       ENDDO
      ENDDO
C     initialise remaining 3d variables
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         N2loc(i,j,k)=GM_Bates_minN2
         N2W(i,j,k) = GM_Bates_minN2
         N2S(i,j,k) = GM_Bates_minN2
         M4loc(i,j,k)=zeroRL
         M4onN2(i,j,k)=zeroRL
         urms(i,j,k)=zeroRL
         SlopeX(i,j,k)=zeroRL
         SlopeY(i,j,k)=zeroRL
         dSigmaDx(i,j,k)=zeroRL
         dSigmaDy(i,j,k)=zeroRL
         gradqx(i,j,k)=zeroRL
         gradqy(i,j,k)=zeroRL
        ENDDO
       ENDDO
      ENDDO

C     Find the zonal velocity at the cell centre
#ifdef ALLOW_EDDYPSI
      IF (GM_InMomAsStress) THEN
        DO k=1,Nr
         DO i = 1-OLx,sNx+OLx
          DO j = 1-OLy,sNy+OLy
           uFldX(i,j,k) = uEulerMean(i,j,k,bi,bj)
           vFldY(i,j,k) = vEulerMean(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
      ELSE
#endif
        DO k=1,Nr
         DO i = 1-OLx,sNx+OLx
          DO j = 1-OLy,sNy+OLy
           uFldX(i,j,k) = uVel(i,j,k,bi,bj)
           vFldY(i,j,k) = vVel(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
#ifdef ALLOW_EDDYPSI
      ENDIF
#endif

C     The following comes from rotate_uv2en_rl
C     This code does two things:
C     1) go from C grid velocity points to A grid velocity points
C     2) go from model grid directions to east/west directions
      DO k = 1,Nr
       DO i = 1-OLx,sNx+OLx
        j=sNy+OLy
        tmpU(i,j)=zeroRL
        tmpV(i,j)=zeroRL
       ENDDO
       DO j = 1-OLy,sNy+OLy-1
        i=sNx+OLx
        tmpU(i,j)=zeroRL
        tmpV(i,j)=zeroRL
        DO i = 1-OLx,sNx+OLx-1
         tmpU(i,j) = 0.5 _d 0
     &        *( uFldX(i+1,j,k) + uFldX(i,j,k) )
         tmpV(i,j) = 0.5 _d 0
     &        *( vFldY(i,j+1,k) + vFldY(i,j,k) )

         tmpU(i,j) = tmpU(i,j) * maskC(i,j,k,bi,bj)
         tmpV(i,j) = tmpV(i,j) * maskC(i,j,k,bi,bj)
        ENDDO
       ENDDO
C     Rotation
       DO j = 1-OLy,sNy+OLy
        DO i = 1-OLx,sNx+OLx
         ubar(i,j,k) =
     &        angleCosC(i,j,bi,bj)*tmpU(i,j)
     &        -angleSinC(i,j,bi,bj)*tmpV(i,j)
        ENDDO
       ENDDO
      ENDDO

C     Calculate the barotropic velocity by vertically integrating
C     and the dividing by the depth of the water column
C     Note that Ubaro is at the C-point.
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         Ubaro(i,j) = Ubaro(i,j) +
     &        drF(k)*hfacC(i,j,k,bi,bj)*ubar(i,j,k)
        ENDDO
       ENDDO
      ENDDO
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        IF (kLow_C(i,j).GT.0) THEN
C         The minus sign is because r_Low<0
          Ubaro(i,j) = -Ubaro(i,j)/r_Low(i,j,bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     Square of the buoyancy frequency at the top of a grid cell
C     Enforce a minimum N2
C     Mask N2, so it is zero at bottom
      DO k=2,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         N2(i,j,k) = -gravity*recip_rhoConst*sigmaR(i,j,k)
         N2(i,j,k) = MAX(N2(i,j,k),GM_Bates_minN2)*maskC(i,j,k,bi,bj)
         N(i,j,k)  = SQRT(N2(i,j,k))
        ENDDO
       ENDDO
      ENDDO
C     N2(k=1) is always zero
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        N2(i,j,1) = zeroRL
        N(i,j,1)  = zeroRL
       ENDDO
      ENDDO
C     Calculate the minimum drho/dz
      maxDRhoDz = -rhoConst*GM_Bates_minN2/gravity

C     Integrate the buoyancy frequency vertically using the trapezoidal method.
C     Assume that N(z=-H)=0
      DO k=1,Nr
       kp1 = min(k+1,Nr)
       mskp1 = oneRL
       IF ( k.EQ.Nr ) mskp1 = zeroRL
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
           BVint(i,j) = BVint(i,j) + hFacC(i,j,k,bi,bj)*drF(k)
     &                         *op5*(N(i,j,k)+mskp1*N(i,j,kp1))
        ENDDO
       ENDDO
      ENDDO

C     Calculate the eigenvalues and eigenvectors
      IF (update_modes) THEN
        CALL GMREDI_CALC_EIGS(
     I       iMin,iMax,jMin,jMax,bi,bj,N2,myThid,
     I       kLow_C, maskC(:,:,:,bi,bj),
     I       hfacC(:,:,:,bi,bj), recip_hfacC(:,:,:,bi,bj),
     I       R_Low(:,:,bi,bj), 1, .TRUE.,
     O       Rmid, modesC(:,:,:,:,bi,bj))

C       Calculate the Rossby Radius
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          Req  = SQRT(BVint(i,j)/(2. _d 0*pi*gradf(i,j,bi,bj)))
          Rdef(i,j,bi,bj) = MIN(Rmid(i,j),Req)
         ENDDO
        ENDDO
      ENDIF

C     Average dsigma/dx and dsigma/dy onto the centre points
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         dSigmaDx(i,j,k) = op5*(sigmaX(i,j,k)+sigmaX(i+1,j,k))
         dSigmaDy(i,j,k) = op5*(sigmaY(i,j,k)+sigmaY(i,j+1,k))
        ENDDO
       ENDDO
      ENDDO

C     ===============================
C     Calculate the Eady growth rate
C     ===============================
      DO k=1,Nr

       kp1 = min(k+1,Nr)
       mskp1 = oneRL
       IF ( k.EQ.Nr ) mskp1 = zeroRL

       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         M4loc(i,j,k) = g_reciprho_sq*( dSigmaDx(i,j,k)**2
     &                                 +dSigmaDy(i,j,k)**2 )
         N2loc(i,j,k) = op5*(N2(i,j,k)+mskp1*N2(i,j,kp1))
        ENDDO
       ENDDO
C      The bottom of the grid cell is shallower than the top
C      integration level, so, advance the depth.
       IF (-rF(k+1) .LE. GM_Bates_EadyMinDepth) CYCLE

C      Do not bother going any deeper since the top of the
C      cell is deeper than the bottom integration level
       IF (-rF(k).GE.GM_Bates_EadyMaxDepth) EXIT

C      We are in the integration depth range
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         IF ( (kLow_C(i,j).GE.k) .AND.
     &        (-hMixLayer(i,j,bi,bj).LE.-rC(k)) ) THEN

           slopeC(i,j,k) = SQRT(M4loc(i,j,k))/N2loc(i,j,k)
C          Limit the slope.  Note, this is not all the Eady calculations.
           IF (slopeC(i,j,k).LE.GM_maxSlope) THEN
             M4onN2(i,j,k) = M4loc(i,j,k)/N2loc(i,j,k)
           ELSE
             slopeC(i,j,k) = GM_maxslope
             M4onN2(i,j,k) = SQRT(M4loc(i,j,k))*GM_maxslope
           ENDIF
           eady(i,j)   = eady(i,j)
     &                   + hfacC(i,j,k,bi,bj)*drF(k)*M4onN2(i,j,k)
           deltaH(i,j) = deltaH(i,j) + drF(k)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C       If the minimum depth for the integration is deeper than the ocean
C       bottom OR the mixed layer is deeper than the maximum depth of
C       integration, we set the Eady growth rate to something small
C       to avoid floating point exceptions.
C       Later, these areas will be given a small diffusivity.
        IF (deltaH(i,j).EQ.zeroRL) THEN
          eady(i,j) = small

C       Otherwise, divide over the integration and take the square root
C       to actually find the Eady growth rate.
        ELSE
          eady(i,j) = SQRT(eady(i,j)/deltaH(i,j))

        ENDIF

       ENDDO
      ENDDO

C     ======================================
C     Calculate the diffusivity
C     ======================================
      DO j=1-OLy+1,sNy+OLy
       DO i=1-OLx+1,sNx+OLx-1
C       Calculate the Visbeck velocity
        Rurms(i,j) = MIN(Rdef(i,j,bi,bj),GM_Bates_Rmax)
        urms(i,j,1) = GM_Bates_Lambda*eady(i,j)*Rurms(i,j)
C       Set the bottom urms to zero
        k=kLow_C(i,j)
        IF (k.GT.0) urms(i,j,k) = 0.0

C       Calculate the Rhines scale
        RRhines(i,j) = SQRT(urms(i,j,1)/gradf(i,j,bi,bj))

C       Calculate the estimated length scale
        Rmix(i,j) = MIN(Rdef(i,j,bi,bj), RRhines(i,j))
        Rmix(i,j) = MAX(Rmix(i,j),GM_Bates_Rmin)

C       Calculate the Doppler shifted long Rossby wave speed
C       Ubaro is at the C-point.
        cDopp(i,j) = Ubaro(i,j)
     &                - gradf(i,j,bi,bj)*Rdef(i,j,bi,bj)*Rdef(i,j,bi,bj)
C       Limit the wave speed to the namelist variable GM_Bates_maxC
        IF (ABS(cDopp(i,j)).GT.GM_Bates_maxC) THEN
          cDopp(i,j) = MAX(GM_Bates_maxC, cDopp(i,j))
        ENDIF

       ENDDO
      ENDDO

C     Project the surface urms to the subsurface using the first baroclinic mode
      CALL GMREDI_CALC_URMS(
     I     iMin,iMax,jMin,jMax,bi,bj,N2,myThid,
     U     urms)

C     Calculate the diffusivity (on the mass grid)
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (k.LE.kLow_C(i,j)) THEN
           IF (deltaH(i,j).EQ.zeroRL) THEN
             GM_BatesK3d(i,j,k,bi,bj) = GM_Bates_smallK
           ELSE
             IF (urms(i,j,k).EQ.0.0) THEN
               GM_BatesK3d(i,j,k,bi,bj) = GM_Bates_smallK
             ELSE
              umc(i,j,k) =ubar(i,j,k) - cDopp(i,j)
              supp(i,j,k) = 1./
     &               ( 1. + GM_Bates_b1*umc(i,j,k)**2/urms(i,j,1)**2 )
C              2*Rmix gives the diameter
              GM_BatesK3d(i,j,k,bi,bj) = GM_Bates_gamma*urms(i,j,k)
     &                           *2.*Rmix(i,j)*supp(i,j,k)
             ENDIF

C            Enforce lower and upper bounds on the diffusivity
             GM_BatesK3d(i,j,k,bi,bj) =
     &               MIN( GM_BatesK3d(i,j,k,bi,bj), GM_Bates_maxK )
             GM_BatesK3d(i,j,k,bi,bj) =
     &               MAX( GM_BatesK3d(i,j,k,bi,bj), GM_Bates_smallK )
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     ======================================
C     Find the PV gradient
C     ======================================
C     Calculate the surface layer thickness.
C     Use hMixLayer (calculated in model/src/calc_oce_mxlayer)
C     for the mixed layer depth.

C     Enforce a minimum surface layer depth
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        surfkz(i,j) = MIN(-GM_Bates_surfMinDepth,-hMixLayer(i,j,bi,bj))
        surfkz(i,j) = MAX(surfkz(i,j),R_low(i,j,bi,bj))
        IF(maskC(i,j,1,bi,bj).EQ.0.0) surfkz(i,j)=0.0
        surfk(i,j) = 0
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         IF (rF(k).GT.surfkz(i,j) .AND. surfkz(i,j).GE.rF(k+1))
     &        surfk(i,j) = k
        ENDDO
       ENDDO
      ENDDO

C     Calculate the isopycnal slope
      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
        SlopeX(i,j,1) = zeroRL
        SlopeY(i,j,1) = zeroRL
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         IF(surfk(i,j).GE.kLowC(i,j,bi,bj)) THEN
C          If the surface layer is thinner than the water column
C          the set the slope to zero to avoid problems.
           SlopeX(i,j,k) = zeroRL
           SlopeY(i,j,k) = zeroRL

         ELSE
C          Calculate the zonal slope at the western cell face (U grid)
           sigz  = MIN( op5*(sigmaR(i,j,k)+sigmaR(i-1,j,k)), maxDRhoDz )
           sigx  = op5*( sigmaX(i,j,k)+sigmaX(i,j,k-1) )
           slope = sigx/sigz
           IF(ABS(slope).GT.GM_maxSlope)
     &          slope = SIGN(GM_maxSlope,slope)
           SlopeX(i,j,k)=-maskW(i,j,k-1,bi,bj)*maskW(i,j,k,bi,bj)*slope

C          Calculate the meridional slope at the southern cell face (V grid)
           sigz  = MIN( op5*(sigmaR(i,j,k)+sigmaR(i,j-1,k)), maxDRhoDz )
           sigy  = op5*( sigmaY(i,j,k) + sigmaY(i,j,k-1) )
           slope = sigy/sigz
           IF(ABS(slope).GT.GM_maxSlope)
     &          slope = SIGN(GM_maxSlope,slope)
           SlopeY(i,j,k)=-maskS(i,j,k-1,bi,bj)*maskS(i,j,k,bi,bj)*slope
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     Calculate gradients of PV stretching term and PV diffusivity.
C     These may be altered later depending on namelist options.
C     Enforce a zero slope bottom boundary condition for the bottom most cells (k=Nr)
      k=Nr
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
C          Zonal gradient of PV stretching term: at the western cell face
           tfluxX(i,j,k) = -fCoriU(i,j)*SlopeX(i,j,k)
     &                      *recip_drF(k)*recip_hFacW(i,j,k,bi,bj)
C     Meridional gradient of PV stretching term: at the southern cell face
           tfluxY(i,j,k) = -fCoriV(i,j)*SlopeY(i,j,k)
     &                     *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)

C          Use the interior diffusivity. Note: if we are using a
C          constant diffusivity KPV is overwritten later
           KPV(i,j,k) = GM_BatesK3d(i,j,k,bi,bj)

       ENDDO
      ENDDO

C     Calculate gradients of PV stretching term and PV diffusivity: for other cells (k<Nr)
      DO k=Nr-1,1,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
C          Zonal gradient of PV stretching term: at the western cell face
         tfluxX(i,j,k)=-fCoriU(i,j)*(SlopeX(i,j,k)-SlopeX(i,j,k+1))
     &        *recip_drF(k)*recip_hFacW(i,j,k,bi,bj)
     &        *maskW(i,j,k,bi,bj)
C     Meridional gradient of PV stretching term: at the southern cell face
         tfluxY(i,j,k)=-fCoriV(i,j)*(SlopeY(i,j,k)-SlopeY(i,j,k+1))
     &        *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
     &        *maskS(i,j,k,bi,bj)

C        Use the interior diffusivity. Note: if we are using a
C        constant diffusivity KPV is overwritten later
         KPV(i,j,k) = GM_BatesK3d(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO

C     Special treatment for the surface layer (if necessary), which overwrites
C     values in the previous loops.
      IF (GM_Bates_ThickSheet .OR. GM_Bates_surfK) THEN
        DO k=Nr-1,1,-1
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           IF(k.LE.surfk(i,j)) THEN
C            We are in the surface layer.  Change the thickness flux
C            and diffusivity as necessary.

             IF (GM_Bates_ThickSheet) THEN
C              We are in the surface layer, so set the thickness flux
C              based on the average slope over the surface layer
C              If we are on the edge of a "cliff" the surface layer at the
C              centre of the grid point could be deeper than the U or V point.
C              So, we ensure that we always take a sensible slope.
               IF(kLow_U(i,j).LT.surfk(i,j)) THEN
                 kk=kLow_U(i,j)
                 hsurf = -rLowW(i,j,bi,bj)
               ELSE
                 kk=surfk(i,j)
                 hsurf = -surfkz(i,j)
               ENDIF
               IF(kk.GT.0) THEN
                 IF(kk.EQ.Nr) THEN
                   tfluxX(i,j,k) = -fCoriU(i,j)*maskW(i,j,k,bi,bj)
     &                  *SlopeX(i,j,kk)/hsurf
                 ELSE
                   tfluxX(i,j,k) = -fCoriU(i,j)*maskW(i,j,k,bi,bj)
     &                  *( SlopeX(i,j,kk)-SlopeX(i,j,kk+1) )/hsurf
                 ENDIF
               ELSE
                 tfluxX(i,j,k) = zeroRL
               ENDIF

               IF(kLow_V(i,j).LT.surfk(i,j)) THEN
                 kk=kLow_V(i,j)
                 hsurf = -rLowS(i,j,bi,bj)
               ELSE
                 kk=surfk(i,j)
                 hsurf = -surfkz(i,j)
               ENDIF
               IF(kk.GT.0) THEN
                 IF(kk.EQ.Nr) THEN
                   tfluxY(i,j,k) = -fCoriV(i,j)*maskS(i,j,k,bi,bj)
     &                  *SlopeY(i,j,kk)/hsurf
                 ELSE
                   tfluxY(i,j,k) = -fCoriV(i,j)*maskS(i,j,k,bi,bj)
     &                  *( SlopeY(i,j,kk)-SlopeY(i,j,kk+1) )/hsurf
                 ENDIF
               ELSE
                 tfluxY(i,j,k) = zeroRL
               ENDIF
             ENDIF

             IF (GM_Bates_surfK) THEN
C              Use a constant K in the surface layer.
               KPV(i,j,k) = GM_Bates_constK
             ENDIF
           ENDIF
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C     Calculate gradq
      IF (GM_Bates_beta_eq_0) THEN
C       Ignore beta in the calculation of grad(q)
        DO k=1,Nr
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           gradqx(i,j,k) = maskW(i,j,k,bi,bj)*tfluxX(i,j,k)
           gradqy(i,j,k) = maskS(i,j,k,bi,bj)*tfluxY(i,j,k)
          ENDDO
         ENDDO
        ENDDO

      ELSE
C       Do not ignore beta
        DO k=1,Nr
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
           gradqx(i,j,k) = maskW(i,j,k,bi,bj)*(dfdx(i,j)+tfluxX(i,j,k))
           gradqy(i,j,k) = maskS(i,j,k,bi,bj)*(dfdy(i,j)+tfluxY(i,j,k))
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C     ======================================
C     Find Xi and the eddy induced velocities
C     ======================================
C     Find the buoyancy frequency at the west and south faces of a cell
C     This is necessary to find the eigenvectors at those points
      DO k=1,Nr
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         N2W(i,j,k) = maskW(i,j,k,bi,bj)
     &                *( N2(i,j,k)+N2(i-1,j,k) )
         N2S(i,j,k) = maskS(i,j,k,bi,bj)
     &                *( N2(i,j,k)+N2(i,j-1,k) )
        ENDDO
       ENDDO
      ENDDO

C     If GM_Bates_use_constK=T, the diffusivity for the eddy transport is
C     set to a constant equal to GM_Bates_constK.
C     If the diffusivity is constant the method here is the same as GM.
C     If GM_Bates_constRedi=T, BatesK3d will be set to GM_Bates_constK later.
      IF(GM_Bates_use_constK) THEN
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           KPV(i,j,k) = GM_Bates_constK
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      IF (.NOT. GM_Bates_smooth) THEN
C       Do not expand K grad(q) => no smoothing
C       May only be done with a constant K, otherwise the
C       integral constraint is violated.
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           Xix(i,j,k) = -maskW(i,j,k,bi,bj)*KPV(i,j,k)*gradqx(i,j,k)
           Xiy(i,j,k) = -maskS(i,j,k,bi,bj)*KPV(i,j,k)*gradqy(i,j,k)
          ENDDO
         ENDDO
        ENDDO

      ELSE
C       Expand K grad(q) in terms of baroclinic modes to smooth
C       and satisfy the integral constraint

C       Start with the X direction
C       ------------------------------
C       Calculate the eigenvectors at the West face of a cell
        IF (update_modes) THEN
          CALL GMREDI_CALC_EIGS(
     I         iMin,iMax,jMin,jMax,bi,bj, N2W, myThid,
     I         kLow_U, maskW(:,:,:,bi,bj),
     I         hfacW(:,:,:,bi,bj), recip_hfacW(:,:,:,bi,bj),
     I         rLowW(:,:,bi,bj), GM_Bates_NModes, .FALSE.,
     O         dummy, modesW(:,:,:,:,bi,bj) )
        ENDIF

C       Calculate Xi_m at the west face of a cell
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DO m=1,GM_Bates_NModes
           XimX(m,i,j) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO m=1,GM_Bates_NModes
            Kdqdx(i,j,k) = KPV(i,j,k)*gradqx(i,j,k)
            XimX(m,i,j) = XimX(m,i,j)
     &           - maskW(i,j,k,bi,bj)*drF(k)*hfacW(i,j,k,bi,bj)
     &           *Kdqdx(i,j,k)*modesW(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Calculate Xi in the X direction at the west face
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           Xix(i,j,k) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO m=1,GM_Bates_NModes
            Xix(i,j,k) = Xix(i,j,k)
     &           + maskW(i,j,k,bi,bj)*XimX(m,i,j)*modesW(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Now the Y direction
C     ------------------------------
C     Calculate the eigenvectors at the West face of a cell
        IF (update_modes) THEN
          CALL GMREDI_CALC_EIGS(
     I         iMin,iMax,jMin,jMax,bi,bj, N2S, myThid,
     I         kLow_V, maskS(:,:,:,bi,bj),
     I         hfacS(:,:,:,bi,bj), recip_hfacS(:,:,:,bi,bj),
     I         rLowS(:,:,bi,bj), GM_Bates_NModes, .FALSE.,
     O         dummy, modesS(:,:,:,:,bi,bj) )
        ENDIF

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          DO m=1,GM_Bates_NModes
           XimY(m,i,j) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO m=1,GM_Bates_NModes
            Kdqdy(i,j,k) = KPV(i,j,k)*gradqy(i,j,k)
            XimY(m,i,j) = XimY(m,i,j)
     &           - drF(k)*hfacS(i,j,k,bi,bj)
     &           *Kdqdy(i,j,k)*modesS(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     Calculate Xi for Y direction at the south face
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           Xiy(i,j,k) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           DO m=1,GM_Bates_NModes
            Xiy(i,j,k) = Xiy(i,j,k)
     &           + maskS(i,j,k,bi,bj)*XimY(m,i,j)*modesS(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     ENDIF (.NOT. GM_Bates_smooth)
      ENDIF

C     Calculate the renormalisation factor
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        uInt(i,j)=zeroRL
        vInt(i,j)=zeroRL
        KdqdyInt(i,j)=zeroRL
        KdqdxInt(i,j)=zeroRL
        uKdqdyInt(i,j)=zeroRL
        vKdqdxInt(i,j)=zeroRL
        uXiyInt(i,j)=zeroRL
        vXixInt(i,j)=zeroRL
        Renorm(i,j)=oneRL
        RenormU(i,j)=oneRL
        RenormV(i,j)=oneRL
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-OLy,sNy+OLy-1
        DO i=1-OLx,sNx+OLx-1
         centreX = op5*(uVel(i,j,k,bi,bj)+uVel(i+1,j,k,bi,bj))
         centreY = op5*(Kdqdy(i,j,k)     +Kdqdy(i,j+1,k)     )
C        For the numerator
         uInt(i,j) = uInt(i,j)
     &        + centreX*hfacC(i,j,k,bi,bj)*drF(k)
         KdqdyInt(i,j) = KdqdyInt(i,j)
     &        + centreY*hfacC(i,j,k,bi,bj)*drF(k)
         uKdqdyInt(i,j) = uKdqdyInt(i,j)
     &        + centreX*centreY*hfacC(i,j,k,bi,bj)*drF(k)
C        For the denominator
         centreY = op5*(Xiy(i,j,k) + Xiy(i,j+1,k))
         uXiyInt(i,j) = uXiyInt(i,j)
     &        + centreX*centreY*hfacC(i,j,k,bi,bj)*drF(k)

         centreX = op5*(Kdqdx(i,j,k)     +Kdqdx(i+1,j,k))
         centreY = op5*(vVel(i,j,k,bi,bj)+vVel(i,j+1,k,bi,bj) )
C        For the numerator
         vInt(i,j) = vInt(i,j)
     &        + centreY*hfacC(i,j,k,bi,bj)*drF(k)
         KdqdxInt(i,j) = KdqdxInt(i,j)
     &        + CentreX*hfacC(i,j,k,bi,bj)*drF(k)
         vKdqdxInt(i,j) = vKdqdxInt(i,j)
     &        + centreY*centreX*hfacC(i,j,k,bi,bj)*drF(k)
C        For the denominator
         centreX = op5*(Xix(i,j,k) + Xix(i+1,j,k))
         vXixInt(i,j) = vXixInt(i,j)
     &        + centreY*centreX*hfacC(i,j,k,bi,bj)*drF(k)

        ENDDO
       ENDDO
      ENDDO

      DO j=1-OLy,sNy+OLy-1
       DO i=1-OLx,sNx+OLx-1
        IF (kLowC(i,j,bi,bj).GT.0) THEN
          numerator =
     &         (uKdqdyInt(i,j)-uInt(i,j)*KdqdyInt(i,j)/R_low(i,j,bi,bj))
     &        -(vKdqdxInt(i,j)-vInt(i,j)*KdqdxInt(i,j)/R_low(i,j,bi,bj))
          denominator = uXiyInt(i,j) - vXixInt(i,j)
C         We can have troubles with floating point exceptions if the denominator
C         of the renormalisation if the ocean is resting (e.g. intial conditions).
C         So we make the renormalisation factor one if the denominator is very small
C         The renormalisation factor is supposed to correct the error in the extraction of
C         potential energy associated with the truncation of the expansion. Thus, we
C         enforce a minimum value for the renormalisation factor.
C         We also enforce a maximum renormalisation factor.
          IF (denominator.GT.small) THEN
            Renorm(i,j) = ABS(numerator/denominator)
            Renorm(i,j) = MAX(Renorm(i,j),GM_Bates_minRenorm)
            Renorm(i,j) = MIN(Renorm(i,j),GM_Bates_maxRenorm)
          ENDIF
        ENDIF
       ENDDO
      ENDDO
C     Now put it back on to the velocity grids
      DO j=1-OLy+1,sNy+OLy-1
       DO i=1-OLx+1,sNx+OLx-1
        RenormU(i,j) = op5*(Renorm(i-1,j)+Renorm(i,j))
        RenormV(i,j) = op5*(Renorm(i,j-1)+Renorm(i,j))
       ENDDO
      ENDDO

C     Calculate the eddy induced velocity in the X direction at the west face
      DO k=1,Nr
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         ustar(i,j,k) = -RenormU(i,j)*Xix(i,j,k)/coriU(i,j)
        ENDDO
       ENDDO
      ENDDO

C     Calculate the eddy induced velocity in the Y direction at the south face
      DO k=1,Nr
       DO j=1-OLy+1,sNy+OLy
        DO i=1-OLx+1,sNx+OLx
         vstar(i,j,k) = -RenormV(i,j)*Xiy(i,j,k)/coriV(i,j)
        ENDDO
       ENDDO
      ENDDO

C     ======================================
C     Calculate the eddy induced overturning streamfunction
C     ======================================
#ifdef GM_BATES_PASSIVE
      k=Nr
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        psistar(i,j,Nr) = -hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
       ENDDO
      ENDDO
      DO k=Nr-1,1,-1
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
           psistar(i,j,k) = psistar(i,j,k+1)
     &          - hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
        ENDDO
       ENDDO
      ENDDO

#else

      IF (GM_AdvForm) THEN
        k=Nr
        DO j=1-OLy+1,sNy+1
         DO i=1-OLx+1,sNx+1
          GM_PsiX(i,j,k,bi,bj) = -hfacW(i,j,k,bi,bj)*drF(k)*ustar(i,j,k)
          GM_PsiY(i,j,k,bi,bj) = -hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
         ENDDO
        ENDDO
        DO k=Nr-1,1,-1
         DO j=1-OLy+1,sNy+1
          DO i=1-OLx+1,sNx+1
           GM_PsiX(i,j,k,bi,bj) = GM_PsiX(i,j,k+1,bi,bj)
     &          - hfacW(i,j,k,bi,bj)*drF(k)*ustar(i,j,k)
           GM_PsiY(i,j,k,bi,bj) = GM_PsiY(i,j,k+1,bi,bj)
     &          - hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
          ENDDO
         ENDDO
        ENDDO

      ENDIF
#endif

#ifdef ALLOW_DIAGNOSTICS
C     Diagnostics
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( GM_BatesK3d, 'GM_BaK  ',
     I                         0, Nr, 1, bi, bj, myThid )
        CALL DIAGNOSTICS_FILL(KPV,    'GM_KPV  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(urms,   'GM_URMS ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Rdef,   'GM_RDEF ',0, 1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Rurms,  'GM_RURMS',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(RRhines,'GM_RRHNS',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Rmix,   'GM_RMIX ',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(supp,   'GM_SUPP ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Xix,    'GM_Xix  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Xiy,    'GM_Xiy  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(cDopp,  'GM_C    ',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Ubaro,  'GM_UBARO',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(eady,   'GM_EADY ',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(SlopeX, 'GM_Sx   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(SlopeY, 'GM_Sy   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(tfluxX, 'GM_TFLXX',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(tfluxY, 'GM_TFLXY',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gradqx, 'GM_dqdx ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(gradqy, 'GM_dqdy ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Kdqdy,  'GM_Kdqdy',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Kdqdx,  'GM_Kdqdx',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(surfkz, 'GM_SFLYR',0, 1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(ustar,  'GM_USTAR',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(vstar,  'GM_VSTAR',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(umc,    'GM_UMC  ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(ubar,   'GM_UBAR ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(modesC, 'GM_MODEC',0,Nr,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(M4loc,  'GM_M4   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(N2loc,  'GM_N2   ',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(M4onN2, 'GM_M4_N2',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(slopeC, 'GM_SLOPE',0,Nr,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(Renorm, 'GM_RENRM',0, 1,2,bi,bj,myThid)
#ifdef GM_BATES_PASSIVE
        CALL DIAGNOSTICS_FILL(psistar,'GM_PSTAR',0,Nr,2,bi,bj,myThid)
#endif
      ENDIF
#endif

C     For the Redi diffusivity, we set GM_BatesK3d to a constant if
C     GM_Bates_constRedi=.TRUE.
      IF (GM_Bates_constRedi) THEN
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GM_BatesK3d(i,j,k,bi,bj) = GM_Bates_constK
          ENDDO
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics )
     &  CALL DIAGNOSTICS_FILL( GM_BatesK3d, 'GM_BaK_T',
     I                         0, Nr, 1, bi, bj, myThid )
#endif

#endif /* GM_BATES_K3D */
      RETURN
      END
