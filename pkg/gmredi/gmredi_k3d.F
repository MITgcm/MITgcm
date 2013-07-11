C $Header: /u/gcmpack/MITgcm/pkg/gmredi/gmredi_k3d.F,v 1.4 2013/07/11 14:33:23 m_bates Exp $
C $Name:  $
#include "GMREDI_OPTIONS.h"

C     !ROUTINE: GMREDI_K3D
C     !INTERFACE:
      SUBROUTINE GMREDI_K3D(
     I     iMin, iMax, jMin, jMax,
     I     sigmaX, sigmaY, sigmaR,
     I     bi, bj, myTime, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_K3D
C     | o Calculates the 3D diffusivity as per Bates et al. (2013)
C     *==========================================================*
C     \ev

      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     bi, bj    :: tile indices
C     myThid    :: My Thread Id. number

      INTEGER iMin,iMax,jMin,jMax
      _RL sigmaX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL sigmaR(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      INTEGER bi, bj
      _RL myTime
      INTEGER myThid

#ifdef GM_K3D

C     === Functions ====
      LOGICAL  DIFFERENT_MULTIPLE
      EXTERNAL DIFFERENT_MULTIPLE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER i,j,k,kk,m

C     update_modes :: Whether to update the eigenmodes
      LOGICAL update_modes

C     surfk  :: index of the depth of the surface layer
C     kLow_C :: Local version of the index of deepest wet grid cell on tracer grid
C     kLow_U :: Local version of the index of deepest wet grid cell on U grid
C     kLow_V :: Local version of the index of deepest wet grid cell on V grid
      INTEGER surfk(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER kLow_C(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER kLow_U(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      INTEGER kLow_V(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

C     N2loc  :: local N**2
C     slope  :: local slope
C     Req    :: local equatorial deformation radius (m)
C     Rurms  :: a local mixing length used in calculation of urms (m)
C     deltaH :: local thickness of Eady integration (m)
C     g_reciprho_sq :: (gravity*recip_rhoConst)**2
C     M4loc  :: local M**4
C     maxDRhoDz :: maximum value of d(rho)/dz (derived from GM_K3D_minN2)
C     sigx   :: local d(rho)/dx
C     sigy   :: local d(rho)/dy
C     sigz   :: local d(rho)/dz
C     hsurf  :: local surface layer depth
C     small  :: a small number (to avoid floating point exceptions)
      _RL N2loc
      _RL slope
      _RL Req
      _RL Rurms
      _RL deltaH
      _RL g_reciprho_sq
      _RL M4loc
      _RL maxDRhoDz
      _RL sigx, sigy, sigz
      _RL hsurf
      _RL small

C     dfdy    :: gradient of the Coriolis paramter, df/dy, 1/(m*s)
C     dfdx    :: gradient of the Coriolis paramter, df/dx, 1/(m*s)
C     gradf   :: total gradient of the Coriolis paramter, SQRT(df/dx**2+df/dy**2), 1/(m*s)
C     RRhines :: The Rhines scale (m)
C     Rmix    :: Mixing length
C     N2      :: Square of the buoyancy frequency (1/s**2)
C     N2W     :: Square of the buoyancy frequency (1/s**2) averaged to west of grid cell
C     N2S     :: Square of the buoyancy frequency (1/s**2) averaged to south of grid cell
C     N       :: Buoyancy frequency, SQRT(N2)
C     BVint   :: The vertical integral of N (m/s)
C     ubar    :: Zonal velocity on a tracer point (m/s)
C     vbar    :: Meridional velocity on a tracer point (m/s)
C     Ubaro   :: Barotropic velocity (m/s)
      _RL dfdy(   1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dfdx(   1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL gradf(  1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dummy(  1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL RRhines(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL Rmix(   1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL N2(     1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL N2W(    1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL N2S(    1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL N(      1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL BVint(  1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL Ubaro(  1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL ubar(   1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      _RL vbar(   1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)

C     Rmid     :: Rossby radius (m)
C     KPV      :: Diffusivity (m**2/s)
C     SlopeX   :: isopycnal slope in x direction
C     SlopeY   :: isopycnal slope in y direction
C     dSigmaDx :: sigmaX averaged onto tracer grid
C     dSigmaDy :: sigmaY averaged onto tracer grid
C     tfluxX   :: thickness flux in x direction
C     tfluxY   :: thickness flux in y direction
C     fCoriU   :: Coriolis parameter averaged to U points
C     fCoriV   :: Coriolis parameter averaged to V points
C     cori     :: Coriolis parameter forced to be finite near the equator
C     coriU    :: As for cori, but, at U point
C     coriV    :: As for cori, but, at V point
C     surfkz   :: Depth of surface layer (in r units)
      _RL Rmid(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL KPV(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL SlopeX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL SlopeY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL dSigmaDx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL dSigmaDy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL tfluxX(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL tfluxY(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL cori(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL coriU(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL coriV(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL fCoriU(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL fCoriV(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL surfkz(1-Olx:sNx+Olx,1-Oly:sNy+Oly)

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
      _RL gradqx(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL gradqy(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL XimX(GM_K3D_NModes,1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL XimY(GM_K3D_NModes,1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL cDopp(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL umc(  1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
      _RL eady( 1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL urms( 1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
      _RL supp( 1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
      _RL ustar(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
      _RL vstar(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
      _RL Xix(   1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL Xiy(   1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
#ifdef GM_K3D_PASSIVE
C     psistar :: eddy induced streamfunction in the y direction
      _RL psistar(1-Olx:sNx+Olx,1-Oly:sNy+Oly,1:Nr)
#endif


C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     ======================================
C     Initialise some variables
C     ======================================
      small = TINY(zeroRL)
      update_modes=.FALSE.
      IF ( DIFFERENT_MULTIPLE(GM_K3D_vecFreq,myTime,deltaTClock) )
     &     update_modes=.TRUE.

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        kLow_C(i,j) = kLowC(i,j,bi,bj)
       ENDDO
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        kLow_U(i,j) = MIN( kLow_C(i,j), kLow_C(i-1,j) )
       ENDDO
      ENDDO
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx,sNx+Olx
        kLow_V(i,j) = MIN( kLow_C(i,j), kLow_C(i,j-1) )
       ENDDO
      ENDDO

C     Dummy values for the edges 
C     This avoids weirdness in gmredi_calc_eigs
      i=1-Olx
      DO j=1-Oly,sNy+Oly
       kLow_U(i,j) = kLow_C(i,j)
      ENDDO
      j=1-Oly
      DO i=1-Olx,sNx+Olx
       kLow_V(i,j) = kLow_C(i,j)
      ENDDO

      g_reciprho_sq = (gravity*recip_rhoConst)**2
C     Gradient of Coriolis
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
        dfdx(i,j) = ( fCori(i,j,bi,bj)-fCori(i-1,j,bi,bj) )
     &              *recip_dxC(i,j,bi,bj)
        dfdy(i,j) = ( fCori(i,j,bi,bj)-fCori(i,j-1,bi,bj) )
     &              *recip_dyC(i,j,bi,bj)
       ENDDO
      ENDDO

C     Coriolis at C points enforcing a minimum value so 
C     that it is defined at the equator
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        cori(i,j)  = SIGN( MAX( ABS(fCori(i,j,bi,bj)),GM_K3D_minCori ),
     &                          fCori(i,j,bi,bj) )
       ENDDO
      ENDDO
C     Coriolis at U and V points
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx
C       Limited so that the inverse is defined at the equator
        coriU(i,j) = op5*( cori(i,j)+cori(i-1,j) )
        coriU(i,j) = SIGN( MAX( ABS(coriU(i,j)),GM_K3D_minCori ), 
     &       coriU(i,j) )

        coriV(i,j) = op5*( cori(i,j)+cori(i,j-1) )
        coriV(i,j) = SIGN( MAX( ABS(coriV(i,j)),GM_K3D_minCori ), 
     &       coriV(i,j) )

C       Not limited
        fCoriU(i,j) = op5*( fCori(i,j,bi,bj)+fCori(i-1,j,bi,bj) )
        fCoriV(i,j) = op5*( fCori(i,j,bi,bj)+fCori(i,j-1,bi,bj) )
       ENDDO
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        gradf(i,j) = SQRT( dfdx(i,j)**2 + dfdy(i,j)**2 )
       ENDDO
      ENDDO

C     Zeroing some cumulative fields
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        eady(i,j)  = zeroRL
        BVint(i,j) = zeroRL
        Ubaro(i,j) = zeroRL
       ENDDO
      ENDDO

C     Find the zonal velocity at the cell centre
C     The logicals here are, in order: 1/ go from grid to north/east directions
C     2/ go from C to A grid and 3/ apply the mask
      CALL rotate_uv2en_rl(uVel, vVel, ubar, vbar, .TRUE., .TRUE., 
     &     .TRUE.,Nr,mythid)

C     Square of the buoyancy frequency at the top of a grid cell
      DO k=2,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         N2(i,j,k) = -gravity*recip_rhoConst*sigmaR(i,j,k)
        ENDDO
       ENDDO
      ENDDO
C     N2(k=1) is always zero
      k=1
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        N2(i,j,k) = 0.0
        N(i,j,k)  = 0.0
       ENDDO
      ENDDO
C     Enforce a minimum N2 
      DO k=2,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (N2(i,j,k).LT.GM_K3D_minN2) N2(i,j,k)=GM_K3D_minN2
         N(i,j,k) = SQRT(N2(i,j,k))
        ENDDO
       ENDDO
      ENDDO
C     Calculate the minimum drho/dz
      maxDRhoDz = -rhoConst*GM_K3D_minN2/gravity

C     Calculate the barotropic velocity by vertically integrating 
C     and the dividing by the depth of the water column
C     Note that Ubaro is on the U grid.
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         Ubaro(i,j) = Ubaro(i,j) + 
     &        maskW(i,j,k,bi,bj)*drF(k)*hfacC(i,j,k,bi,bj)
     &                *ubar(i,j,k,bi,bj)
        ENDDO
       ENDDO
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        IF (kLow_C(i,j).GT.0) THEN
C         The minus sign is because r_Low<0
          Ubaro(i,j) = -Ubaro(i,j)/r_Low(i,j,bi,bj)
        ENDIF
       ENDDO
      ENDDO

C     Integrate the buoyancy frequency vertically using the trapezoidal method.
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (k.LT.kLow_C(i,j)) THEN
           BVint(i,j) = BVint(i,j) + hFacC(i,j,k,bi,bj)*drF(k)
     &                               *(N(i,j,k)+N(i,j,k+1))
         ELSEIF (k.EQ.kLow_C(i,j)) THEN
C          Assume that N(z=-H)=0
           BVint(i,j) = BVint(i,j) + hFacC(i,j,k,bi,bj)*drF(k)*N(i,j,k)
         ENDIF
        ENDDO
       ENDDO
      ENDDO
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        BVint(i,j) = op5*BVint(i,j)
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
        DO j=1-Oly+1,sNy+Oly
         DO i=1-Olx+1,sNx+Olx
          Req  = SQRT(BVint(i,j)/(2*pi*gradf(i,j)))
          Rdef(i,j,bi,bj) = MIN(Rmid(i,j),Req)
         ENDDO
        ENDDO
      ENDIF

C     Average dsigma/dx and dsigma/dy onto the centre points
      
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         dSigmaDx(i,j,k) = op5*(sigmaX(i,j,k)+sigmaX(i+1,j,k))
         dSigmaDy(i,j,k) = op5*(sigmaY(i,j,k)+sigmaY(i,j+1,k))
        ENDDO
       ENDDO
      ENDDO

C     ===============================
C     Calculate the Eady growth rate
C     ===============================
      DO k=1,Nr

C      The bottom of the grid cell is shallower than the top
C      integration level, so, advance the depth.
       IF (-rF(k+1).LE. GM_K3D_EadyMinDepth) CYCLE

C      Don't bother going any deeper since the top of the
C      cell is deeper than the bottom integration level
       IF (-rF(k).GE.GM_K3D_EadyMaxDepth) EXIT

C      We are in the integration depth range
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         IF (kLow_C(i,j).GE.k) THEN
           IF (k.NE.kLow_C(i,j)) THEN
             N2loc = op5*(N2(i,j,k)+N2(i,j,k+1))
           ELSE
             N2loc = op5*N2(i,j,k)
           ENDIF
           M4loc = g_reciprho_sq*( dSigmaDx(i,j,k)**2 
     &                            +dSigmaDy(i,j,k)**2 )
           slope = SQRT(SQRT(M4loc)/N2loc)

C          Limit the slope.  Note, this is not all the Eady calculations.
           IF (slope.LE.GM_K3D_maxSlope) THEN
             eady(i,j) = eady(i,j) 
     &            + hfacC(i,j,k,bi,bj)*drF(k)*M4loc/(N2loc)
           ELSE
             eady(i,j) = eady(i,j)
     &            + hfacC(i,j,k,bi,bj)*drF(k)*SQRT(M4loc)
     &              *GM_K3D_maxSlope*GM_K3D_maxSlope
           ENDIF
         ENDIF
        ENDDO
       ENDDO
      ENDDO

      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
C       If the minimum depth for the integration is deeper than ocean 
C       bottom then give the eady growth rate a dummy, non-zero value 
C       to avoid floating point exceptions.  These points are taken care
C       of by setting K3D=GM_K3D_smallK later.
        IF (kLow_C(i,j).NE.0 
     &       .AND. -r_Low(i,j,bi,bj).LT.GM_K3D_EadyMinDepth) THEN
          eady(i,j) = small

C       Otherwise, multiply eady by the various constants to get the 
C       growth rate. 
        ELSE
          deltaH = MIN(-r_low(i,j,bi,bj),GM_K3D_EadyMaxDepth)
          deltaH = deltaH - GM_K3D_EadyMinDepth
          eady(i,j) = SQRT(eady(i,j)/deltaH)
          
        ENDIF

       ENDDO
      ENDDO

C     ======================================
C     Calculate the diffusivity
C     ======================================
      DO j=1-Oly+1,sNy+Oly
       DO i=1-Olx+1,sNx+Olx-1
C       Calculate the Visbeck velocity
        Rurms = MIN(Rdef(i,j,bi,bj),GM_K3D_maxLurms)
        urms(i,j,1) = GM_K3D_Lambda*eady(i,j)*Rurms
C       Set the bottom urms to zero
        k=kLow_C(i,j)
        IF (k.GT.0) urms(i,j,k) = 0.0

C       Calculate the Rhines scale
        RRhines(i,j) = SQRT(urms(i,j,1)/gradf(i,j))

C       Calculate the estimated length scale
        Rmix(i,j) = MIN(Rdef(i,j,bi,bj), RRhines(i,j))

C       Calculate the Doppler shifted long Rossby wave speed
C       Ubaro is on the U grid so we must average onto the M grid.
        cDopp(i,j) = op5*( Ubaro(i,j)+Ubaro(i+1,j) )
     &                - gradf(i,j)*Rdef(i,j,bi,bj)*Rdef(i,j,bi,bj)
C       Limit the wave speed to the namelist variable GM_K3D_maxC
        IF (ABS(cDopp(i,j)).GT.GM_K3D_maxC) THEN
          cDopp(i,j) = MAX(GM_K3D_maxC, cDopp(i,j))
        ENDIF

       ENDDO
      ENDDO

C     Project the surface urms to the subsurface using the first baroclinic mode
      CALL GMREDI_CALC_URMS(
     I     iMin,iMax,jMin,jMax,bi,bj,N2,myThid,
     U     urms)

C     Calculate the diffusivity (on the mass grid)
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (k.LE.kLow_C(i,j)) THEN
           IF (-r_Low(i,j,bi,bj).LT.GM_K3D_EadyMinDepth) THEN
             K3D(i,j,k,bi,bj) = GM_K3D_smallK
           ELSE
             IF (urms(i,j,k).EQ.0.0) THEN
               K3D(i,j,k,bi,bj) = GM_K3D_smallK
             ELSE
               umc(i,j,k)  = ubar(i,j,k,bi,bj) - cDopp(i,j)
               supp(i,j,k) = 1/( 1 + 4*umc(i,j,k)**2/urms(i,j,1)**2 )
               K3D(i,j,k,bi,bj) = GM_K3D_gamma*urms(i,j,k)
     &                            *Rmix(i,j)*supp(i,j,k)
             ENDIF

C            Enforce lower and upper bounds on the diffusivity
             IF (K3D(i,j,k,bi,bj).LT.GM_K3D_smallK)
     &            K3D(i,j,k,bi,bj) = GM_K3D_smallK
             IF (K3D(i,j,k,bi,bj).GT.GM_maxK3D)
     &            K3D(i,j,k,bi,bj) = GM_maxK3D
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
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        surfkz(i,j) = MIN(-GM_K3D_surfMinDepth,-hMixLayer(i,j,bi,bj))
        surfkz(i,j) = MAX(surfkz(i,j),R_low(i,j,bi,bj))
        IF(maskC(i,j,1,bi,bj).EQ.0.0) surfkz(i,j)=0.0
        surfk(i,j) = 0
       ENDDO
      ENDDO
      DO k=1,Nr
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
         IF (rF(k).GT.surfkz(i,j) .AND. surfkz(i,j).GE.rF(k+1)) 
     &        surfk(i,j) = k
        ENDDO
       ENDDO
      ENDDO

C     Calculate the isopycnal slope
      DO j=1-Oly,sNy+Oly-1
       DO i=1-Olx,sNx+Olx-1
        SlopeX(i,j,1) = zeroRL
        SlopeY(i,j,1) = zeroRL
       ENDDO
      ENDDO
      DO k=2,Nr
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx+1,sNx+Olx
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
C           IF(ABS(slope).GT.GM_K3D_maxSlope)
C     &          slope = SIGN(GM_K3D_maxSlope,slope)
           IF(ABS(slope).GT.GM_maxSlope)
     &          slope = SIGN(GM_maxSlope,slope)
           SlopeX(i,j,k)=-maskW(i,j,k-1,bi,bj)*maskW(i,j,k,bi,bj)*slope
           
C          Calculate the meridional slope at the southern cell face (V grid)
           sigz  = MIN( op5*(sigmaR(i,j,k)+sigmaR(i,j-1,k)), maxDRhoDz )
           sigy  = op5*( sigmaY(i,j,k) + sigmaY(i,j,k-1) )
           slope = sigy/sigz
C           IF(ABS(slope).GT.GM_K3D_maxSlope)
C     &          slope = SIGN(GM_K3D_maxSlope,slope)
           IF(ABS(slope).GT.GM_maxSlope)
     &          slope = SIGN(GM_maxSlope,slope)
           SlopeY(i,j,k)=-maskS(i,j,k-1,bi,bj)*maskS(i,j,k,bi,bj)*slope
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     Calculate the thickness flux
C     Enforce a zero slope bottom boundary condition for the bottom most cells (k=Nr)
      k=Nr
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
C          Zonal thickness flux at the western cell face
           tfluxX(i,j,k) = -fCoriU(i,j)*SlopeX(i,j,k)
     &                      *recip_drF(k)*recip_hFacW(i,j,k,bi,bj)
C          Meridional thickness flux at the southern cell face
           tfluxY(i,j,k) = -fCoriV(i,j)*SlopeY(i,j,k)
     &                     *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO

C     Calculate the thickness flux for other cells (k<Nr)
C     SlopeX and SlopeY are zero in dry cells, so, the bottom boundary
C     condition that the slope is zero is taken care of.
C     We still need to give special treatment for the surface layer however.
      DO k=Nr-1,1,-1
       DO j=1-Oly,sNy+Oly-1
        DO i=1-Olx,sNx+Olx-1
         IF(k.LE.surfk(i,j) .AND. .NOT. GM_K3D_likeGM) THEN
C          We are in the surface layer, so set the thickness flux
C          based on the average slope over the surface layer
C          If we are on the edge of a "cliff" the surface layer at the
C          centre of the grid point could be deeper than the U or V point.  
C          So, we ensure that we always take a sensible slope.
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
     &              *SlopeX(i,j,kk)/hsurf
             ELSE
               tfluxX(i,j,k) = -fCoriU(i,j)*maskW(i,j,k,bi,bj)
     &              *( SlopeX(i,j,kk)-SlopeX(i,j,kk+1) )/hsurf
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
     &              *SlopeY(i,j,kk)/hsurf
             ELSE
               tfluxY(i,j,k) = -fCoriV(i,j)*maskS(i,j,k,bi,bj)
     &              *( SlopeY(i,j,kk)-SlopeY(i,j,kk+1) )/hsurf
             ENDIF
           ELSE
             tfluxY(i,j,k) = zeroRL
           ENDIF

         ELSE
C          We are not in the surface layer, so calculate the thickness
C          flux based on the local isopyncal slope

C          Zonal thickness flux at the western cell face
           tfluxX(i,j,k)=-fCoriU(i,j)*(SlopeX(i,j,k)-SlopeX(i,j,k+1))
     &                    *recip_drF(k)*recip_hFacW(i,j,k,bi,bj)
     &                    *maskW(i,j,k,bi,bj)

C          Meridional thickness flux at the southern cell face
           tfluxY(i,j,k)=-fCoriV(i,j)*(SlopeY(i,j,k)-SlopeY(i,j,k+1))
     &                    *recip_drF(k)*recip_hFacS(i,j,k,bi,bj)
     &                    *maskS(i,j,k,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDDO

C     Calculate gradq
      IF (GM_K3D_likeGM) THEN
C       To make it similar to GM, we ignore beta
        DO k=1,Nr
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
           gradqx(i,j,k) = maskW(i,j,k,bi,bj)*tfluxX(i,j,k)
           gradqy(i,j,k) = maskS(i,j,k,bi,bj)*tfluxY(i,j,k)
          ENDDO
         ENDDO
        ENDDO
        
      ELSE
C       Do not ignore beta
        DO k=1,Nr
         DO j=1-Oly+1,sNy+Oly
          DO i=1-Olx+1,sNx+Olx
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
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx+1,sNx+Olx
         N2W(i,j,k) = maskW(i,j,k,bi,bj)
     &                *( N2(i,j,k)+N2(i-1,j,k) )
         N2S(i,j,k) = maskS(i,j,k,bi,bj)
     &                *( N2(i,j,k)+N2(i,j-1,k) )
        ENDDO
       ENDDO
C     This fudge is necessary to avoid division by zero in gmredi_calc_eigs.
C     It does not affect the end result since it's in the overlap region.
       j=1-Oly
       DO i=1-Olx,sNx+Olx
        N2W(i,j,k) = GM_K3D_minN2
        N2S(i,j,k) = GM_K3D_minN2
       ENDDO
       i=1-Olx
       DO j=1-Oly,sNy+Oly
        N2W(i,j,k) = GM_K3D_minN2
        N2S(i,j,k) = GM_K3D_minN2
       ENDDO
      ENDDO

      IF(GM_K3D_likeGM) THEN
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           KPV(i,j,k) = GM_K3D_constK
          ENDDO
         ENDDO
        ENDDO
      ELSE
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           KPV(i,j,k) = K3D(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
      ENDIF

      IF (.NOT. GM_K3D_smooth) THEN
C       Do not expand K grad(q) => no smoothing
C       May only be done with a constant K, otherwise the 
C       integral constraint is violated.
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
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
     I         iMin,iMax,jMin,jMax,bi,bj,N2W,myThid,
     I         kLow_U,maskW(:,:,:,bi,bj),
     I         hfacW(:,:,:,bi,bj),recip_hfacW(:,:,:,bi,bj),
     I         rLowW(:,:,bi,bj),GM_K3D_NModes,.FALSE.,
     O         dummy,modesW(:,:,:,:,bi,bj))
        ENDIF
        
C       Calculate Xi_m at the west face of a cell
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          DO m=1,GM_K3D_NModes
           XimX(m,i,j) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           DO m=1,GM_K3D_NModes
              XimX(m,i,j) = XimX(m,i,j) 
     &             - maskW(i,j,k,bi,bj)*drF(k)*hfacW(i,j,k,bi,bj)
     &             *KPV(i,j,k)*gradqx(i,j,k)*modesW(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
          
C     Calculate Xi in the X direction at the west face
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           Xix(i,j,k) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           DO m=1,GM_K3D_NModes
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
     I         iMin,iMax,jMin,jMax,bi,bj,N2S,myThid,
     I         kLow_V,maskS(:,:,:,bi,bj),
     I         hfacS(:,:,:,bi,bj),recip_hfacS(:,:,:,bi,bj),
     I         rLowS(:,:,bi,bj), GM_K3D_NModes, .FALSE.,
     O         dummy,modesS(:,:,:,:,bi,bj))
        ENDIF

        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          DO m=1,GM_K3D_NModes
           XimY(m,i,j) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           DO m=1,GM_K3D_NModes
            XimY(m,i,j) = XimY(m,i,j)
     &           - drF(k)*hfacS(i,j,k,bi,bj)
     &           *KPV(i,j,k)*gradqy(i,j,k)*modesS(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
        
C     Calculate Xi for Y direction at the south face
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           Xiy(i,j,k) = zeroRL
          ENDDO
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           DO m=1,GM_K3D_NModes
            Xiy(i,j,k) = Xiy(i,j,k) 
     &           + maskS(i,j,k,bi,bj)*XimY(m,i,j)*modesS(m,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO

C     ENDIF GM_K3D_likeGM
      ENDIF


C     Calculate the eddy induced velocity in the X direction at the west face
      DO k=1,Nr
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx+1,sNx+Olx
         ustar(i,j,k) = -Xix(i,j,k)/coriU(i,j)
        ENDDO
       ENDDO
      ENDDO      

C     Calculate the eddy induced velocity in the Y direction at the south face
      DO k=1,Nr
       DO j=1-Oly+1,sNy+Oly
        DO i=1-Olx+1,sNx+Olx
         vstar(i,j,k) = -Xiy(i,j,k)/coriV(i,j)
        ENDDO
       ENDDO
      ENDDO      

C     ======================================
C     Calculate the eddy induced overturning streamfunction
C     ======================================
#ifdef GM_K3D_PASSIVE
      k=Nr
      DO j=1-Oly,sNy+Oly
       DO i=1-Olx,sNx+Olx
        psistar(i,j,Nr) = -hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
       ENDDO
      ENDDO
      DO k=Nr-1,1,-1
       DO j=1-Oly,sNy+Oly
        DO i=1-Olx,sNx+Olx
           psistar(i,j,k) = psistar(i,j,k+1)
     &          - hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
        ENDDO
       ENDDO
      ENDDO
     
#else

      IF (GM_AdvForm) THEN
        k=Nr
        DO j=1-Oly+1,sNy+1
         DO i=1-Olx+1,sNx+1
          GM_PsiX(i,j,k,bi,bj) = -hfacW(i,j,k,bi,bj)*drF(k)*ustar(i,j,k)
          GM_PsiY(i,j,k,bi,bj) = -hfacS(i,j,k,bi,bj)*drF(k)*vstar(i,j,k)
         ENDDO
        ENDDO
        DO k=Nr-1,1,-1
         DO j=1-Oly+1,sNy+1
          DO i=1-Olx+1,sNx+1
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
        CALL DIAGNOSTICS_FILL(K3D,    'GM_K3D  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(urms,   'GM_URMS ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Rdef,   'GM_RDEF ',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(RRhines,'GM_LRHNS',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Rmix,   'GM_RMIX ',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(supp,   'GM_SUPP ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Xix,    'GM_Xix  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Xiy,    'GM_Xiy  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(cDopp,  'GM_C    ',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(Ubaro,  'GM_UBARO',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(eady,   'GM_EADY ',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(SlopeX, 'GM_Sx   ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(SlopeY, 'GM_Sy   ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(tfluxX, 'GM_TFLXX',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(tfluxY, 'GM_TFLXY',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(gradqx, 'GM_dqdx ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(gradqy, 'GM_dqdy ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(surfkz, 'GM_SFLYR',0, 1,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(ustar,  'GM_USTAR',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(vstar,  'GM_VSTAR',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(umc,    'GM_UMC  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(ubar,   'GM_UBAR ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(modesC(1,:,:,:,bi,bj),
     &                                'GM_MODEC',0,Nr,0,1,1,myThid)
      ENDIF
#endif

#endif /* GM_K3D */
      RETURN
      END
