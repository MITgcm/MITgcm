C $Id: g_calc.F,v 1.3 1998/06/23 13:36:41 adcroft Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"

#define _DB0 0.
#define _DB1 1.
#define _wdvdz 1.D0

C===================================================================
C     Procedure name: G_CALC                                       |
C           Function: Caculates the tendency terms for the momentum|
C                     equations.                                   |
C           Comments:                                              |
C===================================================================
CStartofinterface
      SUBROUTINE G_CALC(
     I                  U, V, W, PH, PS, K,
     U                  RHS2D, divH,
     O                  GU, GV )
       IMPLICIT NONE
C============== Global data ==========================================
#include "SIZE.h"
#include "AJAINF.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OLDG.h"
#include "FORCING.h"
#include "MASKS.h"
C============= Routine arguments =====================================
C     /--------------------------------------------------------------\
C     |U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                  |
C     |PH      - Hydrostatic pressure perturbation (m).              |
C     |PS      - Surface pressure (m).                               |
C     |GU      - d/dt U (m/s/s).                                     |
C     |GU      - d/dt V (m/s/s).                                     |
C     |RHS2D   - Contribution to righ-hand side of                   |
C     |          pressure eqaution.                                  |
C     |divH    - Contribution to barotropic divergence.              |
C     \--------------------------------------------------------------/
      REAL    U (nx,ny,nz)
      REAL    V (nx,ny,nz)
      REAL    W (nx,ny,nz)
      REAL    PH(nx,ny,nz)
      REAL    PS(nx,ny   )
      REAL    GU(nx,ny,nz)
      REAL    GV(nx,ny,nz)
      REAL    RHS2D(nx,ny)
      REAL    divH (nx,ny)
      INTEGER K
CEndofinterface
C============= Local variables  ======================================
C     /--------------------------------------------------------------\
C     |Slice Notation                                                |
C     |phiK   - XY slice of variable "phi" at level K.               |
C     |phiKP1 - XY slice of variable "phi" at level K+1.             |
C     |phiKM1 - XY slice of variable "phi" at level K-1.             |
C     |phiNM1 - Variable "phi" at time level N-1.                    |
C     |phiU   - Varaible "phi" interpolated to U grid.               |
C     |phiV   - Varaible "phi" interpolated to V grid.               |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     |Geometry terms                                                |
C     | o xa,ya,za - X, Y, Z face areas ( m.Pa, m.Pa, m**2 ).        |
C     | o rV       - 1/Volume ( m**2.Pa )                            |
C     | o ?Grad?Op - Generic gradient operator A[XYZ][uvwp].d/d[xyz] |
C     |              uGradxOp: AXu/DXu, uGradyOp: AYu/DYu etc...     |
C     \--------------------------------------------------------------/
      REAL xaK   (nx+1,0:ny)
      REAL yaK   (0:nx,ny+1)
      REAL uGradxOp   (nx  ,ny  )
      REAL uGradyOp   (nx  ,ny  )
      REAL uGradzOpK  (nx  ,ny  )
      REAL uGradzOpKP1(nx  ,ny  )
      REAL vGradxOp   (nx  ,ny  )
      REAL vGradyOp   (nx  ,ny  )
      REAL vGradzOpK  (nx  ,ny  )
      REAL vGradzOpKP1(nx  ,ny  )
      REAL zaK   (0:nx,0:ny)
      REAL zaKP1 (0:nx,0:ny)
      REAL rVk   (nx  ,ny  )
C     /--------------------------------------------------------------\
C     |State variables                                               |
C     | o u, v, w - X,Y,Z velocity slices.                           |
C     | o ph      - Hydrostatic pressure.                            |
C     \--------------------------------------------------------------/
      REAL uK    (nx+1,0:ny)
      REAL uMskK (nx+1,0:ny)
      REAL uKNM1 (nx+1,0:ny)
      REAL wK    (0:nx,0:ny)
      REAL wkP1  (0:nx,0:ny)
      REAL phK   (nx  ,ny  )
      REAL vK    (0:nx,ny+1)
      REAL vMskK (nx+1,0:ny)
      REAL vKNM1 (0:nx,ny+1)
      REAL vKP1  (nx  ,ny  )
      REAL vKM1  (nx  ,ny  )
C     /--------------------------------------------------------------\
C     |Work space arrays                                             |
C     \--------------------------------------------------------------/
      REAL tmp   (0:nx+1,0:ny+1)
      REAL tmp2  (0:nx+1,0:ny+1)
      REAL stmp
C     /--------------------------------------------------------------\
C     |Arrays for accumulating fluxes                                |
C     | o fX, fY, fZ - Flux in X, Y and Z.                           |
C     \--------------------------------------------------------------/
      REAL fX    (0:nx+1,ny)
      REAL fY    (nx,0:ny+1)
      REAL fZK   (nx,ny)
      REAL fZKP1 (nx,ny)
C     Loop counters:
      INTEGER I, J
C     Flags:
      LOGICAL TOP_LAYER
      LOGICAL BOTTOM_LAYER
C === Set up controlling flags ========================================
      TOP_LAYER    = K .EQ. 1
      BOTTOM_LAYER = K .EQ. nz
C === Set up the slices ===============================================
C     /---------------------------------------------------------------\
C     | xaK       <- xa(K)                                            |
C     | yaK       <- ya(K)                                            |
C     | za[K,KP1] <- za[(K),(K+1)]                                    |
C     \---------------------------------------------------------------/
      xaK (1:nx,1:ny) = xa  (_I3(K,:,:))
      xaK (nx+1,1:ny) = xaK (1,1:ny    )
      xaK (:,0      ) = xaK (:,ny      )
      yaK (1:nx,1:ny) = ya  (_I3(K,:,:))
      yaK (0,1:ny   ) = yaK (nx,1:ny   )
      yaK (:,ny+1   ) = yaK (:,1       )
      zaK (1:nx,1:ny) = za  (_I3(K,:,:))
      zaK (0,1:ny   ) = zaK (nx,1:ny   )
      zaK (:,0      ) = zaK (:,ny      )
      IF ( .NOT. BOTTOM_LAYER ) THEN
       zaKP1(1:nx,1:ny) = za   (_I3(K+1,:,:))
       zaKP1(0,1:ny   ) = zaKP1(nx,1:ny     )
       zaKP1(:,0      ) = zaKP1(:,ny        )
      ENDIF
C     /---------------------------------------------------------------\
C     | uGradXOp        <- 1/DXu*Bx{xa}                               |
C     | uGradYOp        <- 1/DYu*Bx{ya}                               |
C     | uGradZOp[K,KP1] <- 1/DZu*Bz{ZA[K,K+1]}                        |
C     \---------------------------------------------------------------/
      DO J = 1, ny
       DO I = 1, nx
        uGradxOp(I,J)  = 0.5*( xaK(I,J)+xaK(I+1,J) )*uDdxOp(_I3(K,I ,J))
       ENDDO
      ENDDO
      DO J = 1, ny
       DO I = 2, nx  
        uGradyOp(I,J)  = 0.5*( yaK(I,J)+yaK(I-1,J) )*uDdyOp(_I3(K,I,J))
        uGradzOpK(I,J) = 
     &         0.5*(ZA(_I3(K,I,J))+ZA(_I3(K,I-1,J)))*uDdzOp(_I3(K,I,J))
       ENDDO
        uGradyOp(1,J)  = 0.5*( yaK(1,J)+yaK(nx,J) )*uDdyOp(_I3(K,1,J))
       uGradzOpK(1,J)  = 
     &         0.5*(ZA(_I3(K,1,J))+ZA(_I3(K,nx,J)))*uDdzOp(_I3(K,1,J))
      ENDDO
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, ny
        DO I = 2, nx  
         uGradzOpKP1(I,J) = 
     &    0.5*(ZA(_I3(K+1,I,J))+ZA(_I3(K+1,I-1,J)))*uDdzOp(_I3(K+1,I,J))
        ENDDO
        uGradzOpKP1(1,J)  = 
     &   0.5*( ZA(_I3(K+1,1,J))+ZA(_I3(K+1,nx,J)) )*uDdzOp(_I3(K+1,1,J))
       ENDDO
      ENDIF
C     /---------------------------------------------------------------\
C     | vGradXOp        <- 1/DXu*By{xa}                               |
C     | vGradYOp        <- 1/DYu*By{ya}                               |
C     | vGradZOp[K,KP1] <- 1/DZu*Bz{ZA[(K),(K+1)]}                    |
C     \---------------------------------------------------------------/
      DO J = 2, ny
       DO I = 1, nx
        vGradxOp(I,J) = 0.5*( xaK(I,J)+xaK(I,J-1) )*vDdxOp(_I3(K,I,J))
        vGradzOpK(I,J)= 
     &        0.5*(ZA(_I3(K,I,J))+ZA(_I3(K,I,J-1)))*vDdzOp(_I3(K,I,J))
       ENDDO
      ENDDO
      DO I = 1, nx
       vGradxOp (I,1) = 0.5*( xaK(I,1)+xaK(I,ny) )*vDdxOp(_I3(K,I,1 ))
       vGradzOpK(I,1) = 0.5*(ZA(_I3(K,I,1))+ZA(_I3(K,I,ny)))*vDdzOp(_I3(K,I,1))
      ENDDO
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 2, ny
        DO I = 1, nx
         vGradzOpKP1(I,J) = 
     &   0.5*(ZA(_I3(K+1,I,J))+ZA(_I3(K+1,I,J-1)))*vDdzOp(_I3(K+1,I,J))
        ENDDO
       ENDDO
       DO I = 1, nx
        vGradzOpKP1(I,1) = 
     &  0.5*(ZA(_I3(K+1,I,1))+ZA(_I3(K+1,I,ny)))*vDdzOp(_I3(K+1,I,1))
       ENDDO
      ENDIF
      DO J = 1, ny-1
       DO I = 1, nx
        vGradyOp(I,J) = 0.5*( yaK(I,J)+yaK(I,J+1) )*vDdyOp(_I3(K,I,J))
       ENDDO
      ENDDO
      DO I = 1, nx
       vGradyOp(I,ny) = 0.5*( yaK(I,ny)+yaK(I,1) )*vDdyOp(_I3(K,I,ny))
      ENDDO
C     /---------------------------------------------------------------\
C     | w[K,KP1] <- W[(K),(K+1)]                                      |
C     \---------------------------------------------------------------/
      wKP1 = 0.
      wK(1:nx,1:ny)  = W(_I3(K,:,:))
      wK(0,1:ny)     = wK(nx,1:ny)
      wK(:,0)        = wK(:,ny)
      IF ( .NOT. BOTTOM_LAYER ) wKP1(1:nx,1:ny) = W(_I3(K+1,:,:))
      wKP1(0,1:ny)     = wKP1(nx,1:ny)
      wKP1(:,0)        = wKP1(:,ny)
C     /---------------------------------------------------------------\
C     | uK <- U(K)                                                    |
C     \---------------------------------------------------------------/
      uK(1:nx,1:ny)    = U(_I3(K,:,:))
      uK(nx+1,1:ny)    = uK(1,1:ny)
      uK(:,0)          = uK(:,ny)
      uKNM1(1:nx,1:ny) = UNM1(_I3(K,:,:))
      uKNM1(nx+1,1:ny) = uKNM1(1,1:ny)
      uKNM1(:,0)       = uKNM1(:,ny)
      uMskK(1:nx,1:ny)    = UMASK(_I3(K,:,:))
      uMskK(nx+1,1:ny)    = uMskK(1,1:ny)
      uMskK(:,0)          = uMskK(:,ny)
C     /---------------------------------------------------------------\
C     | v[KM1,K,KP1] = V[(K-1),(K),(K+1)]                             |
C     \---------------------------------------------------------------/
      vKP1 = 0.
      vKM1 = 0.
      vK(1:nx,1:ny)   = V(_I3(K,:,:))
      vK(0,1:ny) = vK(nx,1:ny)
      vK(:,ny+1) = vK(:,1)
      vKNM1(1:nx,1:ny) = VNM1(_I3(K,:,:))
      vKNM1(0,1:ny)    = vKNM1(nx,1:ny)
      vKNM1(:,ny+1)    = vKNM1(:,1)
      vMskK(1:nx,1:ny)   = VMASK(_I3(K,:,:))
      vMskK(0,1:ny) = vMskK(nx,1:ny)
      vMskK(:,ny+1) = vMskK(:,1)
      IF ( .NOT. TOP_LAYER    ) vKM1(1:nx,1:ny) = V(_I3(K-1,:,:))
      IF ( .NOT. BOTTOM_LAYER ) vKP1(1:nx,1:ny) = V(_I3(K+1,:,:))
C     /---------------------------------------------------------------\
C     | Gu Section                                                    |
C     |===============================================================|
C     | In this section  Gu =   GuDiffusion                           |
C     |                       + GuAdvection                           |
C     |                       + GuMetric                              |
C     |                       + GuXZCoriolis                          |
C     |                       + GuForcing                             |
C     \---------------------------------------------------------------/
C     /---------------------------------------------------------------\
C     | Initialise local terms                                        |
C     |===============================================================|
C     | rVk <- 1/Vu                                                   |
C     \---------------------------------------------------------------/
      rVk   = rUvol(_I3(K,:,:))
      fZK   = 0.
      fZKP1 = 0.
C     /---------------------------------------------------------------\
C     | XY GuDiffusion + XY GuAdvection                               |
C     |===============================================================|
C     | Note:  XY GuDiffusion = Laplacian + Biharmonic in XY-plane.   |
C     |        o Laplacian  ( -a2MomXY.del^2(u) )                     |
C     |        o Biharmonic ( +a4MomXY.del^4(u) )                     |
C     \---------------------------------------------------------------/
      DO J = 1, ny
       DO I = 1, nx
C       /-------------------------------------------------------------\
C       | fX <- 1/DXu*XAu*Ddx{U}                                      |
C       | fY <- 1/DYu*YAu*Ddy{U}                                      |
C       \-------------------------------------------------------------/
        fX(I,J) = uGradxOp(I,J)*(uK(I+1,J)-uK(I,J))
        fY(I,J) = uGradyOp(I,J)*(uK(I,J)-uK(I,J-1))
       ENDDO
      ENDDO
      fX(0   ,1:ny) = fX(nx,1:ny)
      fY(1:nx,ny+1) = fY(1:nx,1)
C     WRITE(0,*) ' CALC_G fY '
C     CALL PLOT_FIELD( fY(1:nx,1:ny), nX, nY )
      DO J=1,ny
       DO I=1,nx
C       /-------------------------------------------------------------\
C       | tmp <- 1/Vu*(Dx{fX}+Dy{fY})                                 |
C       \-------------------------------------------------------------/
        tmp(I,J)=rVk(I,J)*fX(I,J)-rVk(I,J)*fX(I-1,J)
     &          +rVk(I,J)*fY(I,J+1)-rVk(I,J)*fY(I,J)
       ENDDO
      ENDDO
      tmp(nx+1,1:ny) = tmp(1,1:ny)
      tmp(:,0) = tmp(:,ny)
      DO J = 1,ny
       DO I = 1,nx
C       /-------------------------------------------------------------\
C       | Diffusive and advective flux at eastern face                |
C       |=============================================================|
C       | fX  <- -k2*fX               +   (  Laplacian,<Line 1>   )   |
C       |         k4/DXu*XAu*Ddx{tmp} +   (  Biharmonic,<Line 2>  )   |
C       |         Bx{XA*U}*Bx{U}          (  Advection,<Line 3-4> )   |
C       \-------------------------------------------------------------/
        fX(I,J) = 0.
     _LPM(&       -_DB1*a2MomXY*fX(I,J)                                    )
     _BHM(&       +a4MomXY*uGradxOp(I,J)*(tmp(I+1,J)-tmp(I,J))        )
     _ADM(&       +0.25D0*(uK(I,J)*XAK(I,J)+uK(I+1,J)*XAK(I+1,J))*    )
     _ADM(&        (uK(I,J)+uK(I+1,J))                                )
C       /-------------------------------------------------------------\
C       | Diffusive and advective flux at southern face               |
C       |=============================================================|
C       | fY  <- -k2*fY               +   (  Laplacian,<Line 1>   )   |
C       |         k4/DYu*YAu*Ddy{tmp} +   (  Biharmonic,<Line 2>  )   |
C       |         Bx{YA*V}*By{U}          (  Advection,<Line 3-4> )   |
C       \-------------------------------------------------------------/
        fY(I,J) = 0.
     _LPM(&            -_DB1*a2MomXY*fY(I,J)                               )
     _BHM(&            +a4MomXY*uGradyOp(I,J)*(tmp(I,J)-tmp(I,J-1))   )
     _ADM(&            +0.25D0*(uK(I,J)+uK(I,J-1))*                   )
     _ADM(&             (vK(I,J)*YAK(I,J)+vK(I-1,J)*YAK(I-1,J))       )
     _ADM(&             *uMskK(I,J)*uMskK(I,J-1)                      )
C    Note!!!! The last line above "*uMskK(I,J)*uMskK(I,J-1)" is the
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
       ENDDO
      ENDDO

C     /---------------------------------------------------------------\
C     | Z GuDiffusion + Z GuAdvection                                 |
C     |===============================================================|
C     \---------------------------------------------------------------/
      IF ( .NOT. TOP_LAYER ) THEN
       DO J = 1, ny
        DO I = 1, nx
C        /------------------------------------------------------------\
C        | Diffusive and advective flux at upper face                 |
C        |============================================================|
C        | fZK <- -k2Z/DZu*ZAu*Ddz{U} + (  Laplacian,<Line 1>   )     |
C        |           Bx{ZA*-w}*Bz{U}    (  Advection,<Line 2-3> )     |
C        \------------------------------------------------------------/
         fZK(I,J) = 0.
     _LPM(&  -_DB1*a2MomZ*uGradzOpK(I,J)*( U(_I3(K-1,I,J))-uK(I,J) )       )
     _ADM(&  +0.25D0*(uK(I,J)+U(_I3(K-1,I,J)))*                       )
     _ADM(&  (-wK(I,J)*ZAK(I,J)-wK(I-1,J)*ZAK(I-1,J))                 )
     _ADM(&  *uMskK(I,J)*UMASK(_I3(K-1,I,J))                          )
C    Note!!!! The last line above "*uMskK(I,J)*UMASK(_I3(K-1,I,J))" is
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
        ENDDO
       ENDDO
      ELSE
       DO J = 1, ny
        DO I = 1, nx
C        /------------------------------------------------------------\
C        | Advective flux at top layer                                |
C        |============================================================|
C        | o Only active when using implcit free surface.             |
C        \------------------------------------------------------------/
         fZK(I,J) = 0.
     _ADM(&  +freeSurfFac*0.25D0*(uK(I,J)+uK(I,J) )*                  )
     _ADM(&  (-wK(I,J)*ZAK(I,J)-wK(I-1,J)*ZAK(I-1,J))                 )
        ENDDO
       ENDDO
      ENDIF
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, ny
        DO I = 1, nx
C        /------------------------------------------------------------\
C        | Diffusive and advective flux at lower face                 |
C        |============================================================|
C        | fZKP1 <- -k2Z/DZu*ZAu*Ddz{U} +  (  Laplacian,<Line 1>   )  |
C        |              Bx{ZA*-w}*Bz{U}    (  Advection,<Line 2-3> )  |
C        \------------------------------------------------------------/
         fZKP1(I,J) = 0.
     _LPM(&    -_DB1*a2MomZ*uGradzOpKP1(I,J)*(uK(I,J)-U(_I3(K+1,I,J)))     )
     _ADM(&    +0.25D0*(uK(I,J)+U(_I3(K+1,I,J)))*                     )
     _ADM(&    (-wKP1(I,J)*ZAKP1(I,J)-wKP1(I-1,J)*ZAKP1(I-1,J))       )
     _ADM(&    *uMskK(I,J)*UMASK(_I3(K+1,I,J))                        )
C    Note!!!! The last line above "*uMskK(I,J)*UMASK(_I3(K-1,I,J))" is
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
        ENDDO
       ENDDO
      ELSE
       DO J = 1, ny
        DO I = 1, nx
         fZKP1(I,J) = 0.
     _LPM(&    -_DB1*a2MomZ*rDzAtP(K)*2.D0*uK(I,J)                         )
     _LPM(&    *0.5*( ZAk(I,J)+ZAk(I-1,J) )                                )
        ENDDO
       ENDDO
      ENDIF
C     /---------------------------------------------------------------\
C     | gU <- GuDiffusion + GuAdvection = -div(fX,fY,fZ)              |
C     \---------------------------------------------------------------/
      fX(0,1:ny) = fX(nx,1:ny)
      fY(:,ny+1) = fY(:,1)
      DO J = 1, ny
       DO I = 1, nx
C       /-------------------------------------------------------------\
C       | o Minus divergence of the lateral fluxes                    |
C       | gU <- -1/Vu*(Ddx{fX}+Ddy{fY})                               |
C       \-------------------------------------------------------------/
        gU(_I3(K,I,J))=-fX(I,J)*rVk(I,J)+fX(I-1,J)*rVk(I,J)
     &                 -fY(I,J+1)*rVk(I,J)+fY(I,J)*rVk(I,J)
       ENDDO
      ENDDO
      DO J = 1, ny
       DO I = 1, nx
C       /------------------------------------------------------------\
C       | o Minus flux at upper face                                 |
C       | gU <- gU-1/Vu*fZK                                          |
C       \------------------------------------------------------------/
        gU(_I3(K,I,J))=gU(_I3(K,I,J))-fZK(I,J)*rVk(I,J)
       ENDDO
      ENDDO
CcnhDebugStarts
C     Changed to make noslip bottom boundary.
Cdbg  IF ( .NOT. BOTTOM_LAYER ) THEN
CcnhDebugEnds
       DO J = 1, ny
        DO I = 1, nx
C        /------------------------------------------------------------\
C        | o Plus flux at lower face                                  |
C        | gU <- gU+1/Vu*fZP1                                         |
C        \------------------------------------------------------------/
         gU(_I3(K,I,J))=gU(_I3(K,I,J))+fZKP1(I,J)*rVk(I,J)
        ENDDO
       ENDDO
CcnhDebugStarts
C     Changed to make noslip bottom boundary.
Cdbg  ENDIF
CcnhDebugEnds

C     /---------------------------------------------------------------\
C     | gU <- gU + GuMetric + GuXZCoriolis                            |
C     \---------------------------------------------------------------/
      DO J = 1, ny
       DO I = 1, nx
#       ifdef _SPHERICAL_POLAR_METRIC_TERMS
C       /-------------------------------------------------------------\
C       | o Spherical polar metric terms for U                        |
C       |=============================================================|
C       | gU <- gU - U/aEarth*BzBx{-w/G/RONIL} - U*tan(lat)*ByBx{V}   |
C       \-------------------------------------------------------------/
        gU(_I3(K,I,J)) = gU(_I3(K,I,J))
     _SPM(& -uMask(_I3(K,I,J))*uK(I,J)/rZero*                         )
     _SPM(&0.25D0*(wK(I-1,J)+wK(I,J)+wKP1(I-1,J)+wKP1(I,J))/(-G*RONIL))
     _SPM(& +uMask(_I3(K,I,J))*uK(I,J)*uTphiOP(_I3(K,I,J))/rZero      )
     _SPM(&  *0.25D0*(vK(I,J)+vK(I-1,J)+vK(I,J+1)+vK(I-1,J+1))        )
#       endif
C       /-------------------------------------------------------------\
C       | o XZ-plane coriolis terms for U                             |
C       |=============================================================|
C       | gU <- gU - 2*omega*cos(lat)*BzBx{-w/G/RONIL}                |
C       \-------------------------------------------------------------/
        gU(_I3(K,I,J)) = gU(_I3(K,I,J))
     _XZC(& -fCorW(_I3(K,I,J))/                                       )
     _XZC(&G/(-RONIL)*0.25D0*(wK(I-1,J)+wK(I,J)+wKP1(I-1,J)+wKP1(I,J)))
       ENDDO
      ENDDO

#ifdef _MOMENTUM_FORCING
C     /---------------------------------------------------------------\
C     | gU <- gU + GuForcing                                          |
C     \---------------------------------------------------------------/
      IF ( TOP_LAYER ) THEN
C      /--------------------------------------------------------------\
C      | o Add a drag term to the top-layer of U                      |
C      |   u* is the velocity of the disk relative to the surface.    |
C      |   lambda is the inverse of the time scale with which the     |
C      |   flow equilibrates withthe rotating disk.                   |
C      |   to get lambda we use the following                         |
C      |    du/dt = stress/rho/dz                                     |
C      |    stress = rho.Kz.DU/DZ                                     |
C      |    DZ = (2Kz/f)^1/2  (Ekman layer thiickness )               |
C      |    DU = (U-u*)                                               |
C      |    F=1e-4, Kz=1e-3, dz of 25m gives an equilibration time    |
C      |    scale of 1.3 days.                                        |
C      | gU(K=1) <- gU(K=1) - lambda(U-u*)                            |
C      \--------------------------------------------------------------/
       gU(_I3(K,:,:)) = gU(_I3(K,:,:)) 
     &  + fu
      ENDIF
#endif

C *********************************************************************
C ****Gv SECTION*******************************************************
C *********************************************************************
C     In this section  Gv =   GvDiffusion
C                           + GvAdvection
C                           + GvMetric
C                           + GvHorizontalCoriolis
C                           + GvForcing
C === Initialise local operators ======================================
C     rVk: 1/VVolume
      fZK   = 0.
      fZKP1 = 0.
      rVk = rVvol(_I3(K,:,:))
C === Lateral diffusive flux of V =====================================
C     o Laplacian  ( -a2MomXY.del^2(v) )
C     o Biharmonic ( +a4MomXY.del^4(v) )
      DO J = 1, ny
       DO I = 1, nx
C       fX: VAreaX*dx[V]
C       fY: VAreaY*dy[V]
        fX(I,J) = vGradxOp(I,J)*(vK(I,J)-vK(I-1,J))
        fY(I,J) = vGradyOp(I,J)*(vK(I,J+1)-vK(I,J))
       ENDDO
      ENDDO
      fX(nx+1,1:ny) = fX(1 ,1:ny)
      fY(1:nx,0   ) = fY(1:nx,ny)
      DO J=1,ny
       DO I=1,nx
C       o Del^2 V ( for use in biharmonic mixing ).
C       tmp: div(fX,fY) = 1/VVolume*(dx[fX]+dy[fy])
        tmp(I,J)= fX(I+1,J)*rVk(I,J)-fX(I,J)*rVk(I,J)
     &           +fY(I,J)*rVk(I,J)-fY(I,J-1)*rVk(I,J)
       ENDDO
      ENDDO
      tmp(0,1:ny) = tmp(nx,1:ny)
      tmp(:,ny+1) = tmp(:,1)
      DO J = 1,ny
       DO I = 1,nx
C       o Laplacian<Line 1> + Biharmonic<Line 2> + Advection<Line 3,4>.
C       fX: -K2xyMomentum*fX + K4xyMomentum*VAreaX*dx[tmp] + By[Ax*U]*Bx[V]
C       fY: -K2xyMomentum*fY + K4xyMomentum*VAreaY*dy[tmp] + By[Ay*V]*By[V]
        fX(I,J) = 0.
     _LPM(&            -_DB1*a2MomXY*fX(I,J)                               )
     _BHM(&            +a4MomXY*vGradxOp(I,J)*(tmp(I,J)-tmp(I-1,J))        )
     _ADM(&            +0.25D0*(uK(I,J-1)*XAK(I,J-1)+uK(I,J)*XAK(I,J))*    )
     _ADM(&             (vK(I-1,J)+vK(I,J))                                )
     _ADM(&            *vMskK(I,J)*vMskK(I-1,J)                            )
C    Note!!!! The last line above "*vMskK(I,J)*vMskK(I-1,J)"        is
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
        fY(I,J) = 0.
     _LPM(&            -_DB1*a2MomXY*fY(I,J)                               )
     _BHM(&            +a4MomXY*vGradyOp(I,J)*(tmp(I,J+1)-tmp(I,J))        )
     _ADM(&            +0.25D0*(vK(I,J+1)*YAK(I,J+1)+vK(I,J)*YAK(I,J))*    )
     _ADM(&             (vK(I,J+1)+vK(I,J))                                )
C    Note!!!! The last line above "*vMskK(I,J)*vMskK(I-1,J)"        is
       ENDDO
      ENDDO
C === Vertical diffusive flux of V ====================================
C === Vertical advective flux of V ====================================
C     o Laplacian Diffusion<Line 1> + Advection<Line 2,3>
C     fZK: -KzMomentum*VAreaZ*dz[V] + Bz[V]*By[Az*W]
      IF ( .NOT. TOP_LAYER    ) THEN
c      WRITE(0,*) ' fZK subsurface layer '
       DO J = 1, ny
        DO I = 1, nx
         fZK(I,J) = 0.
     _LPM(&  -_DB1*a2MomZ*vGradzOpK(I,J)*(V(_I3(K-1,I,J))-vK(I,J) )            )
     _ADM(&  +0.25D0*(vK(I,J)+V(_I3(K-1,I,J)))*                           )
     _ADM(&               (-wK(I,J)*ZAK(I,J)-wK(I,J-1)*ZAK(I,J-1))        )
     _ADM(&            *vMskK(I,J)*vMask(_I3(K-1,I,J))*_wdvdz             )
C    Note!!!! The last line above "*vMskK(I,J)*vMask(_I3(K-1,I,J))" is
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
        ENDDO
       ENDDO
      ELSE
c      WRITE(0,*) ' fZK top layer '
       DO J = 1, ny
        DO I = 1, nx
         fZK(I,J) = 0.D0
     _ADM(&  +freeSurfFac*0.25D0*(vK(I,J)+vK(I,J))                        )
     _ADM(&              *(-wK(I,J)*ZAK(I,J)-wK(I,J-1)*ZAK(I,J-1))*_wdvdz )
        ENDDO
       ENDDO
      ENDIF
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, ny
        DO I = 1, nx
         fZKP1(I,J) = 0.
     _LPM(& -_DB1*a2MomZ*vGradzOpKP1(I,J)*( vK(I,J)-V(_I3(K+1,I,J)))           )
     _ADM(& +0.25D0*(vK(I,J)+V(_I3(K+1,I,J)))*                            )
     _ADM(& (-wKP1(I,J)*ZAKP1(I,J)-wKP1(I,J-1)*ZAKP1(I,J-1))              )
     _ADM(& *vMskK(I,J)*vMask(_I3(K+1,I,J))*_wdvdz                        )
C    Note!!!! The last line above "*vMskK(I,J)*vMask(_I3(K+1,I,J))" is
C    Note!!!! boundary condition in the standard v010 CM5 code. The
C    Note!!!! FV paper used a different bc in which the last line is
C    Note!!!! omitted.
        ENDDO
       ENDDO
      ELSE    
       DO J = 1, ny
        DO I = 1, nx
         fZKP1(I,J) = 0.
     _LPM(&    -_DB1*a2MomZ*rDzAtP(K)*2.*vK(I,J)                           )
     _LPM(&    *0.5*( ZAk(I,J)+ZAk(I,J-1) )                                )

        ENDDO
       ENDDO
      ENDIF
c     WRITE(0,*) 'K, MAXVAL(fZK)   = ', K, MAXVAL(fZK)
c     WRITE(0,*) 'K, MAXVAL(fZKP1) = ', K, MAXVAL(fZKP1)
C === Combine advective and diffusive fluxes of V =====================
C     o V tendency is minus divergence of diffusive and advective flux.
C     gV:  -div(fX,fY,fZ) = -1/VVolume*(dx[fX]+dy[fY]+fZK-fZKP1)
      fX(nx+1,1:ny) = fX(1,1:ny)
      fY(:,0) = fY(:,ny)
      DO I = 1, nx
       DO J = 1, ny
        gV(_I3(K,I,J))=-fX(I+1,J)*rVK(I,J)+fX(I,J)*rVK(I,J)
     &                 -fY(I,J)*rVK(I,J)+fY(I,J-1)*rVK(I,J)
       ENDDO
      ENDDO
      DO J = 1, ny
       DO I = 1, nx
        gV(_I3(K,I,J))=gV(_I3(K,I,J))-fZK(I,J)*rVK(I,J)
       ENDDO
      ENDDO
CcnhDebugStarts
C     Changed to provide noslip bottom boundary
Cdbg  IF ( .NOT. BOTTOM_LAYER ) THEN
CcnhDebugEnds
       DO J = 1, ny
        DO I = 1, nx
         gV(_I3(K,I,J))=gV(_I3(K,I,J))+fZKP1(I,J)*rVK(I,J)
        ENDDO
       ENDDO
CcnhDebugStarts
Cdbg  ENDIF
CcnhDebugEnds
C === Metric terms due to curvilinear coordinates =====================
      DO J = 1, ny
       DO I = 1, nx
C       o Spherical polar metric terms.
C       gV:  gV - V/radius*ByBz[w]+tan(latitude)*BxBy[u]
        gV(_I3(K,I,J)) = gV(_I3(K,I,J))
     _SPM(&   -vMask(_I3(K,I,J))*vK(I,J)/rZero*                            )
     _SPM(&  0.25D0*(wK(I,J-1)+wK(I,J)+wKP1(I,J-1)+wKP1(I,J))/(-G*RONIL)   )
     _SPM(&   -vMask(_I3(K,I,J))*vTphiOP(_I3(K,I,J))/rZero*0.25D0*0.25D0   )
     _SPM(&   *(uK(I,J)+uK(I+1,J)+uK(I,J-1)+uK(I+1,J-1))                   )
     _SPM(&   *(uK(I,J)+uK(I+1,J)+uK(I,J-1)+uK(I+1,J-1))                   )
       ENDDO
      ENDDO

#ifdef _MOMENTUM_FORCING
      IF ( TOP_LAYER ) THEN
       gV(_I3(K,:,:)) = gV(_I3(K,:,:)) 
     &  + fv
      ENDIF
#endif

C     Hydrostatic pressure term
      DO J=1,Ny
       DO I=1,Nx
        tmp(I,J)=G*PH(_I3(K,I,J))
       ENDDO
      ENDDO
      DO J=1,Ny
       tmp(0,J)=tmp(Nx,J)
      ENDDO
      DO I=1,Nx
       tmp(I,0)=tmp(I,Ny)
      ENDDO
      DO J = 1, ny
       DO I = 1, nx
        gU(_I3(K,I,J)) = gU(_I3(K,I,J)) -pDdxOp(_I3(K,I,J))*(tmp(I,J)-tmp(I-1,J))
        gV(_I3(K,I,J)) = gV(_I3(K,I,J)) -pDdyOp(_I3(K,I,J))*(tmp(I,J)-tmp(I,J-1))
       ENDDO
      ENDDO


#ifdef _XY_CORIOLIS
C     /---------------------------------------------------------------\
C     | Cd Scheme                                                     |
C     |===============================================================|
C     | o Gu = Gu + GuXYCoriolis                                      |
C     | o Gv = Gv + GvXYCoriolis                                      |
C     \---------------------------------------------------------------/
C     /---------------------------------------------------------------\
C     | tmp2 <- P(n) = P extrapolated to time level n.                |
C     \---------------------------------------------------------------/
      tmp2(1:nx,1:ny) =  (1.5+abEps)*(pS+pH(_I3(K,:,:)) )
     &                 -(0.5+abEps)*(psNM1+phNM1(_I3(K,:,:)) )
      tmp2(1:nx,1:ny) =  (1.5+abEps)*(pS )
     &                 -(0.5+abEps)*(psNM1 )
      tmp2(0,1:ny)    = tmp2(nx,1:ny)
      DO J = 1,ny
       DO I = 1,nx
C       /-------------------------------------------------------------\
C       | tmp <- -G*Ddx{P(n)} + gU                                    |
C       \-------------------------------------------------------------/
        tmp(I,J) = 
     &   -G*pDdxOp(_I3(K,I,J))*(tmp2(I,J)-tmp2(I-1,J))+
     &    gU(_I3(K,I,J))*uMask(_I3(K,I,J))
       ENDDO
      ENDDO
      tmp(nx+1,1:ny)    = tmp(1,1:ny)
      tmp(:,0      )    = tmp(:,ny  )
      DO J = 1,ny
       DO I = 1,nx
C       /-------------------------------------------------------------\
C       | uAja <- uAja + DT*BxBy{-d/dx P(n) + gU}+f*V                 |
C       \-------------------------------------------------------------/
        uAja(_I3(K,I,J)) = uAja(_I3(K,I,J))
     &  +1.*DelT*(0.*tmp(I,J)+
     &          1.*0.25D0*(tmp(I,J)+tmp(I+1,J)+tmp(I,J-1)+tmp(I+1,J-1))
     &         +fCorV(_I3(K,I,J))*V(_I3(K,I,J)) )
C       uAja: uAja - lambda(uAja-BxBy[U(n+0.5)] ( note rAja = (1-lambda) )
        uAja(_I3(K,I,J)) = rAja*uAja(_I3(K,I,J))
     &   +(1.-rAja)*( (1.5+abEps)*0.25D0*(uK(I,J)+uK(I+1,J)+uK(I,J-1)+uK(I+1,J-1))
     &    -(0.5+abEps)*0.25D0*(uKNM1(I,J)+uKNM1(I+1,J)+uKNM1(I,J-1)+uKNM1(I+1,J-1)) )
       ENDDO
      ENDDO
CcnhDebugStarts
C     WRITE(0,*) ' CG2D MAXVAL(uAja) ', MAXVAL(uAja)
CcnhDebugEnds
C     tmp2: P(n) = P extrapolated to time level n.
      tmp2(1:nx,1:ny) =  (1.5+abEps)*(pS+pH(_I3(K,:,:)) )
     &                  -(0.5+abEps)*(psNM1+phNM1(_I3(K,:,:)) )
      tmp2(1:nx,1:ny) =  (1.5+abEps)*(pS )
     &                  -(0.5+abEps)*(psNM1 )
      tmp2(1:nx,0)    = tmp2(1:nx,ny)
      DO J = 1,ny
       DO I = 1,nx
C       tmp: -d/dy P(n) + gV
        tmp(I,J) = -G*pDdyOp(_I3(K,I,J))*(tmp2(I,J)-tmp2(I,J-1))+
     &              gV(_I3(K,I,J))*vMask(_I3(K,I,J))
       ENDDO
      ENDDO
      tmp(0,1:ny) = tmp(nx,1:ny)
      tmp(:,ny+1) = tmp(: ,1   )
      DO J = 1,ny
       DO I = 1,nx
C       vAja: vAja + DT*(BxBy[-d/dy P(n) + gV]-f*U)
        vAja(_I3(K,I,J)) = vAja(_I3(K,I,J))
     &  +DelT*(0.25D0*(tmp(I-1,J)+tmp(I,J)+tmp(I-1,J+1)+tmp(I,J+1))
     &         -fCorU(_I3(K,I,J))*U(_I3(K,I,J)) )
C       vAja: vAja - lambda(vAja-BxBy[V(n+0.5)] ( note rAja = (1-lambda) )
        vAja(_I3(K,I,J)) = rAja*vAja(_I3(K,I,J))
     &   +(1.-rAja)*( (1.5+abEps)*0.25D0*(vK(I,J)+vK(I-1,J)+vK(I,J+1)+vK(I-1,J+1))
     &    -(0.5+abEps)*0.25D0*(vKNM1(I,J)+vKNM1(I-1,J)+vKNM1(I,J+1)+vKNM1(I-1,J+1)) )
       ENDDO
      ENDDO
#endif

C     Add coriolis term from Cd grid into Gu and Gv.
C     Note: AB2 does not apply to Cd variables.
      gU(_I3(K,:,:)) = gU(_I3(K,:,:))*uMask(_I3(K,:,:))
      tmp(1:nx,1:ny) = gUNM1(_I3(K,:,:))
      gUNM1(_I3(K,:,:))   = gU(_I3(K,:,:))
      gU(_I3(K,:,:)) = 
     & (1.5+abEps)*gU(_I3(K,:,:))-(0.5+abEps)*tmp(1:nx,1:ny)
     & +fCorU(_I3(K,:,:))*vAja(_I3(K,:,:))*uMask(_I3(K,:,:))
      IF ( BOTTOM_LAYER ) THEN
      _D(( ' S/R G_CALC: MAXVAL(GU)', MAXVAL(GU) ))
      _D(( '             MAXVAL(GU) @', MAXLOC(GU) ))
      ENDIF

      gV(_I3(K,:,:)) = gV(_I3(K,:,:))*vMask(_I3(K,:,:))
      tmp(1:nx,1:ny) = gVNM1(_I3(K,:,:))
      gVNM1(_I3(K,:,:))   = gV(_I3(K,:,:))
      gV(_I3(K,:,:)) = 
     & (1.5+abEps)*gV(_I3(K,:,:))-(0.5+abEps)*tmp(1:nx,1:ny)
     & -fCorV(_I3(K,:,:))*uAja(_I3(K,:,:))*vMask(_I3(K,:,:))
      _D(( ' S/R G_CALC: MAXVAL(GV)', MAXVAL(GV) ))
C *********************************************************************
C *** FILTER SECTION **************************************************
C *********************************************************************
C     Apply filter to control energy build up at high-latitudes.

C *********************************************************************
C *** RHS SECTION *****************************************************
C *********************************************************************
C     Form right-hand side of elliptic eqaution
C     RHS2d: dx[Ax*Gu]+dy[Ay*Gv] 
C           +1/delt*(dx[Ax*U]+dy[Ay*V])
      tmp(1:nx,1:ny) = 
     &                 gU(_I3(K,:,:))/RONIL/G
     &                +rDelt*Uk(1:nx,1:ny)/RONIL/G
      tmp(nx+1,1:ny) = tmp(1,1:ny)
      DO J=1,ny
       DO I=1,nx
        RHS2d(I,J)=RHS2d(I,J)+XAk(I+1,J)*tmp(I+1,J)-XAk(I,J)*tmp(I,J)
       ENDDO
      ENDDO
      tmp(1:nx,1:ny) = 
     &                 gV(_I3(K,:,:))/RONIL/G
     &                +rDelt*Vk(1:nx,1:ny)/RONIL/G
      tmp(1:nx,ny+1) = tmp(1:nx,1)
      DO J=1,ny
       DO I=1,nx
        RHS2d(I,J)=RHS2d(I,J)+YAk(I,J+1)*tmp(I,J+1)-YAk(I,J)*tmp(I,J)
       ENDDO
      ENDDO
CcnhDebugStarts
      IF ( BOTTOM_LAYER ) THEN
C      DO I=1,Nx
C       WRITE(0,*) ' CG2D RHS2d(I,30) ', rhs2d(I,30), I
C      ENDDO
C      DO J=1,Ny
C       WRITE(0,*) ' CG2D MAXVAL(RHS2d) ', 
C    &  MAXVAL(RHS2d(1:Nx,J)), J
C       WRITE(0,*) ' CG2D MINVAL(RHS2d) ', 
C    &  MINVAL(RHS2d(1:Nx,J)), J
C    &  MAXVAL(ABS(fCorV(_I3(:,I,:))*uAja(_I3(:,I,:))*vMask(_I3(:,I,:))))
C      ENDDO
C      CALL PLOT_FIELD( RHS2d(1:nx,1:ny), nX, nY )
C      STOP
      ENDIF
CcnhDebugEnds
      DO J=1,ny
       DO I=1,nx
        divH(I,J)= divH(I,J)+ 
     &   XAk(I+1,J)*uK(I+1,J)-XAk(I,J)*uK(I,J)
     &  +YAk(I,J+1)*vK(I,J+1)-YAk(I,J)*vK(I,J)
       ENDDO
      ENDDO

      RETURN
      END
