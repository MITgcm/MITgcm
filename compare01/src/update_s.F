C $Id: update_s.F,v 1.2 1998/06/15 05:13:55 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C===================================================================
C     Procedure name: UPDATE_S                                     |
C           Function: Step forward the salinity field.             |
C           Comments:                                              |
C===================================================================
CStartofinterface
      SUBROUTINE UPDATE_S(
     &                  U, V, W, K, T, GT )
      IMPLICIT NONE
C============== Global data ==========================================
#include "SIZE.h"
#include "AJAINF.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OPERATORS.h"
#include "OLDG.h"
#include "MASKS.h"
#include "FORCING.h"
C============= Routine arguments =====================================
C     /--------------------------------------------------------------\
C     | U, V, W - X,Y,Z Velocity ( m/s, m/s, Pa/s ).                 |
C     | K       - Working level.                                     |
C     | T       - Salinity (ppt).                                    |
C     \--------------------------------------------------------------/
      REAL    U  (Nx,Ny,Nz)
      REAL    V  (Nx,Ny,Nz)
      REAL    W  (Nx,Ny,Nz)
      INTEGER K
      REAL    T  (Nx,Ny,Nz)
      REAL    gt (Nx,Ny,Nz)
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
C     | o ?Grad?Op - Generic gradient operator A[XYZ][t].d/d[xyz]    |
C     |              tGradxOp: AXt/DXt, tGradyOp: AYt/DYt etc...     |
C     \--------------------------------------------------------------/
      REAL xaK        (Nx+1,0:Ny)
      REAL yaK        (0:Nx,Ny+1)
      REAL tGradxOp   (Nx  ,Ny  )
      REAL tGradyOp   (Nx  ,Ny  )
      REAL tGradzOpK  (Nx  ,Ny  )
      REAL tGradzOpKP1(Nx  ,Ny  )
      REAL zaK   (0:Nx,0:Ny)
      REAL zaKP1 (Nx,Ny)
      REAL rVk   (Nx  ,Ny  )
C     /--------------------------------------------------------------\
C     |State variables                                               |
C     | o u, v, w - X,Y,Z velocity slices.                           |
C     | o t       - Potential temperature.                           |
C     | o ph      - Hydrostatic pressure.                            |
C     \--------------------------------------------------------------/
      REAL uK    (Nx+1,0:Ny)
      REAL wK    (0:Nx,0:Ny)
      REAL wkP1  (0:Nx,0:Ny)
      REAL vK    (0:Nx,Ny+1)
      REAL tK    (0:Nx+1,0:Ny+1)
      REAL phK   (60)
C     /--------------------------------------------------------------\
C     |Work space arrays                                             |
C     \--------------------------------------------------------------/
      REAL tmp   (0:Nx+1,0:Ny+1)
      REAL tmp2  (0:Nx+1,0:Ny+1)
C     /--------------------------------------------------------------\
C     |Arrays for accumulating fluxes                                |
C     | o fX, fY, fZ - Flux in X, Y and Z.                           |
C     \--------------------------------------------------------------/
      REAL fX    (0:Nx+1,Ny)
      REAL fY    (Nx,0:Ny+1)
      REAL fZK   (Nx,Ny)
      REAL fZKP1 (Nx,Ny)
C     Loop counters:
      INTEGER I, J
C     Flags:
      LOGICAL TOP_LAYER
      LOGICAL BOTTOM_LAYER
C === Set up controlling flags ========================================
      TOP_LAYER    = K .EQ. 1
      BOTTOM_LAYER = K .EQ. Nz
C     _D(( ' K, Nz ', K, Nz ))
C     _D(( ' BOTTOM_LAYER ', BOTTOM_LAYER ))
C === Set up the slices ===============================================
C     /---------------------------------------------------------------\
C     | xaK       <- xa(K)                                            |
C     | yaK       <- ya(K)                                            |
C     | za[K,KP1] <- za[(K),(K+1)]                                    |
C     \---------------------------------------------------------------/
      DO J=1,Ny
       DO I=1,Nx
        xaK (I,J)   = xa  (_I3(K,I,J))
        yaK (I,J)   = ya  (_I3(K,I,J))
        zaK (I,J)   = za  (_I3(K,I,J))
       ENDDO
      ENDDO
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J=1,Ny
        DO I=1,Nx
         zaKP1(I,J)   = za(_I3(K+1,I,J))
        ENDDO
       ENDDO
      ENDIF
      fZK   = 0.
      fZKP1 = 0.
C     /---------------------------------------------------------------\
C     | tGradXOp        <- 1/DX*xa                                    |
C     | tGradYOp        <- 1/DY*ya                                    |
C     | tGradZOp[K,KP1] <- 1/DZ*ZA[K,K+1]                             |
C     \---------------------------------------------------------------/
      DO J = 1, Ny
       DO I = 1, Nx
        tGradxOp (I,J) = xaK(I,J)*pDdxOp(_I3(K,I,J))
        tGradyOp (I,J) = yaK(I,J)*pDdyOp(_I3(K,I,J))
        tGradzOpK(I,J) = zaK(I,J)*pDdzOp(_I3(K,I,J))
       ENDDO
      ENDDO
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, Ny
        DO I = 1, Nx
         tGradzOpKP1(I,J) = zaKP1(I,J)*pDdzOp(I,J,K+1)
        ENDDO
       ENDDO
      ENDIF
C     /---------------------------------------------------------------\
C     | w[K,KP1] <- W[(K),(K+1)]                                      |
C     \---------------------------------------------------------------/
      wKP1 = 0.
      DO J=1,Ny                                                 
       DO I=1,Nx                                                
        wK(I,J)  = W(_I3(K,I,J))                                
       ENDDO                                                    
      ENDDO                                                     
      IF ( .NOT. BOTTOM_LAYER ) THEN                            
       DO J=1,Ny                                                
        DO I=1,Nx                                               
         wKP1(I,J) = W(_I3(K+1,I,J))                            
        ENDDO                                                   
       ENDDO                                                    
      ENDIF                                                     
C     /---------------------------------------------------------------\
C     | uK <- U(K)                                                    |
C     \---------------------------------------------------------------/
      DO J=1,Ny                                                 
       DO I=1,Nx                                               
        uK(I,J) = U(_I3(K,I,J))                               
       ENDDO                                                 
      ENDDO                                                 
C     /---------------------------------------------------------------\
C     | vK <- V(K)                                                    |
C     \---------------------------------------------------------------/
      DO J=1,Ny                                                 
       DO I=1,Nx                                               
        vK(I,J) = V(_I3(K,I,J))                               
       ENDDO                                                 
      ENDDO                                                 
C     /---------------------------------------------------------------\
C     | tK <- T(K)                                                    |
C     \---------------------------------------------------------------/
      DO J=1,Ny                                                 
       DO I=1,Nx                                                
        tK(I,J)   = T(_I3(K,I,J))                               
       ENDDO                                                    
      ENDDO                                                     
      DO J=1,Ny                                                 
       tK(0,J)      = tK(Nx,J)                                  
       tK(Nx+1,J)   = tK(1 ,J)                                  
      ENDDO                                                     
      DO I=0,Nx+1                                               
       tK(I,0)         = tK(I ,  Ny)                            
       tK(I,Ny+1)      = tK(I ,   1)                            
      ENDDO                                                     
C     /---------------------------------------------------------------\
C     | Gt Section                                                    |
C     |===============================================================|
C     | In this section  Gt =   GtDiffusion                           |
C     |                       + GtAdvection                           |
C     |                       + GtForcing                             |
C     \---------------------------------------------------------------/
C     /---------------------------------------------------------------\
C     | Initialise local terms                                        |
C     | rVk <- 1/V                                                    |
C     \---------------------------------------------------------------/
      DO J=1,Ny                                                 
       DO I=1,Nx                                               
        rVk(I,J)   = rPvol(_I3(K,I,J))                         
       ENDDO                                                   
      ENDDO                                                    
      fZK   = 0.
      fZKP1 = 0.
C     /---------------------------------------------------------------\
C     | XY GtDiffusion + XY GtAdvection                               |
C     | Note:  XY GtDiffusion = Laplacian + Biharmonic                |
C     |        o Laplacian  ( -a2TempXY.del^2(u) )                    |
C     |        o Biharmonic ( +a4TempXY.del^4(u) )                    |
C     \---------------------------------------------------------------/
      DO J = 1, Ny
       DO I = 1, Nx
C       /-------------------------------------------------------------\
C       | fX <- 1/DX*XA*Ddx{T}                                        |
C       | fY <- 1/DY*YA*Ddy{T}                                        |
C       \-------------------------------------------------------------/
        fX(I,J) = tGradxOp(I,J)*(tK(I,J)-tK(I-1,J))
        fY(I,J) = tGradyOp(I,J)*(tK(I,J)-tK(I,J-1))
       ENDDO
      ENDDO
      DO J=1,Ny
       fX(Nx+1,J) = fX(1 ,J)
      ENDDO
      DO I=1,Nx
       fY(I,Ny+1) = fY(I, 1)
      ENDDO
      DO J=1,Ny
       DO I=1,Nx
C       /-------------------------------------------------------------\
C       | tmp <- 1/V*(Dx{fX}+Dy{fY})                                  |
C       \-------------------------------------------------------------/
        tmp(I,J)= rVk(I,J)*fX(I+1,J)-rVk(I,J)*fX(I,J)
     &           +rVk(I,J)*fY(I,J+1)-rVk(I,J)*fY(I,J)
       ENDDO
      ENDDO
      tmp(0,1:Ny) = tmp(Nx,1:Ny)
      tmp(1:Nx,0) = tmp(1:Nx,Ny)
      DO J = 1,Ny
       DO I = 1,Nx
C       /-------------------------------------------------------------\
C       | fX  <- -k2*fX               +   (  Laplacian,<Line 1>   )   |
C       |         k4/DX*XA*Ddx{tmp}   +   (  Biharmonic,<Line 2>  )   |
C       |         XA*UBx{T}               (  Advection,<Line 3>   )   |
C       \-------------------------------------------------------------/
        fX(I,J) = 0.
     _LPT(&       -a2TempXY*fX(I,J)                                   )
     _BHT(&       +a4TempXY*tGradxOp(I,J)*(tmp(I,J)-tmp(I-1,J))       )
     _ADT(&       +0.5*uK(I,J)*XAK(I,J)*( tK(I-1,J)+tK(I,J)  )        )
C       /-------------------------------------------------------------\
C       | fY  <- -k2*fY               +   (  Laplacian,<Line 1>   )   |
C       |         k4/DY*YA*Ddy{tmp}   +   (  Biharmonic,<Line 2>  )   |
C       |         YA*V*By{T}              (  Advection,<Line 3>   )   |
C       \-------------------------------------------------------------/
        fY(I,J) = 0.
     _LPT(&            -a2TempXY*fY(I,J)                              )
     _BHT(&            +a4TempXY*tGradyOp(I,J)*(tmp(I,J)-tmp(I,J-1))  )
     _ADT(&            +0.5*vK(I,J)*yaK(I,J)*( tK(I,J-1)+tK(I,J)  )   )
       ENDDO
      ENDDO
C     /---------------------------------------------------------------\
C     | Z GtDiffusion + Z GtAdvection                                 |
C     \---------------------------------------------------------------/
      IF ( .NOT. TOP_LAYER ) THEN
       DO J = 1, Ny
        DO I = 1, Nx
C        /------------------------------------------------------------\
C        | fZK <- -k2Z/DZ*ZA*Ddz{T} + (  Laplacian,<Line 1>   )       |
C        |              ZA*-w*Bz{T}   (  Advection,<Line 2>   )       |
C        \------------------------------------------------------------/
         fZK(I,J) = 0.
     _LPT(& -a2TempZ*tGradzOpK(I,J)*(T(_I3(K-1,I,J))-tK(I,J)  )       )
     _ADT(& +0.5*(tK(I,J)+T(_I3(K-1,I,J)))*( -wK(I,J)*ZAK(I,J) )      )
        ENDDO
       ENDDO
      ELSE
       DO J = 1, Ny
        DO I = 1, Nx
C        /------------------------------------------------------------\
C        | fZK <-       ZA*-w*Bz{T}   (  Advection,<Line 1>   )       |
C        \------------------------------------------------------------/
         fZK(I,J) = 0.
     _ADT(& +0.5*freeSurfFac*(tK(I,J)+tK(I,J))*(-wK(I,J)*ZAK(I,J) )  )
        ENDDO
       ENDDO
      ENDIF
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, Ny
        DO I = 1, Nx
C        /------------------------------------------------------------\
C        | fZKP1 <- -k2Z/DZ*ZA*Ddz{T} +  (  Laplacian,<Line 1>   )    |
C        |                ZA*-w*Bz{T}    (  Advection,<Line 2>   )    |
C        \------------------------------------------------------------/
         fZKP1(I,J) = 0.
     _LPT(& -a2TempZ*tGradzOpKP1(I,J)*( tK(I,J)-T(_I3(K+1,I,J)) )     )
     _ADT(& +0.5*(tK(I,J)+T(_I3(K+1,I,J)))*(-wKP1(I,J))*zaKP1(I,J)    )
        ENDDO
       ENDDO
      ENDIF
C     /---------------------------------------------------------------\
C     | gT <- GtDiffusion + GtAdvection = -div(fX,fY,fZ)              |
C     \---------------------------------------------------------------/
      fX(Nx+1,1:Ny) = fX(1,1:Ny)
      fY(:,Ny+1) = fY(:,1)
      DO J = 1, Ny
       DO I = 1, Nx
C       /-------------------------------------------------------------\
C       | gT <- -1/V*(Ddx{fX}+Ddy{fY})                                |
C       \-------------------------------------------------------------/
        gt(_I3(K,I,J))=-fX(I+1,J)*rVk(I,J)+fX(I,J)*rVk(I,J)
     &                 -fY(I,J+1)*rVk(I,J)+fY(I,J)*rVk(I,J)
       ENDDO
      ENDDO
      DO J = 1, Ny
       DO I = 1, Nx
C       /------------------------------------------------------------\
C       | gT <- gT-1/Vu*fZK                                          |
C       \------------------------------------------------------------/
        gt(_I3(K,I,J))=gt(_I3(K,I,J))-fZK(I,J)*rVk(I,J)
       ENDDO
      ENDDO
      IF ( .NOT. BOTTOM_LAYER ) THEN
       DO J = 1, Ny
        DO I = 1, Nx
C        /------------------------------------------------------------\
C        | gT <- gT+1/Vu*fZP1                                         |
C        \------------------------------------------------------------/
         gt(_I3(K,I,J))=gt(_I3(K,I,J))+fZKP1(I,J)*rVk(I,J)
        ENDDO
       ENDDO
      ENDIF
C     _D(( ' S/R UPDATE_S (GtDiff+GtAdv): MAXVAL(Gt  )', MAXVAL(Gt)))
C     _D(( ' S/R UPDATE_S (GtDiff+GtAdv): MINVAL(Gt  )', MINVAL(Gt)))
C     /---------------------------------------------------------------\
C     | gT <- gT + GtForcing                                          |
C     \---------------------------------------------------------------/
C     IF ( K.EQ.1 ) THEN
C      DO J =1,Ny                                               
C       DO I=1, Nx                                             
C        gT(_I3(K,I,J)) = gT(_I3(K,I,J)) -
C    &                    1./(30.*oneDay)*( T(_I3(K,I,J)) - Ssurf(I,J,K) )
C       ENDDO
C      ENDDO
C     ENDIF

C     _D(( ' S/R UPDATE_S (Gt Section): MAXVAL(Gt  )', MAXVAL(Gt)  ))
C     _D(( ' S/R UPDATE_S (Gt Section): MINVAL(Gt  )', MINVAL(Gt)  ))


C     Final time derivative from AB2.
      gt(_I3(K,1:Nx,1:Ny)) = Gt(_I3(K,1:Nx,1:Ny))*pMask(_I3(K,:,:))
      tmp  = 0.
      tmp2 = 0.
      tmp (1:Nx,1:Ny) = gsNM1(_I3(K,:,:))
      tmp2(1:Nx,1:Ny) = gt   (_I3(K,:,:))

C     _D(( ' S/R UPDATE_S (Gt Section): MAXVAL(Gt  )', MAXVAL(Gt)  ))
C     _D(( ' S/R UPDATE_S (Gt Section): MINVAL(Gt  )', MINVAL(Gt)  ))
 

      gsNM1(_I3(K,:,:)) = gt(_I3(K,:,:))      
      tmp2=(1.5+abEps)*tmp2-(0.5+abEps)*tmp
      gt(_I3(K,1:Nx,1:Ny))=tmp2(1:Nx,1:Ny)

C 
      RETURN
      END
