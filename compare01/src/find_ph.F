C $Id: find_ph.F,v 1.2 1998/06/23 13:36:41 adcroft Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
# define _D(a) 
C=====================================================================
C|||    Procedure name: FIND_PH                                    |||
C|||          Function: Diagnose hydrostatic pressure field.       |||
C|||          Comments:                                            |||
C=====================================================================
CStartOfInterface
      SUBROUTINE FIND_PH( RHS2D, rPrmBz, divH,
     O            PH , PS )
      IMPLICIT NONE
C     /--------------------------------------------------------------\
C     |    ==== Global data ==========                               |
C     \--------------------------------------------------------------/
#include "SIZE.h"
#include "AJAINF.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "OLDG.h"
#include "MASKS.h"
#include "CG2DA.h"
C     /--------------------------------------------------------------\
C     |    ==== Routine arguments ====                               |
C     | RHS2D   - div(GuHat) - 1/dt*div(UHat)                        |
C     | rprmBz  - Density anomaly wrt inert reference (kg/m**3)      |
C     | divH    - Vertically integrated divergent flow (m**3/s)      |
C     | PH      - Hydrostatic "pressure" (m).                        |
C     | PS      - Surface "pressure" (m).                            |
C     \--------------------------------------------------------------/
      REAL rPrmBz(Nx,Ny,Nz)
      REAL RHS2D (Nx,Ny   )
      REAL divH  (Nx,Ny   )
      REAL PH    (Nx,Ny,Nz)
      REAL PS    (Nx,Ny   )
CEndOfInterface
C     /--------------------------------------------------------------\
C     |    ==== Local variables ======                               |
C     | PHxhat  - Vertically integrated d/dx PH.                     |
C     | PHyhat  - Vertically integrated d/dy PH.                     |
C     | divHMx  - Maximum vert. intergrated divergence (s**-1).      |
C     | MAXIT   - Limit on number of CG iterations.                  |
C     | rMax    - Max. absolute value of elliptic eqn. rhs.          |
C     | toler   - tolerance for CG convergence.                      |
C     | freqCheck - Interval at which CG convergence is tested.      |
C     | rStart2   - Starting CG residual.                            |
C     | rFinal2   - Converged CG residual.                           |
C     | numIt2    - No. of CG iterations to converge.                |
C     \--------------------------------------------------------------/
      REAL PHxhat (Nx+1,Ny+1)
      REAL PHyhat (Nx+1,Ny+1)
      REAL    divHMx
      INTEGER MAXIT
      REAL    rMax
      REAL    toler
      INTEGER freqCheck
      INTEGER numIt2
      REAL    rStart2
      REAL    rFinal2
C     /--------------------------------------------------------------\
C     | Loop counters                                                |
C     \--------------------------------------------------------------/
      INTEGER I, J, K

C     _D(( ' S/R PFIND: MAXVAL(RHS2d) = ',MAXVAL(RHS2d)))
c     WRITE(0,*) ' S/R PFIND: MAXVAL(RHS2d) = ',MAXVAL(RHS2d)

C     /--------------------------------------------------------------\
C     | Hydrostatic pressure                                         |
C     \--------------------------------------------------------------/
      pHNM1 = pH
      DO J = 1, Ny
       DO I = 1, Nx
C       /------------------------------------------------------------\
C       | pH(K=1) <- 0.                                              |
C       \------------------------------------------------------------/
        pH(I,J,1) = 0.5*DELPS(1  )*rprmBz(I,J,1)
       ENDDO
      ENDDO
      DO K = 2, Nz
       DO J = 1, Ny
        DO I = 1, Nx
C        /-----------------------------------------------------------\
C        | pH(K) <- Bz{DP}*rhoPrime(K)+pH(K-1),K=2,Nz                |
C        \-----------------------------------------------------------/
         pH(I,J,K) = 0.5*DELPS(K-1)*rprmBz(I,J,K)+
     &               0.5*DELPS(K  )*rprmBz(I,J,K)+
     &               pH(I,J,K-1)
         pH(I,J,K) = pH(I,J,K)*PMASK(I,J,K)
        ENDDO
       ENDDO
      ENDDO
CcnhDebugStarts
c      WRITE(0,*) 'PH K=Nz'
c      CALL PLOT_FIELD(PH(_I3(Nz,:,:))/RONIL,Nx,Ny)
c      WRITE(0,*) ' END '
CcnhDebugEnds
      DO K = 1, Nz
       DO J = 1, Ny
        DO I = 1, Nx
C        /-----------------------------------------------------------\
C        | pH <- pH/G/RONIL**2                                       |
C        \-----------------------------------------------------------/
         pH(I,J,K) = pH(I,J,K)/G/RONIL/RONIL
        ENDDO
       ENDDO
      ENDDO
C
      RETURN
      END
