C $Id: cg2d.F,v 1.1 1998/05/25 20:21:04 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
      SUBROUTINE CG2D (
     I                 b, maxIters, freqCheckToler, tolerance,
     U                 x,
     O                 nIters, initialResidual, finalResidual)
C
C     Preconditioned conjugate gradient solver for Ax = b. x and b
C     represent values of a field in a two dimensional domain of extents
C     Nx by Ny.
      IMPLICIT NONE
C     ====== Global variables =========
#include "SIZE.h"
#include "CG2DZ.h"
#include "CG2DA.h"
#include "GRID.h"
#include "PARAMS.h"
C     ====== Routine arguments ========
C     x:              To be found.
C     b:              Right hand side.
C     maxIters:       Solver will give up after maxIters iterations.
C     freqCheckToler: Solver tests for convergence every freqCheckToler
C                     iterations.
C     tolerance:      Solver converges when residue is LT tolerance
C     nIters:         Number of iterations solver performed before exiting.
C     initialResidue: Residual at start of first iteration.
C     finalResidue:   Residual on exit from solver.
      REAL    x(Nx,Ny)
      REAL    b(Nx,Ny)    
      INTEGER maxIters
      INTEGER freqCheckToler
      REAL    tolerance
      INTEGER nIters
      REAL    initialResidual
      REAL    finalResidual
C     ===== Local variables ========
C     Residual vector
      REAL    r(0:Nx+1,0:Ny+1)
C     Conjugate direction vector
      REAL    s(0:Nx+1,0:Ny+1)
C     Workspace for Zr and As
      REAL    q(0:Nx+1,0:Ny+1)
C     Scalars
      REAL    err
      REAL    etaN
      REAL    etaNM1
      REAL    alpha
      REAL    alphaFac
      REAL    beta
      REAL    rr
      REAL    alpha1, alpha2, alpha3, alpha4, alpha5
      REAL    err1, err2, err3, err4, err5
      REAL    eta1, eta2, eta3, eta4, eta5
      INTEGER nIts
C
      INTEGER JMN1, JMX1, JMN2, JMX2, JMN3, JMX3, JMN4, JMX4, JMN5, JMX5
CcnhDebugStarts
C      PARAMETER ( JMN1 = 1        )
C      PARAMETER ( JMX1 = Ny/5     )
C      PARAMETER ( JMN2 = JMX1 + 1 )
C      PARAMETER ( JMX2 = 2*Ny/5   )
C      PARAMETER ( JMN3 = JMX2 + 1 )
C      PARAMETER ( JMX3 = 3*Ny/5   )
C      PARAMETER ( JMN4 = JMX3 + 1 )
C      PARAMETER ( JMX4 = 4*Ny/5   )
C      PARAMETER ( JMN5 = JMX4 + 1 )
C      PARAMETER ( JMX5 = Ny       )
      PARAMETER ( JMN1 = 1        )
      PARAMETER ( JMX1 = Ny       )
      PARAMETER ( JMN2 = 0        )
      PARAMETER ( JMX2 = 0        )
      PARAMETER ( JMN3 = 0        )
      PARAMETER ( JMX3 = 0        )
      PARAMETER ( JMN4 = 0        )
      PARAMETER ( JMX4 = 0        )
      PARAMETER ( JMN5 = 0        )
      PARAMETER ( JMX5 = 0        )
CcnhDebugEnds
      INTEGER TCOUNT, MYNUM, I, J
C
      DO J = 1, Ny
       DO I = 1, Nx
         s(I,J) = x(I,J)
       ENDDO
      ENDDO
      s(   0,1:Ny) = s(Nx,1:Ny)
      s(Nx+1,1:Ny) = s( 1,1:Ny)
      s(:   ,   0) = s( :,  Ny)
      s(:   ,Ny+1) = s( :,   1)
      etaNM1 = 1.
      etaN   = 0.
      initialResidual = 0.
      DO J = 1, Ny
       DO I = 1, Nx
         r(I,J) = b(I,J)
     &   -(aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
     &   )
         initialResidual = initialResidual+r(I,J)*r(I,J)
       ENDDO
      ENDDO
      DO J = 0, Ny+1
       DO I = 0, Nx+1
         s(I,J) = 0.
       ENDDO
      ENDDO
#ifdef periodic
      DO J =1, Ny
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
      ENDDO
      DO I =1, Nx
        r(   I,0   ) = r(I ,Ny)
        r(   I,Ny+1) = r(I ,1 )
      ENDDO
#endif
      DO J = 1, Ny
       DO I = 1, Nx
         q(I,J) = 
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         etaN = etaN + r(I,J)*q(I,J)
       ENDDO
      ENDDO
#ifdef periodic
      DO J =1, Ny
        q(   0,J   ) = q(Nx,J)
        q(Nx+1,J   ) = q(1 ,J)
      ENDDO
      DO I =1, Nx
        q(   I,0   ) = q(I ,Ny)
        q(   I,Ny+1) = q(I ,1 )
      ENDDO
#endif
      TCOUNT = 0
C
C     ========= Start timing here
CMIC$  PARALLEL
CMIC$& SHARED(beta,etaN,etaNM1,alpha,alphaFac,err),
CMIC$& SHARED(freeSurfFAC,ZA,G,DELT,maxIters,tolerance,aNorm2d)
CMIC$& SHARED(alpha1,alpha2,alpha3,alpha4,alpha5),
CMIC$& SHARED(eta1,eta2,eta3,eta4,eta5),
CMIC$& SHARED(err1,err2,err3,err4,err5),
CMIC$& SHARED(S,Q,aX,aY,x,r,pX,pY,pC),
CMIC$& SHARED(TCOUNT),
CMIC$& PRIVATE(nIts,I,J),
CMIC$& PRIVATE(MYNUM)
C     Give each thread a number
CMIC$ GUARD
      TCOUNT = TCOUNT + 1
      MYNUM  = TCOUNT
      nIts   = 0
CMIC$ END GUARD
C
   10 CONTINUE
       beta = etaN/etaNM1
CMIC$  CASE
C      PRINT *, 'LOOP 1, BLOCK 1, THREAD', MYNUM
       DO J = JMN1, JMX1
        DO I = 1, Nx
         s(I,J) = q(I,J)+s(I,J)*beta
        ENDDO
       ENDDO
       DO J = JMN1, JMX1
         s(   0,J   ) = s(Nx,J)
         s(Nx+1,J   ) = s(1 ,J)
       ENDDO
CMIC$  CASE
C      PRINT *, 'LOOP 1, BLOCK 2, THREAD', MYNUM
       DO J =  JMN2, JMX2
        DO I = 1, Nx
         s(I,J) = q(I,J)+s(I,J)*beta
        ENDDO
       ENDDO
       DO J = JMN2, JMX2
         s(   0,J   ) = s(Nx,J)
         s(Nx+1,J   ) = s(1 ,J)
       ENDDO
CMIC$  CASE
C      PRINT *, 'LOOP 1, BLOCK 3, THREAD', MYNUM
       DO J =  JMN3, JMX3
        DO I = 1, Nx
         s(I,J) = q(I,J)+s(I,J)*beta
        ENDDO
       ENDDO
       DO J = JMN3, JMX3
         s(   0,J   ) = s(Nx,J)
         s(Nx+1,J   ) = s(1 ,J)
       ENDDO
CMIC$  CASE
C      PRINT *, 'LOOP 1, BLOCK 4, THREAD', MYNUM
       DO J =  JMN4, JMX4
        DO I = 1, Nx
         s(I,J) = q(I,J)+s(I,J)*beta
        ENDDO
       ENDDO
       DO J = JMN4, JMX4
         s(   0,J   ) = s(Nx,J)
         s(Nx+1,J   ) = s(1 ,J)
       ENDDO
CMIC$  CASE
C      PRINT *, 'LOOP 1, BLOCK 5, THREAD', MYNUM
       DO J =  JMN5, JMX5
        DO I = 1, Nx
         s(I,J) = q(I,J)+s(I,J)*beta
        ENDDO
       ENDDO
       DO J = JMN5, JMX5
         s(   0,J   ) = s(Nx,J)
         s(Nx+1,J   ) = s(1 ,J)
       ENDDO
CMIC$  END CASE
CMIC$  CASE
       DO I =1, Nx
        s(   I,0   ) = s(I ,Ny)
        s(   I,Ny+1) = s(I ,1 )
       ENDDO
CMIC$  END CASE
CMIC$  CASE
       alpha1 = 0.
       DO J = JMN1, JMX1
        DO I = 1, Nx
         q(I,J) = 
     &     aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
         alpha1 = alpha1+s(I,J)*q(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       alpha2 = 0.
       DO J = JMN2, JMX2
        DO I = 1, Nx
         q(I,J) = 
     &     aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
         alpha2 = alpha2+s(I,J)*q(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       alpha3 = 0.
       DO J = JMN3, JMX3
        DO I = 1, Nx
         q(I,J) = 
     &     aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
         alpha3 = alpha3+s(I,J)*q(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       alpha4 = 0.
       DO J = JMN4, JMX4
        DO I = 1, Nx
         q(I,J) = 
     &     aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
         alpha4 = alpha4+s(I,J)*q(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       alpha5 = 0.
       DO J = JMN5, JMX5
        DO I = 1, Nx
         q(I,J) = 
     &     aX(I,J)*s(I-1,J)+aX(I+1,J)*s(I+1,J)+aY(I,J)*s(I,J-1)
     &    +aY(I,J+1)*s(I,J+1)-aX(I,J)*s(I,J)-aX(I+1,J)*s(I,J)
     &    -aY(I,J)*s(I,J)-aY(I,J+1)*s(I,J)
     &    -freeSurfFac*ZA(_I3(1,I,J))*s(I,J)/DELT/DELT/aNorm2d
         alpha5 = alpha5+s(I,J)*q(I,J)
        ENDDO
       ENDDO
CMIC$  END CASE
CMIC$  CASE
       alpha = alpha1+alpha2+alpha3+alpha4+alpha5
       etaNM1   = etaN
       alphaFac = etaN/alpha
       etaN     = 0.
       err = 0.
CMIC$  END CASE
CMIC$  CASE
       DO J = JMN1, JMX1
        DO I = 1, Nx
         x(I,J) = x(I,J) + alphaFac*s(I,J)
         r(I,J) = r(I,J) - alphaFac*q(I,J)
        ENDDO
       ENDDO
       DO J =JMN1, JMX1
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
       ENDDO
CMIC$  CASE
       DO J = JMN2, JMX2
        DO I = 1, Nx
         x(I,J) = x(I,J) + alphaFac*s(I,J)
         r(I,J) = r(I,J) - alphaFac*q(I,J)
        ENDDO
       ENDDO
       DO J =JMN2, JMX2
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
       ENDDO
CMIC$  CASE
       DO J = JMN3, JMX3
        DO I = 1, Nx
         x(I,J) = x(I,J) + alphaFac*s(I,J)
         r(I,J) = r(I,J) - alphaFac*q(I,J)
        ENDDO
       ENDDO
       DO J =JMN3, JMX3
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
       ENDDO
CMIC$  CASE
       DO J = JMN4, JMX4
        DO I = 1, Nx
         x(I,J) = x(I,J) + alphaFac*s(I,J)
         r(I,J) = r(I,J) - alphaFac*q(I,J)
        ENDDO
       ENDDO
       DO J =JMN4, JMX4
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
       ENDDO
CMIC$  CASE
       DO J = JMN5, JMX5
        DO I = 1, Nx
         x(I,J) = x(I,J) + alphaFac*s(I,J)
         r(I,J) = r(I,J) - alphaFac*q(I,J)
        ENDDO
       ENDDO
       DO J =JMN5, JMX5
        r(   0,J   ) = r(Nx,J)
        r(Nx+1,J   ) = r(1 ,J)
       ENDDO
CMIC$  END CASE
CMIC$  CASE
       DO I =1, Nx
        r(   I,0   ) = r(I ,Ny)
        r(   I,Ny+1) = r(I ,1 )
       ENDDO
CMIC$  END CASE
CMIC$  CASE
       eta1 = 0.
       err1 = 0.
       DO J = JMN1, JMX1
        DO I = 1, Nx
         q(I,J) =
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         eta1   = eta1 + r(I,J)*q(I,J)
         err1 = err1+r(I,J)*r(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       eta2 = 0.
       err2 = 0.
       DO J = JMN2, JMX2
        DO I = 1, Nx
         q(I,J) =
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         eta2   = eta2 + r(I,J)*q(I,J)
         err2 = err2+r(I,J)*r(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       eta3 = 0.
       err3 = 0.
       DO J = JMN3, JMX3
        DO I = 1, Nx
         q(I,J) =
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         eta3   = eta3 + r(I,J)*q(I,J)
         err3 = err3+r(I,J)*r(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       eta4 = 0.
       err4 = 0.
       DO J = JMN4, JMX4
        DO I = 1, Nx
         q(I,J) =
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         eta4   = eta4 + r(I,J)*q(I,J)
         err4 = err4+r(I,J)*r(I,J)
        ENDDO
       ENDDO
CMIC$  CASE
       eta5 = 0.
       err5 = 0.
       DO J = JMN5, JMX5
        DO I = 1, Nx
         q(I,J) =
     &     pX(I,J)*r(I-1,J)+pX(I+1,J)*r(I+1,J)+pY(I,J)*r(I,J-1)
     &    +pY(I,J+1)*r(I,J+1)+pC(I,J)*r(I,J)
         eta5   = eta5 + r(I,J)*q(I,J)
         err5 = err5+r(I,J)*r(I,J)
        ENDDO
       ENDDO
CMIC$  END CASE
CMIC$  CASE 
        etaN = eta1+eta2+eta3+eta4+eta5
        err  = err1+err2+err3+err4+err5
CMIC$  END CASE 
       nIts = nIts + 1
CMIC$  CASE
C      READ *
CMIC$  END CASE
C      _D((' Thread ',MYNUM,' error = ',SQRT(err),' nit = ',nIts))
      IF ( nIts .LT. maxIters ) THEN
CcnhDebugStarts
C      WRITE(0,*) ' CG2D: Iteration: ',nIts, ' residual = ', sqrt(err)
CcnhDEbugEnds
       IF ( SQRT(err)  .GE. tolerance    ) GOTO 10
      ENDIF
CMIC$ END PARALLEL
C
      _D(( 'finalResidual = ',SQRT(err) ))
      _D(( 'Iterations    = ',nIts      ))
      finalResidual = err
      nIters        = nITs
C
C     ========= End timing here
C
      END
      SUBROUTINE CG2D_OLD ( 
     I                 b, maxIters, freqCheckToler, tolerance, 
     U                 x, 
     O                 nIters, initialResidue, finalResidue )
C
C     Preconditioned conjugate gradient solver for Ax = b. x and b
C     represent values of a field in a two dimensional domain of extents
C     Nx by Ny.
      IMPLICIT NONE
#include "SIZE.h"
C     ====== Routine arguments ========
C     x:              To be found.
C     b:              Right hand side.
C     maxIters:       Solver will give up after maxIters iterations.
C     freqCheckToler: Solver tests for convergence every freqCheckToler 
C                     iterations.
C     tolerance:      Solver converges when residue is LT tolerance
C     nIters:         Number of iterations solver performed before exiting.
C     initialResidue: Residual at start of first iteration.
C     finalResidue:   Residual on exit from solver.
      REAL x (Nx,Ny)
      REAL b (Nx,Ny)
      INTEGER maxIters
      INTEGER freqCheckToler
      REAL    tolerance
      INTEGER nIters
      REAL    initialResidue
      REAL    finalResidue
C     ===== Local variables ========
C     Residual vector
      REAL r (Nx,Ny)
C     Conjugate direction vector
      REAL s (Nx,Ny)
C     Workspace for Zr and As
      REAL q (Nx,Ny)
C     Scalars
      REAL    etaN 
      REAL    etaNM1
      REAL    alpha
      REAL    beta
      REAL    rr
      INTEGER N
C
C     ===== Set initial direction vectors and scalars. =================
      etaNM1 = 1.D0
      etaN   = 1.D0
      s      = 0.D0
C     ===== Calculate initial residual vector r = b - Ax ===============
      CALL CG2DMA ( 
     I             x,
     O             r )
      r = b - r
      rr = SUM(r*r)
      initialResidue = SQRT(rr)
      WRITE(0,*) ' CG2D: Iteration ', 1,' residual = ',SQRT(rr)
      finalResidue   = initialResidue
      nIters         = maxIters
C     ===== Begin iterating  ===========================================
      DO 10 N = 1, maxIters
C       ===== Test for convergence   ===================================
        IF ( MOD(N,freqCheckToler) .EQ. 0 ) THEN
          rr = SUM(r*r)
C         WRITE(0,*) ' CG2D: Iteration ', N-1,' residual = ',SQRT(rr)
          IF ( SQRT(rr) .LT. tolerance ) THEN
            nIters = N-1
            GOTO 100
          ENDIF
        ENDIF
C       ===== Solve preconditioning equation q = Zr   ==================
        CALL CG2DPC  ( 
     I                r,
     O                q )
C       q = r
C       ===== Update conjugate direction vector   ======================
        etaN = SUM(r*q)

CcnhDebugStarts
C       WRITE(0,*) ' CG2D: Iteration ', N-1,' etaN = ',etaN
CcnhDebugEnds

        beta = etaN/etaNM1

CcnhDebugStarts
C       WRITE(0,*) ' CG2D: Iteration ', N-1,' beta = ',beta
CcnhDebugEnds

        s = q + s*beta
        etaNM1 = etaN
C       ===== Compute  q = As   ========================================
        CALL CG2DMA ( 
     I               s,
     O               q )
C       ===== Update solution and residual vectors   ===================
        alpha = etaN/SUM(s*q)

CcnhDebugStarts
C       WRITE(0,*) ' CG2D: Iteration ', N-1,' SUM(s*q)= ',SUM(s*q)
C       WRITE(0,*) ' CG2D: Iteration ', N-1,' alpha= ',alpha
CcnhDebugEnds

        x = x+alpha*s
        r = r-alpha*q
   10 CONTINUE
  100 CONTINUE
C
      rr = SUM(r*r)
      finalResidue = SQRT(rr)
      WRITE(0,*) ' CG2D: Iteration ', nIters,' residual = ',SQRT(rr)
C     _D(( ' CG2D: Number of iterations = ', nIters ))
C     _D(( ' CG2D: residual = ', finalResidue  ))
C
      END
      SUBROUTINE CG2DPC (
     I                   r,
     O                   q )
      IMPLICIT NONE
C     Solve preconditioning equation A~q = r
#include "SIZE.h"
#include "CG2DZ.h"
#include "PARAMS.h"
      REAL r (Nx,Ny)
      REAL q (Nx,Ny)
C
      q = r * pC(1:Nx,1:Ny)
     &   +CSHIFT(r*pX(1:Nx,1:Ny),+1,1)
     &   +CSHIFT(r,-1,1)*pX(1:Nx,1:Ny)
     &   +CSHIFT(r*pY(1:Nx,1:Ny),+1,2)
     &   +CSHIFT(r,-1,2)*pY(1:Nx,1:Ny)
C     q = r 
      RETURN
      END
      SUBROUTINE CG2DMA (
     I                   x,
     O                   Atimex 
     &                  )
      IMPLICIT NONE
C     Evaluate matrix vector multiplication Ax.
#include "SIZE.h"
#include "CG2DA.h"
#include "GRID.h"
#include "PARAMS.h"
C
      REAL x (Nx,Ny)
      REAL Atimex (Nx,Ny)
C
      Atimex =
     &   -x*aX(1:Nx,1:Ny)
     &   -x*aY(1:Nx,1:Ny)
     &   -x*CSHIFT(aX(1:Nx,1:Ny),+1,1)
     &   -x*CSHIFT(aY(1:Nx,1:Ny),+1,2)
     &   -freeSurfFac*ZA(_I3(1,:,:))*x/DELT/DELT/aNorm2d
     &   + CSHIFT(x*aX(1:Nx,1:Ny),+1,1)
     &   + CSHIFT(x   ,-1,1)*aX(1:Nx,1:Ny)
     &   + CSHIFT(x*aY(1:Nx,1:Ny),+1,2)
     &   + CSHIFT(x   ,-1,2)*aY(1:Nx,1:Ny)
      RETURN
      END
