C $Id: CG2DZ.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C     Common block used to hold precondidioner for 2d Laplace operator inversion
C     code.
      COMMON /CG2DZ/ pC, pX, pY
      REAL pC(Nx,Ny)
      REAL pX(0:Nx+1,0:Ny+1)
      REAL pY(0:Nx+1,0:Ny+1)
