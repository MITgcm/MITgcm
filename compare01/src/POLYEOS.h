C $Id: POLYEOS.h,v 1.1 1998/05/25 20:21:07 cnh Exp $
C     Coefficients for polynomial approximation to full equation of state.
C     Currently these are loaded from an external file.
      COMMON /POLYEOS/ tRef, sRef, rprmInit, C, sig0
      REAL tRef(Nz+1)
      REAL sRef(Nz+1)
      REAL rprmInit(Nz+1)
      REAL sig0(Nz+1)
      REAL C(Nz+1,9)
