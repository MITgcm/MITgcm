C $Id: OPERATORS.h,v 1.1 1998/05/25 20:21:07 cnh Exp $
C       File name: OPERATORS.h
C     Description: Global arrays holding linear differencing operators.
C         Written: January 13th 1994, by Chris Hill M.I.T.
C        Comments:
C
C     P grid operators.
C     Spatial averaging operators.
C     pDdxOp - P volume centre separation in X.
C     pDdyOp - P volume centre separation in Y.
C     pDdzOp - P volume centre separation in Z.
      COMMON /P_Op/ 
     &   pDdxOp, pDdyOp, pDdzOp
      real pDdxOp (_I3(nz,nx,ny))
      real pDdyOp (_I3(nz,nx,ny))
      real pDdzOp (_I3(nz,nx,ny))
C
C     ========================== U grid operators. ======================
C     Spatial averaging operators.
C     uByOpN - U volume: Y face displacement North/ U volume displacement.
C     uByOpS - U volume: Y face displacement South/ U volume displacement.
C     uBzOpU - U volume: Z face displacement Up   / U volume displacement.
C     uBzOpD - U volume: Z face displacement Down / U volume displacement.
C     fCorU      - Horizontal coriolis term at a U point.
C     fCorVertU  - Vertical coriolis term at a U point.
C     rURadius   - Radial displacement of a U point.     Curvilinear coord.
C     uTPhiOp    - Tan(phi)/uRadius for a U point.       terms.
C     uDdxOp - Reciprocal of U point separation in X.
C     uDdyOp - Reciprocal of U point separation in Y.
C     uDdzOp - Reciprocal of U point separation in Z.
      COMMON /U_Op/ 
     &   fCorU, fCorVertU, uTPhiOp,
     &   uDdxOp, uDdyOp, uDdzOp
      real fCorU      (_I3(nz,nx,ny))
      real fCorVertU  (_I3(nz,nx,ny))
      real uTPhiOp    (_I3(nz,nx,ny))
      real uDdxOp     (_I3(nz,nx,ny))
      real uDdyOp     (_I3(nz,nx,ny))
      real uDdzOp     (_I3(nz,nx,ny))
C
C     ============ V grid operators. ==================================
C     Spatial averaging operators
C     vBxOpW - V volume X face displacement West / V volume displacement.
C     vBxOpE - V volume X face displacement East / V volume displacement.
C     vBzOpU - V volume Z face displacment Up    / V volume displacement.
C     vBzOpD - V volume Z face displacment Down  / V volume displacement.
C     fCorV      - Horizontal coriolis term at a V point.
C     rVRadius   - Reciprocal radial displacement of a V point. Curvilinear
C     vTPhiOp    - Tan(phi)/vRadius for a V point.              terms.
C     vDdxOp - Reciprocal of V point separation in X.
C     vDdyOp - Reciprocal of V point separation in Y.
C     vDdzOp - Reciprocal of V point separation in Z.
      COMMON /V_Op/ 
     &   fCorV,  vTPhiOp,
     &   vDdxOp, vDdyOp, vDdzOp
      real fCorV   (_I3(nz,nx,ny))
      real vTPhiOp (_I3(nz,nx,ny))
      real vDdxOp  (_I3(nz,nx,ny))
      real vDdyOp  (_I3(nz,nx,ny))
      real vDdzOp  (_I3(nz,nx,ny))
C
C     W grid operators.
C     Spatial averaging operators.
C     fCorVertW  - Vertical coriolis term at a W point.
C     rWRadius   - Reciprocal radial displacement of a W point. Curvilinear
C                                                               terms.
      COMMON /W_Op/ 
     &   fCorW
      real fCorW  (_I3(nz,nx,ny))

      COMMON /Redi_Op/
     &   rDXatU,rDYatV,rDZatW,rDZatP
      real rDXatU(Nx,Ny),rDYatV(Nx,Ny),rDZatW(1:Nz+1),rDZatP(Nz)
