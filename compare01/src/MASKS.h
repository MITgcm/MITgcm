C $Id: MASKS.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C       File name: MASKS.h
C     Description: Global arrays used to represent topography.
C         Written: January 13th 1994, by Chris Hill M.I.T.
C    Code changes:
C
C        Comments:
C
C     P_MASKS   - Masks relating to Pressure point grid.
C     PMASK     - 0. if P point is on land.
C                 1. if P point is in water.
      COMMON /P_MASKS/ PMASK
      real pMask (_I3(Nz,Nx,Ny))
CMF$LAYOUT pMask (:SERIAL,:NEWS,:NEWS) 
C
C     U_MASKS - Masks relating to U velocity grid.
C     UMASK   - 0. if U point is on land.
C               1. if U point is in water.
      COMMON /U_MASKS/ UMASK
      real uMask (_I3(Nz,Nx,Ny))
CMF$LAYOUT uMask (:SERIAL,:NEWS,:NEWS) 
C
C     V_MASKS - Masks relating to V velocity grid.
C     VMASK   - 0. if V point is on land.
C               1. if V point is in water.
      COMMON /V_MASKS/ VMASK
      real vMask (_I3(Nz,Nx,Ny))
CMF$LAYOUT vMask (:SERIAL,:NEWS,:NEWS) 
C
C     W_MASKS - Masks relating to W velocity grid.
C     WMASK   - 0. if W point is on land.
C               1. if W point is in water.
      COMMON /W_MASKS/ WMASK
      real wMask (_I3(Nz,Nx,Ny))
CMF$LAYOUT wMask (:SERIAL,:NEWS,:NEWS) 
