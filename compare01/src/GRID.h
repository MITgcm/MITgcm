C $Id: GRID.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C       File name: GRID.h
C     Description: Global arrays used to hold fields describing volumes into
C                  which the model domain has been subdivided.
C         Written: January 13th 1994, by Chris Hill M.I.T.
C    Code changes:
C
C        Comments: The grid represents the model domain divided into
C                  boxes centred on P points. The edge dimensions, face areas
C                  and volumes of these boxes are defined here. 
C
C     P grid configuration
C     XA     - Area of X face to the west of the centre.
C     YA     - Area of Y face to the south of the centre.
C     ZA     - Area of Z face to above the centre.
C     rpVol       - Reciprocal of volume of P grid box.
C     ruVol       - Reciprocal of volume of U grid box.
C     rvVol       - Reciprocal of volume of V grid box.
C     rpVolHat    - Reciprocal of depth integrated water column volume.
C     nDeep       - Two dimensional array of depths in number of grid boxes
C                   of each water column.
C
      COMMON /P_topog/ 
     &       ZA, XA,  YA,
     &       rpVol  , rVVol, rUVol,
     &       rpVolHat
      real rPVol     (_I3(Nz,Nx,Ny))
CMF$LAYOUT rPVol     (:SERIAL,:NEWS,:NEWS) 
      real rUVol     (_I3(Nz,Nx,Ny))
CMF$LAYOUT rUVol     (:SERIAL,:NEWS,:NEWS) 
      real rVVol     (_I3(Nz,Nx,Ny))
CMF$LAYOUT rVVol     (:SERIAL,:NEWS,:NEWS) 
      real rPVolHat (Nx,Ny)
CMF$LAYOUT rPVolHat (:NEWS,:NEWS) 
      real XA   (_I3(Nz,Nx,Ny))
CMF$LAYOUT XA   (:SERIAL,:NEWS,:NEWS) 
      real YA   (_I3(Nz,Nx,Ny))
CMF$LAYOUT YA   (:SERIAL,:NEWS,:NEWS) 
      real ZA   (_I3(Nz,Nx,Ny))
CMF$LAYOUT ZA   (:SERIAL,:NEWS,:NEWS) 
