C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth3d.F,v 1.1 2011/05/20 00:52:47 gforget Exp $
C $Name:  $

#include "SMOOTH_OPTIONS.h"

      subroutine smooth3D (
     U     fld_in,smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth3D
C     | o Routine that applies 3D spatial smoothing
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
c#include "tamc.h"
#include "SMOOTH.h"


      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nR,nSx,nSy)
      integer smoothOpNb
      integer nbt_in
      character*( 80) fnamegeneric
      integer i,j,k,bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

      
c read smoothing [i.e diffusion] operator:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth3Doperator',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kwx,1, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kwy,2, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kwz,3, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kux,4, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kvy,5, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kuz,6, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kvz,7, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kuy,8, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_Kvx,9, 1, myThid )
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec, 
     &           Nr,smooth3D_kappaR,10, 1, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kwx, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kwy, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kwz, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kux, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kvy, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kuz, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kvz, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kuy, myThid )
      _EXCH_XYZ_RL ( smooth3D_Kvx, myThid )
      _EXCH_XYZ_RL ( smooth3D_kappaR, myThid )

c do the smoothing:
      nbt_in=smooth3Dnbt(smoothOpNb)/2
      call smooth_diff3D(fld_in,nbt_in,mythid)

      end
