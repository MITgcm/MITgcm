C $Header: /u/gcmpack/MITgcm/pkg/smooth/smooth_correl3d.F,v 1.2 2015/01/23 18:58:26 gforget Exp $
C $Name:  $

#include "SMOOTH_OPTIONS.h"

      subroutine smooth_correl3D (
     U     fld_in,smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_correl3D
C     | o Routine that applies spatial correlation 
C     |   operator to a 3D control field
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
      CALL EXCH_XYZ_RL ( smooth3D_Kwx, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kwy, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kwz, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kux, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kvy, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kuz, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kvz, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kuy, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_Kvx, myThid )
      CALL EXCH_XYZ_RL ( smooth3D_kappaR, myThid )

c read normalization field [i.e. 1/sqrt(var(filter))]:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth3Dnorm',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           Nr, smooth3Dnorm,1,1,mythid)
      CALL EXCH_XYZ_RL ( smooth3Dnorm, myThid )

c division by ~sqrt(volume):
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx 
           fld_in(i,j,k,bi,bj)=fld_in(i,j,k,bi,bj)
     & *sqrt(recip_rA(i,j,bi,bj)*recip_drF(k))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XYZ_RL ( fld_in , myThid )

c do the smoothing:
      nbt_in=smooth3Dnbt(smoothOpNb)/2
      call smooth_diff3D(fld_in,nbt_in,mythid)

c division by ~sqrt(var(filter)):
       do bj = jtlo,jthi
        do bi = itlo,ithi
         DO j = 1,sNy
          DO i = 1,sNx
           DO k = 1,nR
       fld_in(i,j,k,bi,bj)=fld_in(i,j,k,bi,bj)
     & *smooth3Dnorm(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      CALL EXCH_XYZ_RL ( fld_in , myThid )

      end
