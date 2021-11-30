#include "SMOOTH_OPTIONS.h"

      subroutine smooth_correl2D (
     U     fld_in,mask_in,smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_correl2D
C     | o Routine that applies spatial correlation
C     |   operator to a 2D control field
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
c#include "tamc.h"
#include "SMOOTH.h"

      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      integer smoothOpNb
      integer nbt_in
      character*( 80) fnamegeneric
      integer i,j,bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      integer myThid


      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


c read smoothing [i.e diffusion] operator:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Kux,1,1,mythid)
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2D_Kvy,2,1,mythid)
      CALL EXCH_XY_RL ( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL ( smooth2D_Kvy, myThid )

c read normalization field [i.e. 1/sqrt(var(filter))]:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Dnorm',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1, smooth2Dnorm,1,1,mythid)
      CALL EXCH_XY_RL ( smooth2Dnorm, myThid )

c division by ~sqrt(area):
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO j = 1,sNy
         DO i = 1,sNx
      fld_in(i,j,bi,bj)=fld_in(i,j,bi,bj)
     & *sqrt(recip_rA(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XY_RL ( fld_in , myThid )

c do the smoothing:
      nbt_in=smooth2Dnbt(smoothOpNb)/2
      call smooth_diff2D(fld_in,mask_in,nbt_in,mythid)

c division by ~sqrt(var(filter)):
       do bj = jtlo,jthi
        do bi = itlo,ithi
         DO j = 1,sNy
          DO i = 1,sNx
       fld_in(i,j,bi,bj)=fld_in(i,j,bi,bj)
     & *smooth2Dnorm(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      CALL EXCH_XY_RL ( fld_in , myThid )

      end
