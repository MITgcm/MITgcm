#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_CORREL2D(
     &           fld_in, mask_in, smoothOpNb, myThid )

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
#include "SMOOTH.h"

      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER smoothOpNb
      INTEGER myThid

      INTEGER nbt_in
      CHARACTER*( 80) fnamegeneric
      INTEGER i,j,bi,bj

c read smoothing [i.e diffusion] operator:
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                     1, smooth2D_Kux,1,1,myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                     1, smooth2D_Kvy,2,1,myThid )
      CALL EXCH_XY_RL( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL( smooth2D_Kvy, myThid )

c read normalization field [i.e. 1/sqrt(var(filter))]:
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &    'smooth2Dnorm',smoothOpNb
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &                     1, smooth2Dnorm, 1, 1, myThid )
      CALL EXCH_XY_RL( smooth2Dnorm, myThid )

c division by ~sqrt(area):
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
           fld_in(i,j,bi,bj)=fld_in(i,j,bi,bj)
     & *SQRT(recip_rA(i,j,bi,bj))
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XY_RL ( fld_in , myThid )

c do the smoothing:
      nbt_in = smooth2Dnbt(smoothOpNb)/2
      CALL SMOOTH_DIFF2D( fld_in, mask_in, nbt_in, myThid )

c division by ~sqrt(var(filter)):
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1,sNy
          DO i = 1,sNx
            fld_in(i,j,bi,bj) = fld_in(i,j,bi,bj)
     &                        *smooth2Dnorm(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      CALL EXCH_XY_RL( fld_in , myThid )

      RETURN
      END
