#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH_CORREL3D(
     &           fld_in, smoothOpNb, myThid )

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
#include "SMOOTH.h"

      _RL fld_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      INTEGER smoothOpNb
      INTEGER myThid

      INTEGER nbt_in
      CHARACTER*(80) fnamegeneric
      INTEGER i,j,k,bi,bj

c read smoothing [i.e diffusion] operator:
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &      'smooth3Doperator', smoothOpNb
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kwx, 1, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kwy, 2, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kwz, 3, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kux, 4, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kvy, 5, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kuz, 6, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kvz, 7, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kuy, 8, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_Kvx, 9, 1, myThid )
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3D_kappaR, 10, 1, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwx, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kwz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kux, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvz, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kuy, myThid )
      CALL EXCH_XYZ_RL( smooth3D_Kvx, myThid )
      CALL EXCH_XYZ_RL( smooth3D_kappaR, myThid )

c read normalization field [i.e. 1/sqrt(var(filter))]:
      WRITE(fnamegeneric(1:80),'(1A,I3.3)')
     &      'smooth3Dnorm', smoothOpNb
      CALL READ_REC_3D_RL( fnamegeneric, smoothprec,
     &           Nr, smooth3Dnorm, 1, 1, myThid)
      CALL EXCH_XYZ_RL( smooth3Dnorm, myThid )

c division by ~sqrt(volume):
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           fld_in(i,j,k,bi,bj) = fld_in(i,j,k,bi,bj)
     &          *SQRT(recip_rA(i,j,bi,bj)*recip_drF(k))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XYZ_RL( fld_in, myThid )

c do the smoothing:
      nbt_in = smooth3Dnbt(smoothOpNb)/2
      CALL smooth_diff3D( fld_in, nbt_in, myThid )

c division by ~sqrt(var(filter)):
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx
          DO k = 1,Nr
           fld_in(i,j,k,bi,bj) = fld_in(i,j,k,bi,bj)
     &                          *smooth3Dnorm(i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_XYZ_RL( fld_in, myThid )

      RETURN
      END
