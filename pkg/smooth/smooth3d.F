#include "SMOOTH_OPTIONS.h"

      SUBROUTINE SMOOTH3D(
     &           fld_in, smoothOpNb, myThid )

C     *==========================================================*
C     | SUBROUTINE smooth3D
C     | o Routine that applies 3D spatial smoothing
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

c do the smoothing:
      nbt_in = smooth3Dnbt(smoothOpNb)
      CALL smooth_diff3D( fld_in, nbt_in, myThid )

      RETURN
      END
