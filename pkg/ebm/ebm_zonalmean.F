#include "EBM_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP 0
C !ROUTINE: EBM_ZONALMEAN

C !INTERFACE:
      SUBROUTINE EBM_ZONALMEAN( myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R CALCULATE ZONAL MEAN TEMPERATURE
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_EBM
# include "EBM.h"
#endif
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT PARAMETERS:
C     === Routine arguments ===
C     myThid   :: my Thread Id number
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EBM
C     !LOCAL VARIABLES:
C     i, j, k :: Loop counters
      INTEGER i, j, k, bi, bj
      INTEGER jg
      _RL tileSumMask(nSx,nSy)
      _RL tileSumSST (nSx,nSy)
      _RL locSumMask(Ny)
      _RL locSumSST (Ny)

C--   Top layer only
      k = 1

C--   Calculate the zonal mean
      DO jg = 1,Ny
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          tileSumMask(bi,bj) = 0.
          tileSumSST (bi,bj) = 0.
          j = jg + 1 - myYGlobalLo - (bj-1)*sNy
          IF ( j.GE.1 .AND. j.LE.sNy ) THEN
           DO i = 1,sNx
            tileSumMask(bi,bj) = tileSumMask(bi,bj)
     &                         + maskC(i,j,k,bi,bj)
            tileSumSST (bi,bj) = tileSumSST (bi,bj)
     &                         + maskC(i,j,k,bi,bj)*theta(i,j,k,bi,bj)
           ENDDO
          ENDIF
         ENDDO
        ENDDO
        CALL GLOBAL_SUM_TILE_RL( tileSumMask, locSumMask(jg), myThid )
        CALL GLOBAL_SUM_TILE_RL( tileSumSST,  locSumSST(jg),  myThid )
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE CountX = comlev1, key = ikey_dynamics
#endif
      _BEGIN_MASTER(myThid)
      DO bj=1,nSy
       DO j=1-OLy,sNy+OLy
        jg = myYGlobalLo + j-1 + (bj-1)*sNy
C       cyclic domain in Y:
c       jg = 1 + MOD( jg-1+Ny, Ny)
C       closed domain in Y:
        jg = MAX(MIN(jg,Ny),1)
        CountX(j,bj) = locSumMask(jg)
        ZonalMeanSST(j,bj) = locSumSST(jg)
        IF ( CountX(j,bj).GT.0. _d 0 ) THEN
          ZonalMeanSST(j,bj) = ZonalMeanSST(j,bj)/CountX(j,bj)
        ENDIF
       ENDDO
      ENDDO
      _END_MASTER(myThid)
      _BARRIER

      IF ( tauThetaZonRelax .NE. 0. _d 0 ) THEN
C-    replace SST with ZonalMeanSST for relaxation towards Zonal-Mean value
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
            SST(i,j,bi,bj) = ZonalMeanSST(j,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
c      _EXCH_XY_RS( SST, myThid )
      ENDIF

#endif /* ALLOW_EBM */

      RETURN
      END
