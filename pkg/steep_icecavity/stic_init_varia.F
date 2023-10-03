#include "STIC_OPTIONS.h"

CBOP
      SUBROUTINE STIC_INIT_VARIA( myThid )
C     *============================================================*
C     | SUBROUTINE STIC_INIT_VARIA
C     | o Routine to initialize STIC variables.
C     *============================================================*
C     | Initialize STIC parameters and variables.
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "STIC.h"
#ifdef ALLOW_COST
# include "STIC_COST.h"
#endif /* ALLOW_COST */

C     === Routine arguments ===
C     myThid -  Number of this instance of STIC_INIT_VARIA
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_STEEP_ICECAVITY
C     === Local variables ===
C     i,j,bi,bj - Loop counters
      INTEGER i, j, k, bi, bj
CEOP

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
           shelficeForcingT (i,j,bi,bj)           = 0. _d 0
           shelficeForcingS (i,j,bi,bj)           = 0. _d 0
           shelficeHeatFlux      (i,j,bi,bj)      = 0. _d 0
           shelficeFreshWaterFlux(i,j,bi,bj)      = 0. _d 0
           SHIICFHeatFlux        (I,J,bi,bj)      = 0. _d 0
           SHIICFFreshWaterFlux  (I,J,bi,bj)      = 0. _d 0
#ifndef ALLOW_shiTransCoeff_3d
           shiTransCoeffT        (i,j,bi,bj)      = 0. _d 0
           shiTransCoeffS        (i,j,bi,bj)      = 0. _d 0
#endif
           sticMass              (i,j,bi,bj)      = 0. _d 0
           shiCDragFld           (i,j,bi,bj)      = 0. _d 0
           shiDragQuadFld        (i,j,bi,bj)      = 0. _d 0
          DO k = 1, NR
#ifdef ALLOW_shiTransCoeff_3d
             shiTransCoeffT3d      (i,j,k,bi,bj)  = 0. _d 0
             shiTransCoeffS3d      (i,j,k,bi,bj)  = 0. _d 0
#endif
            iceFrontHeatFlux(i,j,k,bi,bj)         = 0. _d 0
            iceFrontFreshWaterFlux(i,j,k,bi,bj)   = 0. _d 0
            iceFrontForcingT(i,j,k,bi,bj)         = 0. _d 0
            iceFrontForcingS(i,j,k,bi,bj)         = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#ifdef ALLOW_AUTODIFF
C- Note: To simplify dependency & avoid recomputations, when compiling
C        pkg/autodiff, we always call STIC_INIT_VARIA to initialise
C        control
C        variables (as done above) without condition on useSTIC.
C        Therefore, in this case, the "If useSTIC" is added back
C        here:
      IF ( useSTIC ) THEN
#endif

      IF ( .NOT. STICuseGammaFrict ) THEN
       IF ( STICTransCoeffTFile .NE. ' ' ) THEN
#ifndef ALLOW_shiTransCoeff_3d
        CALL READ_FLD_XY_RL( STICTransCoeffTFile, ' ',
     &                       shiTransCoeffT, 0, myThid )
        _EXCH_XY_RL( shiTransCoeffT, myThid )
#else
        CALL READ_FLD_XYZ_RL( STICTransCoeffTFile, ' ',
     &                       shiTransCoeffT3d, 0, myThid )
        _EXCH_XYZ_RL( shiTransCoeffT3d, myThid )
#endif
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
#ifndef ALLOW_shiTransCoeff_3d
            shiTransCoeffS(i,j,bi,bj) = STICsaltToHeatRatio *
     &                                  shiTransCoeffT(i,j,bi,bj)
#else
            DO k = 1, Nr
            shiTransCoeffS3d(i,j,k,bi,bj) = STICsaltToHeatRatio *
     &                                  shiTransCoeffT3d(i,j,k,bi,bj)
            ENDDO
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
C     set coefficients to constant values
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
#ifndef ALLOW_shiTransCoeff_3d
            shiTransCoeffT(i,j,bi,bj) = STICheatTransCoeff
            shiTransCoeffS(i,j,bi,bj) = STICsaltTransCoeff
#else
            DO k = 1, Nr
            shiTransCoeffT3d(i,j,k,bi,bj) = STICheatTransCoeff
            shiTransCoeffS3d(i,j,k,bi,bj) = STICsaltTransCoeff
            ENDDO
#endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF
C     Update the spatially varying version of the drag coefficient
C     shiCDragFld used in shelfice_thermodynamics
C     shiCDragQuadFld used in shelfice_u/v_drag_coeff
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
          shiCDragFld(i,j,bi,bj) = shiCDrag
          shiDragQuadFld(i,j,bi,bj) = STICDragQuadratic
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j = 1-OLy, sNy+OLy
         DO i = 1-OLx, sNx+OLx
           sticMass(i,j,bi,bj) = sticMassInit(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( STICMassStepping .AND.
     &     ( nIter0.NE.0 .OR. startTime.NE.baseTime
     &                   .OR. pickupSuff.NE.' ') ) THEN
        CALL STIC_READ_PICKUP( 1, nIter0, myThid )
c     ENDIF

c     IF ( STICMassStepping ) THEN
C     set/update "kTopC" to agree with changing ice-shelf mass
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j = 1-OLy, sNy+OLy
           DO i = 1-OLx, sNx+OLx
             IF ( kSurfC(i,j,bi,bj).LE.Nr .AND.
     &            sticMass(i,j,bi,bj).GT.zeroRL ) THEN
               kTopC(i,j,bi,bj) = kSurfC(i,j,bi,bj)
             ELSE
               kTopC(i,j,bi,bj) = 0
             ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

#ifdef ALLOW_COST
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           cMeanSHIforT (i,j,bi,bj) = 0. _d 0
           cMeanSHIforS (i,j,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
       ENDDO
      ENDDO
c--
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
         objf_stic(bi,bj) = 0. _d 0
       ENDDO
      ENDDO
#endif /* ALLOW_COST */

#ifdef ALLOW_AUTODIFF
C-    end if useSTIC block
      ENDIF
#endif

#endif /* ALLOW_STEEP_ICECAVITY */

      RETURN
      END
