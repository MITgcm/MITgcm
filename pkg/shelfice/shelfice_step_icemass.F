#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SHELFICE_STEP_ICEMASS

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_STEP_ICEMASS(
     I                        myTime, myIter, myThid )

C !DESCRIPTION:
C Serves as a "stub" for ice dynamics
C will later be used to

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SHELFICE.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     myTime      :: current time in simulation
C     myIter      :: current iteration number insimulation
C     myThid      :: my thread Id number
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C     i,j, bi,bj  :: loop indices
      INTEGER bi,bj,i,j

      IF ( SHELFICEMassStepping ) THEN

       IF (useStreamIce) THEN

#ifdef ALLOW_STREAMICE
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
            IF ( streamice_hmask(i,j,bi,bj).EQ.1 .OR.
     &           streamice_hmask(i,j,bi,bj).EQ.2 ) THEN
             shelficeMass(i,j,bi,bj) =
     &        H_streamice(i,j,bi,bj) * streamice_density
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_STREAMICE */

       ELSE

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx

            IF ( .NOT.SHELFICEDynMassOnly ) THEN
             shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &        + shelfIceFreshWaterFlux(i,j,bi,bj)*deltaT
            ENDIF

            shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &        + shelfIceMassDynTendency(i,j,bi,bj)*deltaT

           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       _EXCH_XY_RL( shelficeMass, myThid )

      ENDIF

#ifdef ALLOW_DIAGNOSTICS
      IF (useDiagnostics) THEN
        CALL DIAGNOSTICS_FILL( shelficeMass, 'SHI_mass',
     I                         0, 1, 0, 1, 1, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */
#endif /* ALLOW_SHELFICE */

      RETURN
      END
