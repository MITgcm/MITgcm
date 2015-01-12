C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_step_icemass.F,v 1.4 2015/01/12 17:35:30 jmc Exp $
C $Name:  $

#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

CBOP
C !ROUTINE: SHELFICE_U_DRAG

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_STEP_ICEMASS(
     I                        myTime, myIter, myThid )

!        myTime, myIter, myThid

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
C     === Routine arguments ===
C     myIter :: iteration counter for this thread
C     myTime :: time counter for this thread
C     myThid :: thread number for this instance of the routine.
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C  i,j                  :: loop indices
      INTEGER bi,bj,i,j

      IF ( SHELFICEMassStepping ) THEN

#ifdef ALLOW_STREAMICE
       IF (useStreamIce) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy-1
           DO i=1-OLx+1,sNx+OLx-1
            if (streamice_hmask(i,j,bi,bj).eq.1 .or.
     &          streamice_hmask(i,j,bi,bj).eq.2) then

             shelficeMass(i,j,bi,bj) =
     &        H_streamice(I,J,bi,bj) * streamice_density

            endif
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
#endif /* ALLOW_STREAMICE */

       IF (.NOT.useStreamIce) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx

            shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &      + shelfIceFreshWaterFlux(I,J,bi,bj) * deltaT

            shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &      + shelfIceMassDynTendency(I,J,bi,bj)*deltaT

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
