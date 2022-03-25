#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_STREAMICE
# include "STREAMICE_OPTIONS.h"
#endif

C--   File shelfice_step_icemass.F:
C--    Contents:
C--    o SHELFICE_STEP_ICEMASS
C--    o SHELFICE_NETMASSFLUX_SURF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
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

           IF ( .NOT.SHELFICEDynMassOnly ) THEN
            DO j=1-OLy,sNy+OLy
             DO i=1-OLx,sNx+OLx
              shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &              + shelfIceFreshWaterFlux(i,j,bi,bj)*deltaT
             ENDDO
            ENDDO
           ENDIF

           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              shelficeMass(i,j,bi,bj) = shelficeMass(i,j,bi,bj)
     &              + shelfIceMassDynTendency(i,j,bi,bj)*deltaT
            ENDDO
           ENDDO

C--    quick fix to prevent ice-shelf mass to become negative
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              shelficeMass(i,j,bi,bj) =
     &              MAX( shelficeMass(i,j,bi,bj), zeroRL )
            ENDDO
           ENDDO

         ENDDO
        ENDDO
       ENDIF

       _EXCH_XY_RL( shelficeMass, myThid )

#ifndef ALLOW_OPENAD
C--  Calculate new loading anomaly (in case the ice-shelf mass was updated)
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx, sNx+OLx
           shelficeLoadAnomaly(i,j,bi,bj) = gravity
     &      *( shelficeMass(i,j,bi,bj) + rhoConst*Ro_surf(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#endif

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: SHELFICE_NETMASSFLUX_SURF

C !INTERFACE: ==========================================================
      SUBROUTINE SHELFICE_NETMASSFLUX_SURF(
     O                        shelfIceNetMassFlux,
     I                        myTime, myIter, myThid )

C !DESCRIPTION:
C compute the net mass flux implied by S/R SHELFICE_STEP_MASS

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
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myTime      :: current time in simulation
C     myIter      :: current iteration number insimulation
C     myThid      :: my thread Id number
      _RL  myTime
      INTEGER myIter
      INTEGER myThid
C     net mass flux in m^3/s
      _RL shelfIceNetMassFlux
CEOP

#ifdef ALLOW_SHELFICE
C !LOCAL VARIABLES : ====================================================
C     i,j, bi,bj  :: loop indices
      INTEGER bi,bj,i,j
      _RL shelfIceNetMassFluxTile(nSx,nSy)

      shelfIceNetMassFlux = 0. _d 0

      IF ( SHELFICEMassStepping ) THEN

       IF (useStreamIce) THEN

#ifdef ALLOW_STREAMICE
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shelfIceNetMassFluxTile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
            IF ( streamice_hmask(i,j,bi,bj).EQ.1 .OR.
     &           streamice_hmask(i,j,bi,bj).EQ.2 ) THEN
C--   this assumes that the change in H_streamice include the melting flux
CML             shelfIceNetMassFluxTile(bi,bj) =
CML     &              shelfIceNetMassFluxTile(bi,bj)
CML     &            +(H_streamice(i,j,bi,bj)-H_streamice_prev(i,j,bi,bj))
CML     &            /deltaT * streamice_density * mass2rUnit
CML     &            * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
#endif /* ALLOW_STREAMICE */

       ELSE

        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          shelfIceNetMassFluxTile(bi,bj) = 0. _d 0
          DO j=1,sNy
           DO i=1,sNx
C--   Flux of new ice will raise the sea level
            shelfIceNetMassFluxTile(bi,bj) =
     &             shelfIceNetMassFluxTile(bi,bj)
     &           + shelfIceMassDynTendency(i,j,bi,bj)
     &           * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
            IF ( .NOT.SHELFICEDynMassOnly ) THEN
C--   Freshwater flux by thermodynamic processes. In the case of
C     melting this will be negative (positive upward) so it will reduce
C     the amount of added ice mass. The same freshwater flux is added
C     to the ocean in EmPmR and will raise the sea level, so that this
C     contribution is canceled.
             shelfIceNetMassFluxTile(bi,bj) =
     &              shelfIceNetMassFluxTile(bi,bj)
     &            + shelfIceFreshWaterFlux(i,j,bi,bj)
     &            * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO

       ENDIF

       CALL GLOBAL_SUM_TILE_RL( shelficeNetMassFluxTile,
     &      shelficeNetMassFlux, myThid )

      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END
