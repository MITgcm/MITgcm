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
      _RL etaTile(nSx,nSy)
      _RL areaTile(nSx,nSy)
      _RL etaSum, areaSum
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      shelfIceNetMassFlux = 0. _d 0

      IF ( SHELFICEMassStepping ) THEN

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         shelfIceNetMassFluxTile(bi,bj) = 0. _d 0
         etaTile(bi,bj) = 0. _d 0
         areaTile(bi,bj) = 0. _d 0
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx

C--   I do not think that this contributon should be counted here
CML           IF ( .NOT.SHELFICEDynMassOnly ) THEN
CML            shelfIceNetMassFluxTile(bi,bj) =
CML     &             shelfIceNetMassFluxTile(bi,bj)
CML     &           + shelfIceFreshWaterFlux(i,j,bi,bj)*mass2rUnit
CML     &           * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
CML           ENDIF
           shelfIceNetMassFluxTile(bi,bj) =
     &            shelfIceNetMassFluxTile(bi,bj)
     &          + shelfIceMassDynTendency(i,j,bi,bj)*mass2rUnit
     &           * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
           IF ( kTopC(I,J,bi,bj) .EQ. 0
     &          .AND. kSurfC(I,J,bi,bj) .EQ. 1 ) THEN
            etaTile(bi,bj) = etaTile(bi,bj)
     &           + EtaN(i,j,bi,bj) * _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
            areaTile(bi,bj) = areaTile(bi,bj)
     &           + _rA(i,j,bi,bj) * maskInC(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       CALL GLOBAL_SUM_TILE_RL( etaTile, etaSum, myThid )
       CALL GLOBAL_SUM_TILE_RL( areaTile, areaSum, myThid )
       IF ( debugLevel.GE.debLevC ) THEN
        WRITE(msgBuf,'(A,I9,A,1P1E16.8)') 'OBCS_balance (it=',
     &       myIter, ' ) etaMean (open Water):', etaSum/areaSum
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &       SQUEEZE_RIGHT, myThid )
       ENDIF

       CALL GLOBAL_SUM_TILE_RL( shelficeNetMassFluxTile,
     &      shelficeNetMassFlux, myThid )

      ENDIF

#endif /* ALLOW_SHELFICE */

      RETURN
      END
