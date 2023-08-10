#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_FORCING
C     !INTERFACE:
      SUBROUTINE INI_FORCING( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_FORCING
C     | o Set model initial forcing fields.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  :: Tile indices
C     i, j   :: Loop counters
      INTEGER bi, bj
      INTEGER  i, j
CEOP

C-    Initialise all arrays in common blocks
C     <-- moved to new S/R INI_FFIELDS

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          IF ( doThetaClimRelax .AND.
     &         ABS(yC(i,j,bi,bj)).LE.latBandClimRelax ) THEN
           lambdaThetaClimRelax(i,j,bi,bj) = 1. _d 0/tauThetaClimRelax
          ELSE
           lambdaThetaClimRelax(i,j,bi,bj) = 0. _d 0
          ENDIF
          IF ( doSaltClimRelax .AND.
     &         ABS(yC(i,j,bi,bj)).LE.latBandClimRelax ) THEN
           lambdaSaltClimRelax(i,j,bi,bj) = 1. _d 0/tauSaltClimRelax
          ELSE
           lambdaSaltClimRelax(i,j,bi,bj) = 0. _d 0
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-    every-one waits before master thread loads from file
C     this is done within IO routines => no longer needed
c     _BARRIER

      IF ( zonalWindFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( zonalWindFile, ' ', fu, 0, myThid )
      ENDIF
      IF ( meridWindFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( meridWindFile, ' ', fv, 0, myThid )
      ENDIF
      IF ( surfQFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( surfQFile, ' ', Qnet, 0, myThid )
      ELSEIF ( surfQnetFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( surfQnetFile, ' ', Qnet, 0, myThid )
      ENDIF
      IF ( EmPmRfile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( EmPmRfile, ' ', EmPmR, 0, myThid )
c      IF ( convertEmP2rUnit.EQ.mass2rUnit ) THEN
C-     EmPmR is now (after c59h) expressed in kg/m2/s (fresh water mass flux)
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            EmPmR(i,j,bi,bj) = EmPmR(i,j,bi,bj)*rhoConstFresh
           ENDDO
          ENDDO
         ENDDO
        ENDDO
c      ENDIF
      ENDIF
      IF ( saltFluxFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( saltFluxFile, ' ', saltFlux, 0, myThid )
      ENDIF
      IF ( thetaClimFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( thetaClimFile, ' ', SST, 0, myThid )
      ENDIF
      IF ( saltClimFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( saltClimFile, ' ', SSS, 0, myThid )
      ENDIF
      IF ( lambdaThetaFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( lambdaThetaFile, ' ',
     &  lambdaThetaClimRelax, 0, myThid )
      ENDIF
      IF ( lambdaSaltFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( lambdaSaltFile, ' ',
     &  lambdaSaltClimRelax, 0, myThid )
      ENDIF
#ifdef SHORTWAVE_HEATING
      IF ( surfQswFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( surfQswFile, ' ', Qsw, 0, myThid )
       IF ( surfQFile .NE. ' '  ) THEN
C-     Qnet is now (after c54) the net Heat Flux (including SW)
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            Qnet(i,j,bi,bj) = Qnet(i,j,bi,bj) + Qsw(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif
#ifdef ATMOSPHERIC_LOADING
      IF ( pLoadFile .NE. ' '  ) THEN
       CALL READ_FLD_XY_RS( pLoadFile, ' ', pLoad, 0, myThid )
      ENDIF
#endif
#ifdef ALLOW_ADDFLUID
      IF ( addMassFile .NE. ' ' ) THEN
       CALL READ_FLD_XYZ_RL( addMassFile, ' ', addMass, 0, myThid )
       CALL EXCH_XYZ_RL( addMass, myThid )
      ENDIF
#endif /* ALLOW_ADDFLUID */
#ifdef ALLOW_GEOTHERMAL_FLUX
      IF ( geothermalFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( geothermalFile, ' ',
     &  geothermalFlux, 0, myThid )
       CALL EXCH_XY_RS( geothermalFlux, myThid )
# ifdef ALLOW_MONITOR
       CALL MON_PRINTSTATS_RS(
     &  1,geothermalFlux,'geothermalFlux',myThid)
# endif
      ENDIF
#endif /* ALLOW_GEOTHERMAL_FLUX */
#ifdef ALLOW_BALANCE_FLUXES
      IF ( selectBalanceEmPmR.EQ.2 ) THEN
C-    set default weight to 1 (i.e., same correction as selectBalanceEmPmR=1 )
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            weight2BalanceFlx(i,j,bi,bj) = oneRS
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      IF ( wghtBalanceFile .NE. ' ' ) THEN
       CALL READ_FLD_XY_RS( wghtBalanceFile, ' ',
     &                      weight2BalanceFlx, 0, myThid )
       CALL EXCH_XY_RS( weight2BalanceFlx, myThid )
      ENDIF
#endif /* ALLOW_GEOTHERMAL_FLUX */

      CALL EXCH_UV_XY_RS( fu,fv, .TRUE., myThid )
      CALL EXCH_XY_RS( Qnet , myThid )
      CALL EXCH_XY_RS( EmPmR, myThid )
      CALL EXCH_XY_RS( saltFlux, myThid )
      CALL EXCH_XY_RS( SST  , myThid )
      CALL EXCH_XY_RS( SSS  , myThid )
      CALL EXCH_XY_RS( lambdaThetaClimRelax, myThid )
      CALL EXCH_XY_RS( lambdaSaltClimRelax , myThid )
#ifdef SHORTWAVE_HEATING
      CALL EXCH_XY_RS( Qsw  , myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      CALL EXCH_XY_RS( pLoad  , myThid )
C     CALL PLOT_FIELD_XYRS( pLoad, 'S/R INI_FORCING pLoad',1,myThid)
#endif
C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      RETURN
      END
