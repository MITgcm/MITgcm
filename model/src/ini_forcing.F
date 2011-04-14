C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.51 2011/04/14 21:11:21 jmc Exp $
C $Name:  $

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
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fu              (i,j,bi,bj) = 0. _d 0
          fv              (i,j,bi,bj) = 0. _d 0
          Qnet            (i,j,bi,bj) = 0. _d 0
          EmPmR           (i,j,bi,bj) = 0. _d 0
          saltFlux        (i,j,bi,bj) = 0. _d 0
          SST             (i,j,bi,bj) = 0. _d 0
          SSS             (i,j,bi,bj) = 0. _d 0
          Qsw             (i,j,bi,bj) = 0. _d 0
          pLoad           (i,j,bi,bj) = 0. _d 0
          sIceLoad        (i,j,bi,bj) = 0. _d 0
          surfaceForcingU (i,j,bi,bj) = 0. _d 0
          surfaceForcingV (i,j,bi,bj) = 0. _d 0
          surfaceForcingT (i,j,bi,bj) = 0. _d 0
          surfaceForcingS (i,j,bi,bj) = 0. _d 0
          surfaceForcingTice(i,j,bi,bj) = 0. _d 0
#ifndef EXCLUDE_FFIELDS_LOAD
          taux0           (i,j,bi,bj) = 0. _d 0
          taux1           (i,j,bi,bj) = 0. _d 0
          tauy0           (i,j,bi,bj) = 0. _d 0
          tauy1           (i,j,bi,bj) = 0. _d 0
          Qnet0           (i,j,bi,bj) = 0. _d 0
          Qnet1           (i,j,bi,bj) = 0. _d 0
          EmPmR0          (i,j,bi,bj) = 0. _d 0
          EmPmR1          (i,j,bi,bj) = 0. _d 0
          saltFlux0       (i,j,bi,bj) = 0. _d 0
          saltFlux1       (i,j,bi,bj) = 0. _d 0
          SST0            (i,j,bi,bj) = 0. _d 0
          SST1            (i,j,bi,bj) = 0. _d 0
          SSS0            (i,j,bi,bj) = 0. _d 0
          SSS1            (i,j,bi,bj) = 0. _d 0
#ifdef SHORTWAVE_HEATING
          Qsw0            (i,j,bi,bj) = 0. _d 0
          Qsw1            (i,j,bi,bj) = 0. _d 0
#endif
#ifdef ATMOSPHERIC_LOADING
          pLoad0          (i,j,bi,bj) = 0. _d 0
          pLoad1          (i,j,bi,bj) = 0. _d 0
#endif
#endif /* EXCLUDE_FFIELDS_LOAD */
         ENDDO
        ENDDO
#ifndef EXCLUDE_FFIELDS_LOAD
        loadedRec(bi,bj) = 0
#endif /* EXCLUDE_FFIELDS_LOAD */
       ENDDO
      ENDDO

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
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
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
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

      CALL EXCH_UV_XY_RS( fu,fv, .TRUE., myThid )
      CALL EXCH_XY_RS( Qnet , myThid )
      CALL EXCH_XY_RS( EmPmR, myThid )
      CALL EXCH_XY_RS( saltFlux, myThid )
      CALL EXCH_XY_RS( SST  , myThid )
      CALL EXCH_XY_RS( SSS  , myThid )
      CALL EXCH_XY_RS( lambdaThetaClimRelax, myThid )
      CALL EXCH_XY_RS( lambdaSaltClimRelax , myThid )
#ifdef SHORTWAVE_HEATING
      CALL EXCH_XY_RS(Qsw  , myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      CALL EXCH_XY_RS(pLoad  , myThid )
C     CALL PLOT_FIELD_XYRS( pLoad, 'S/R INI_FORCING pLoad',1,myThid)
#endif
C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

#ifdef ATMOSPHERIC_LOADING
      IF ( pLoadFile .NE. ' ' .AND. usingPCoords ) THEN
C-- This is a hack used to read phi0surf from a file (pLoadFile)
C          instead of computing it from bathymetry & density ref. profile.
C-  Ocean: The true atmospheric P-loading is not yet implemented for P-coord
C          (requires time varying dP(Nr) like dP(k-bottom) with NonLin FS).
C-  Atmos: sometime usefull to overwrite phi0surf with fixed-in-time field
C          read from file (and anyway, pressure loading is meaningless here)
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
             phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF
#endif /* ATMOSPHERIC_LOADING */

      RETURN
      END
