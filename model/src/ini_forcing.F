C $Header: /u/gcmpack/MITgcm/model/src/ini_forcing.F,v 1.40 2005/12/13 19:47:39 heimbach Exp $
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
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_FORCING
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj  - Loop counters
C     I,J
      INTEGER bi, bj
      INTEGER  I, J, K
CEOP

      _BARRIER

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
#ifdef ATMOSPHERIC_LOADING
          pload           (i,j,bi,bj) = 0. _d 0
          sIceLoad        (i,j,bi,bj) = 0. _d 0
#endif
          surfaceForcingU(i,j,bi,bj) = 0. _d 0
          surfaceForcingV(i,j,bi,bj) = 0. _d 0
          surfaceForcingT(i,j,bi,bj) = 0. _d 0
          surfaceForcingS(i,j,bi,bj) = 0. _d 0
          surfaceForcingTice(i,j,bi,bj) = 0. _d 0
#ifndef ALLOW_EXF
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
          pload0          (i,j,bi,bj) = 0. _d 0
          pload1          (i,j,bi,bj) = 0. _d 0
#endif
#endif
         ENDDO
        ENDDO
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
#ifdef ALLOW_CLIMTEMP_RELAXATION
           thetaStar (i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_CLIMSALT_RELAXATION
           saltStar  (i,j,k,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J=1-Oly,sNy+Oly
         DO I=1-Olx,sNx+Olx
          IF ( doThetaClimRelax .AND. 
     &         abs(yC(i,j,bi,bj)).LE.latBandClimRelax ) THEN
           lambdaThetaClimRelax(I,J,bi,bj) = 1./tauThetaClimRelax
          ELSE
           lambdaThetaClimRelax(I,J,bi,bj) = 0.D0
          ENDIF
          IF ( doSaltClimRelax .AND.
     &         abs(yC(i,j,bi,bj)).LE.latBandClimRelax ) THEN
           lambdaSaltClimRelax(I,J,bi,bj) = 1./tauSaltClimRelax
          ELSE
           lambdaSaltClimRelax(I,J,bi,bj) = 0.D0
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C
      _BARRIER
      _BEGIN_MASTER(myThid)
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
        DO bj = 1,nSy
         DO bi = 1,nSx
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
       CALL READ_FLD_XY_RS( pLoadFile, ' ', pload, 0, myThid )
      ENDIF
#endif
#ifdef ALLOW_CLIMTEMP_RELAXATION
      IF ( hydrogThetaFile .NE. ' '  ) THEN
       CALL READ_FLD_XYZ_RS( 
     &  hydrogThetaFile, ' ', thetaStar, 0, myThid )
      ENDIF
#endif
#ifdef ALLOW_CLIMSALT_RELAXATION
      IF ( hydrogSaltFile .NE. ' '  ) THEN
       CALL READ_FLD_XYZ_RS( 
     &  hydrogSaltFile, ' ', saltStar, 0, myThid )
      ENDIF
#endif

      _END_MASTER(myThid)
C
      _EXCH_XY_R4(fu   , myThid )
      _EXCH_XY_R4(fv   , myThid )
      _EXCH_XY_R4(Qnet , myThid )
      _EXCH_XY_R4(EmPmR, myThid )
      _EXCH_XY_R4( saltFlux, myThid )
      _EXCH_XY_R4(SST  , myThid )
      _EXCH_XY_R4(SSS  , myThid )
      _EXCH_XY_R4(lambdaThetaClimRelax , myThid )
      _EXCH_XY_R4(lambdaSaltClimRelax , myThid )
#ifdef SHORTWAVE_HEATING
      _EXCH_XY_R4(Qsw  , myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
      _EXCH_XY_R4(pload  , myThid )
C     CALL PLOT_FIELD_XYRS( pload, 'S/R INI_FORCING pload',1,myThid)
#endif
#ifdef ALLOW_CLIMTEMP_RELAXATION
      _EXCH_XYZ_R4(thetaStar , myThid )
#endif
#ifdef ALLOW_CLIMSALT_RELAXATION
      _EXCH_XYZ_R4(saltStar , myThid )
#endif
C     CALL PLOT_FIELD_XYRS( fu, 'S/R INI_FORCING FU',1,myThid)
C     CALL PLOT_FIELD_XYRS( fv, 'S/R INI_FORCING FV',1,myThid)

      RETURN
      END
