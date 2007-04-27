C $Header: /u/gcmpack/MITgcm/model/src/external_fields_load.F,v 1.29 2007/04/27 15:45:29 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: EXTERNAL_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE EXTERNAL_FIELDS_LOAD
C     | o Control reading of fields from external source.
C     *==========================================================*
C     | External source field loading routine.
C     | This routine is called every time we want to
C     | load a a set of external fields. The routine decides
C     | which fields to load and then reads them in.
C     | This routine needs to be customised for particular
C     | experiments.
C     | Notes
C     | =====
C     | Two-dimensional and three-dimensional I/O are handled in
C     | the following way under MITgcmUV. A master thread
C     | performs I/O using system calls. This threads reads data
C     | into a temporary buffer. At present the buffer is loaded
C     | with the entire model domain. This is probably OK for now
C     | Each thread then copies data from the buffer to the
C     | region of the proper array it is responsible for.
C     | =====
C     | Conversion of flux fields are described in FFIELDS.h
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter


#ifndef EXCLUDE_FFIELDS_LOAD


C     !LOCAL VARIABLES:
C     === Local arrays ===
C     aWght, bWght :: Interpolation weights
      INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      _RL tmp1Wght, tmp2Wght
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
CEOP

      IF ( periodicExternalForcing ) THEN

C First call requires that we initialize everything to zero for safety
cph    has been shifted to ini_forcing.F

C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(externForcingCycle/externForcingPeriod)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(externForcingPeriod*rdt)
      Ifcyc = NINT(externForcingCycle*rdt)
      Imytm = Imytm + Ifcyc*( 1 - NINT(myTime/externForcingCycle) )
      Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

      intime0 = 1 + INT(Iftm/Ifprd)
      intime1 = 1 + MOD(intime0,nForcingPeriods)
C-jmc: with some option of g77, FLOAT results in real*4 evaluation
C      of aWght; using DFLOAT always force real*8 computation:
c     aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
C-ph: however, TAF doesnt recognize DFLOAT,
C-jmc: so let's try this:
      tmp1Wght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
      tmp2Wght = FLOAT( Ifprd )
      aWght =  tmp1Wght / tmp2Wght
      bWght = 1. _d 0 - aWght

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BARRIER

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       _BEGIN_MASTER(myThid)
       WRITE(standardMessageUnit,'(A,2I5,I10,1P1E20.12)')
     &  'S/R EXTERNAL_FIELDS_LOAD: Reading new data:',
     &  intime0, intime1, myIter, myTime
       _END_MASTER(myThid)

      IF ( zonalWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( zonalWindFile, taux0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( zonalWindFile, taux1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( meridWindFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( meridWindFile, tauy0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( meridWindFile, tauy1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( surfQFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( surfQFile, Qnet0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( surfQFile, Qnet1,
     &                      intime1, myIter, myThid )
      ELSEIF ( surfQnetFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( surfQnetFile, Qnet0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( surfQnetFile, Qnet1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( EmPmRfile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( EmPmRfile, EmPmR0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( EmPmRfile, EmPmR1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( saltFluxFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( saltFluxFile, saltFlux0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( saltFluxFile, saltFlux1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( thetaClimFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( thetaClimFile, SST0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( thetaClimFile, SST1,
     &                      intime1, myIter, myThid )
      ENDIF
      IF ( saltClimFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( saltClimFile, SSS0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( saltClimFile, SSS1,
     &                      intime1, myIter, myThid )
      ENDIF
#ifdef SHORTWAVE_HEATING
      IF ( surfQswFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( surfQswFile, Qsw0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( surfQswFile, Qsw1,
     &                      intime1, myIter, myThid )
       IF ( surfQFile .NE. ' '  ) THEN
C-     Qnet is now (after c54) the net Heat Flux (including SW)
        _BARRIER
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            Qnet0(i,j,bi,bj) = Qnet0(i,j,bi,bj) + Qsw0(i,j,bi,bj)
            Qnet1(i,j,bi,bj) = Qnet1(i,j,bi,bj) + Qsw1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif
#ifdef ATMOSPHERIC_LOADING
      IF ( pLoadFile .NE. ' '  ) THEN
       CALL READ_REC_XY_RS( pLoadFile, pLoad0,
     &                      intime0, myIter, myThid )
       CALL READ_REC_XY_RS( pLoadFile, pLoad1,
     &                      intime1, myIter, myThid )
      ENDIF
#endif

C-    thread synchronisation (barrier) is part of the EXCH S/R calls
       _EXCH_XY_R4(SST0  , myThid )
       _EXCH_XY_R4(SST1  , myThid )
       _EXCH_XY_R4(SSS0  , myThid )
       _EXCH_XY_R4(SSS1  , myThid )
       CALL EXCH_UV_XY_RS(taux0,tauy0,.TRUE.,myThid)
       CALL EXCH_UV_XY_RS(taux1,tauy1,.TRUE.,myThid)
       _EXCH_XY_R4(Qnet0, myThid )
       _EXCH_XY_R4(Qnet1, myThid )
       _EXCH_XY_R4(EmPmR0, myThid )
       _EXCH_XY_R4(EmPmR1, myThid )
       _EXCH_XY_R4(saltFlux0, myThid )
       _EXCH_XY_R4(saltFlux1, myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw0, myThid )
       _EXCH_XY_R4(Qsw1, myThid )
#endif
#ifdef ATMOSPHERIC_LOADING
       _EXCH_XY_R4(pLoad0, myThid )
       _EXCH_XY_R4(pLoad1, myThid )
#endif

      ENDIF

C--   Interpolate fu,fv,Qnet,EmPmR,SST,SSS,Qsw
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        IF ( thetaClimFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            SST(i,j,bi,bj)   = bWght*SST0(i,j,bi,bj)
     &                       + aWght*SST1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( saltClimFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            SSS(i,j,bi,bj)   = bWght*SSS0(i,j,bi,bj)
     &                       + aWght*SSS1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( zonalWindFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            fu(i,j,bi,bj)    = bWght*taux0(i,j,bi,bj)
     &                       + aWght*taux1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( meridWindFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            fv(i,j,bi,bj)    = bWght*tauy0(i,j,bi,bj)
     &                       + aWght*tauy1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( surfQnetFile .NE. ' '
     &     .OR. surfQFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            Qnet(i,j,bi,bj)  = bWght*Qnet0(i,j,bi,bj)
     &                       + aWght*Qnet1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( EmPmRfile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            EmPmR(i,j,bi,bj) = bWght*EmPmR0(i,j,bi,bj)
     &                       + aWght*EmPmR1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
        IF ( saltFluxFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            saltFlux(i,j,bi,bj) = bWght*saltFlux0(i,j,bi,bj)
     &                          + aWght*saltFlux1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#ifdef SHORTWAVE_HEATING
        IF ( surfQswFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            Qsw(i,j,bi,bj)   = bWght*Qsw0(i,j,bi,bj)
     &                       + aWght*Qsw1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif
#ifdef ATMOSPHERIC_LOADING
        IF ( pLoadFile .NE. ' '  ) THEN
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            pLoad(i,j,bi,bj) = bWght*pLoad0(i,j,bi,bj)
     &                       + aWght*pLoad1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif
       ENDDO
      ENDDO

C-- Print for checking:
c     IF ( debugLevel.GE.debLevA  .AND. myIter.LT.50+nIter0) THEN
      IF ( debugLevel.GE.debLevA  .AND. myTime.LT.62208000.) THEN
        _BEGIN_MASTER( myThid )
        WRITE(standardMessageUnit,'(a,1p7e12.4,2i6,2e12.4)')
     &   'time,SST,SSS,fu,fv,Q,E-P,i0,i1,a,b = ',
     &   myTime,
     &   SST(1,sNy,1,1),SSS(1,sNy,1,1),
     &   fu(1,sNy,1,1),fv(1,sNy,1,1),
     &   Qnet(1,sNy,1,1),EmPmR(1,sNy,1,1),
     &   intime0,intime1,aWght,bWght
        WRITE(standardMessageUnit,'(a,1p4e12.4,2E23.15)')
     &   'time,fu0,fu1,fu = ',
     &   myTime,
     &   taux0(1,sNy,1,1),taux1(1,sNy,1,1),fu(1,sNy,1,1),
     &   aWght,bWght
        _END_MASTER( myThid )
      ENDIF

C endif for periodicForcing
      ENDIF

#endif /* EXCLUDE_FFIELDS_LOAD */

      RETURN
      END
