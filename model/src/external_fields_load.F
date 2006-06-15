C $Header: /u/gcmpack/MITgcm/model/src/external_fields_load.F,v 1.26 2006/06/15 16:24:48 jmc Exp $
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
 

#ifndef ALLOW_EXF

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
cph    arrays are now globally visible
cph
cph      IF ( myIter .EQ. nIter0 ) THEN
cph       CALL LEF_ZERO( taux0 ,myThid )
cph       CALL LEF_ZERO( tauy0 ,myThid )
cph       CALL LEF_ZERO( Qnet0 ,myThid )
cph       CALL LEF_ZERO( EmPmR0 ,myThid )
cph       CALL LEF_ZERO( SST0 ,myThid )
cph       CALL LEF_ZERO( SSS0 ,myThid )
cph       CALL LEF_ZERO( taux1 ,myThid )
cph       CALL LEF_ZERO( tauy1 ,myThid )
cph       CALL LEF_ZERO( Qnet1 ,myThid )
cph       CALL LEF_ZERO( EmPmR1 ,myThid )
cph       CALL LEF_ZERO( SST1 ,myThid )
cph       CALL LEF_ZERO( SSS1 ,myThid )
#ifdef ATMOSPHERIC_LOADING
cph       CALL LEF_ZERO( pload0 ,myThid )
cph       CALL LEF_ZERO( pload1 ,myThid )
#endif
#ifdef SHORTWAVE_HEATING
cph       CALL LEF_ZERO( Qsw0 ,myThid )
cph       CALL LEF_ZERO( Qsw1 ,myThid )
#endif
cph      ENDIF

C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(externForcingCycle/externForcingPeriod)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(externForcingPeriod*rdt)
      Ifcyc = NINT(externForcingCycle*rdt)
      Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

      intime0 = 1 + INT(Iftm/Ifprd)
      intime1 = 1 + MOD(intime0,nForcingPeriods)
C-jmc: with some option of g77, FLOAT results in real*4 evaluation
C      of aWght; using DFLOAT always force real*8 computation:
C-ph: however, TAF doesnt recognize DFLOAT, 
C     so I put it back to FLOAT for now 
c     aWght =  FLOAT( Iftm-Ifprd*(intime0 - 1) ) /  FLOAT( Ifprd )
c     aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
C-jmc: so let's try this:
      tmp1Wght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
      tmp2Wght = FLOAT( Ifprd )
      aWght =  tmp1Wght / tmp2Wght
      bWght = 1. _d 0 - aWght

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(standardMessageUnit,'(A,2I5,I10,1P1E20.12)')
     &  'S/R EXTERNAL_FIELDS_LOAD: Reading new data:',
     &  intime0, intime1, myIter, myTime

#ifdef ALLOW_MDSIO
      IF ( zonalWindFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( zonalWindFile, readBinaryPrec,
     &        'RS', 1, taux0, intime0, myThid )
       CALL MDSREADFIELD ( zonalWindFile, readBinaryPrec,
     &        'RS', 1, taux1, intime1, myThid )
      ENDIF
      IF ( meridWindFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( meridWindFile, readBinaryPrec,
     &        'RS', 1, tauy0, intime0, myThid )
       CALL MDSREADFIELD ( meridWindFile, readBinaryPrec,
     &        'RS', 1, tauy1, intime1, myThid )
      ENDIF
      IF ( surfQFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( surfQFile, readBinaryPrec,
     &        'RS', 1, Qnet0, intime0, myThid )
       CALL MDSREADFIELD ( surfQFile, readBinaryPrec,
     &        'RS', 1, Qnet1, intime1, myThid )
      ELSEIF ( surfQnetFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( surfQnetFile, readBinaryPrec,
     &        'RS', 1, Qnet0, intime0, myThid )
       CALL MDSREADFIELD ( surfQnetFile, readBinaryPrec,
     &        'RS', 1, Qnet1, intime1, myThid )
      ENDIF
      IF ( EmPmRfile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( EmPmRfile, readBinaryPrec,
     &        'RS', 1, EmPmR0, intime0, myThid )
       CALL MDSREADFIELD ( EmPmRfile, readBinaryPrec,
     &        'RS', 1, EmPmR1, intime1, myThid )
      ENDIF
      IF ( saltFluxFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( saltFluxFile, readBinaryPrec,
     &        'RS', 1, saltFlux0, intime0, myThid )
       CALL MDSREADFIELD ( saltFluxFile, readBinaryPrec,
     &        'RS', 1, saltFlux1, intime1, myThid )
      ENDIF
      IF ( thetaClimFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( thetaClimFile, readBinaryPrec,
     &        'RS', 1, SST0, intime0, myThid )
       CALL MDSREADFIELD ( thetaClimFile, readBinaryPrec,
     &        'RS', 1, SST1, intime1, myThid )
      ENDIF
      IF ( saltClimFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( saltClimFile, readBinaryPrec,
     &        'RS', 1, SSS0, intime0, myThid )
       CALL MDSREADFIELD ( saltClimFile, readBinaryPrec,
     &        'RS', 1, SSS1, intime1, myThid )
      ENDIF
#ifdef SHORTWAVE_HEATING
      IF ( surfQswFile .NE. ' '  ) THEN
       CALL MDSREADFIELD ( surfQswFile, readBinaryPrec,
     &        'RS', 1, Qsw0, intime0, myThid )
       CALL MDSREADFIELD ( surfQswFile, readBinaryPrec,
     &        'RS', 1, Qsw1, intime1, myThid )
       IF ( surfQFile .NE. ' '  ) THEN
C-     Qnet is now (after c54) the net Heat Flux (including SW)
        DO bj=1,nSy
         DO bi=1,nSx
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
       CALL MDSREADFIELD ( pLoadFile, readBinaryPrec,
     &        'RS', 1, pload0, intime0, myThid )
       CALL MDSREADFIELD ( pLoadFile, readBinaryPrec,
     &        'RS', 1, pload1, intime1, myThid )
      ENDIF
#endif
#endif /* ALLOW_MDSIO */

       _END_MASTER(myThid)
C
       _EXCH_XY_R4(SST0  , myThid )
       _EXCH_XY_R4(SST1  , myThid )
       _EXCH_XY_R4(SSS0  , myThid )
       _EXCH_XY_R4(SSS1  , myThid )
c      _EXCH_XY_R4(taux0 , myThid )
c      _EXCH_XY_R4(taux1 , myThid )
c      _EXCH_XY_R4(tauy0 , myThid )
c      _EXCH_XY_R4(tauy1 , myThid )
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
       _EXCH_XY_R4(pload0, myThid )
       _EXCH_XY_R4(pload1, myThid )
#endif
C
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
            pload(i,j,bi,bj) = bWght*pload0(i,j,bi,bj)  
     &                       + aWght*pload1(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDIF
#endif
C- jmc: leave the old code commented out (with all fields beeing reset, 
C       even if they are never loaded)
c       DO j=1-Oly,sNy+Oly
c        DO i=1-Olx,sNx+Olx
c         SST(i,j,bi,bj)   = bWght*SST0(i,j,bi,bj)  
c    &                       +aWght*SST1(i,j,bi,bj)
c         SSS(i,j,bi,bj)   = bWght*SSS0(i,j,bi,bj)  
c    &                       +aWght*SSS1(i,j,bi,bj)
c         fu(i,j,bi,bj)    = bWght*taux0(i,j,bi,bj) 
c    &                       +aWght*taux1(i,j,bi,bj)
c         fv(i,j,bi,bj)    = bWght*tauy0(i,j,bi,bj) 
c    &                       +aWght*tauy1(i,j,bi,bj)
c         Qnet(i,j,bi,bj)  = bWght*Qnet0(i,j,bi,bj)
c    &                       +aWght*Qnet1(i,j,bi,bj)
c         EmPmR(i,j,bi,bj) = bWght*EmPmR0(i,j,bi,bj)
c    &                       +aWght*EmPmR1(i,j,bi,bj)
c         saltFlux(i,j,bi,bj) = bWght*saltFlux0(i,j,bi,bj)
c    &                        + aWght*saltFlux1(i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
c         Qsw(i,j,bi,bj)   = bWght*Qsw0(i,j,bi,bj)
c    &                       +aWght*Qsw1(i,j,bi,bj)
#endif
#ifdef ATMOSPHERIC_LOADING
c         pload(i,j,bi,bj) = bWght*pload0(i,j,bi,bj)  
c    &                      +aWght*pload1(i,j,bi,bj)
#endif
c        ENDDO
c       ENDDO
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

#endif /* ALLOW_EXF */

      RETURN
      END
