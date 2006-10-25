C $Header: /u/gcmpack/MITgcm/verification/natl_box/code/Attic/external_fields_load.F,v 1.13 2006/10/25 16:05:10 jmc Exp $
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
cph       CALL LEF_ZERO( Qsw0 ,myThid )
cph       CALL LEF_ZERO( taux1 ,myThid )
cph       CALL LEF_ZERO( tauy1 ,myThid )
cph       CALL LEF_ZERO( Qnet1 ,myThid )
cph       CALL LEF_ZERO( EmPmR1 ,myThid )
cph       CALL LEF_ZERO( SST1 ,myThid )
cph       CALL LEF_ZERO( SSS1 ,myThid )
cph       CALL LEF_ZERO( Qsw1 ,myThid )
cph      ENDIF

C Now calculate whether it is time to update the forcing arrays
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=int(externForcingCycle/externForcingPeriod)
      Imytm=int(myTime*rdt)
      Ifprd=int(externForcingPeriod*rdt)
      Ifcyc=int(externForcingCycle*rdt)
      Iftm=mod( Imytm ,Ifcyc)

      intime0=int(Iftm/Ifprd)
      intime1=mod(intime0+1,nForcingPeriods)
      aWght=mod(myTime/externForcingPeriod,1. _d 0)
      bWght=1.-aWght

      intime0=intime0+1
      intime1=intime1+1

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
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw0, myThid )
       _EXCH_XY_R4(Qsw1, myThid )
#endif
C
      ENDIF

C--   Interpolate fu,fv,Qnet,EmPmR,SST,SSS,Qsw
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          SST(i,j,bi,bj)   = bWght*SST0(i,j,bi,bj)  
     &                       +aWght*SST1(i,j,bi,bj)
          SSS(i,j,bi,bj)   = bWght*SSS0(i,j,bi,bj)  
     &                       +aWght*SSS1(i,j,bi,bj)
          fu(i,j,bi,bj)    = bWght*taux0(i,j,bi,bj) 
     &                       +aWght*taux1(i,j,bi,bj)
          fv(i,j,bi,bj)    = bWght*tauy0(i,j,bi,bj) 
     &                       +aWght*tauy1(i,j,bi,bj)
          Qnet(i,j,bi,bj)  = bWght*Qnet0(i,j,bi,bj)
     &                       +aWght*Qnet1(i,j,bi,bj)
          EmPmR(i,j,bi,bj) = bWght*EmPmR0(i,j,bi,bj)
     &                       +aWght*EmPmR1(i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
          Qsw(i,j,bi,bj)   = bWght*Qsw0(i,j,bi,bj)
     &                       +aWght*Qsw1(i,j,bi,bj)
#endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C-- Diagnostics
      IF (myThid.EQ.1 .AND. myTime.LT.62208000.) THEN
        write(*,'(a,1p7e12.4,2i6,2e12.4)')
     &   'time,SST,SSS,fu,fv,Q,E-P,i0,i1,a,b = ',
     &   myTime,
     &   SST(1,sNy,1,1),SSS(1,sNy,1,1),
     &   fu(1,sNy,1,1),fv(1,sNy,1,1),
     &   Qnet(1,sNy,1,1),EmPmR(1,sNy,1,1),
     &   intime0,intime1,aWght,bWght
        write(*,'(a,1p4e12.4,2E23.15)')
     &   'time,fu0,fu1,fu = ',
     &   myTime,
     &   taux0(1,sNy,1,1),taux1(1,sNy,1,1),fu(1,sNy,1,1),
     &   aWght,bWght
      ENDIF

C endif for periodicForcing
      ENDIF

#endif /* ALLOW_EXF */

      RETURN
      END
