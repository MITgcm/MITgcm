C $Header: /u/gcmpack/MITgcm/pkg/obcs/Attic/obcs_external_fields_load.F,v 1.4 2005/07/06 08:22:00 mlosch Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"
 
CBOP
C     !ROUTINE: OBCS_EXTERNAL_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE OBCS_EXTERNAL_FIELDS_LOAD( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OBCS_EXTERNAL_FIELDS_LOAD                           
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
C     | This routine is the complete analogue to external_fields_load,
C     | except for exchanges of forcing fields. These are done in
C     | obcs_precribe_exchanges, which is called from dynamics.
C     | - Forcing period and cycle are the same as for other fields
C     |   in external forcing.
C     | - constant boundary values are also read here and not
C     |   directly in obcs_init_variables (which calls obcs_calc 
C     |   which in turn call this routine)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter
 
#if (defined ALLOW_OBCS && defined ALLOW_OBCS_PRESCRIBE)
C     if external forcing (exf) package is enabled, all loading of external
C     fields is done by exf
#ifndef ALLOW_EXF
#include "OBCS.h"

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     aWght, bWght :: Interpolation weights
      INTEGER bi,bj,i,j,k,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
CEOP

      IF ( periodicExternalForcing ) THEN

C Now calculate whether it is time to update the forcing arrays
      rdt=1. _d 0 / deltaTclock
      nForcingPeriods=int(externForcingCycle/externForcingPeriod+0.5)
      Imytm=int(myTime*rdt+0.5)
      Ifprd=int(externForcingPeriod*rdt+0.5)
      Ifcyc=int(externForcingCycle*rdt+0.5)
      Iftm=mod( Imytm+Ifcyc-Ifprd/2 ,Ifcyc)

      intime0=int(Iftm/Ifprd)
      intime1=mod(intime0+1,nForcingPeriods)
      aWght=float( Iftm-Ifprd*intime0 )/float( Ifprd )
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
     &  'S/R OBCS_EXTERNAL_FIELDS_LOAD: Reading new data:',
     &  intime0, intime1, myIter, myTime

#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
      IF ( OBEuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu0, intime0, myThid )
       CALL MDSREADFIELDXz ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu1, intime1, myThid )
      ENDIF
      IF ( OBEvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv1, intime1, myThid )
      ENDIF
      IF ( OBEtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt1, intime1, myThid )
      ENDIF
      IF ( OBEsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
      IF ( OBWuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu0, intime0, myThid )
       CALL MDSREADFIELDXz ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu1, intime1, myThid )
      ENDIF
      IF ( OBWvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv1, intime1, myThid )
      ENDIF
      IF ( OBWtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt1, intime1, myThid )
      ENDIF
      IF ( OBWsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs0, intime0, myThid )
       CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
      IF ( OBNuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu0, intime0, myThid )
       CALL MDSREADFIELDXz ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu1, intime1, myThid )
      ENDIF
      IF ( OBNvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv1, intime1, myThid )
      ENDIF
      IF ( OBNtFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt1, intime1, myThid )
      ENDIF
      IF ( OBNsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
      IF ( OBSuFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu0, intime0, myThid )
       CALL MDSREADFIELDXz ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu1, intime1, myThid )
      ENDIF
      IF ( OBSvFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv1, intime1, myThid )
      ENDIF
      IF ( OBStFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt1, intime1, myThid )
      ENDIF
      IF ( OBSsFile .NE. ' '  ) THEN
       CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs0, intime0, myThid )
       CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs1, intime1, myThid )
      ENDIF
#endif /* ALLOW_OBCS_SOUTH */
       _END_MASTER(myThid)
C
C     At this point in external_fields_load the input fields are exchanged.
C     However, we do not have exchange routines for vertical 
C     slices and they are not planned, either, so the approriate fields
C     are exchanged after the open boundary conditions have been 
C     applied. (in DYNAMICS and DO_FIELDS_BLOCKING_EXCHANGES)
C
      ENDIF

C--   Interpolate OBSu, OBSv, OBSt, OBSs
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO j=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
          OBEu(j,k,bi,bj)   = bWght*OBEu0(j,k,bi,bj)  
     &                       +aWght*OBEu1(j,k,bi,bj)
          OBEv(j,k,bi,bj)   = bWght*OBEv0(j,k,bi,bj)  
     &                       +aWght*OBEv1(j,k,bi,bj)
          OBEt(j,k,bi,bj)   = bWght*OBEt0(j,k,bi,bj)  
     &                       +aWght*OBEt1(j,k,bi,bj)
          OBEs(j,k,bi,bj)   = bWght*OBEs0(j,k,bi,bj)  
     &                       +aWght*OBEs1(j,k,bi,bj)
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST 
          OBWu(j,k,bi,bj)   = bWght*OBWu0(j,k,bi,bj)  
     &                       +aWght*OBWu1(j,k,bi,bj)
          OBWv(j,k,bi,bj)   = bWght*OBWv0(j,k,bi,bj)  
     &                       +aWght*OBWv1(j,k,bi,bj)
          OBWt(j,k,bi,bj)   = bWght*OBWt0(j,k,bi,bj)  
     &                       +aWght*OBWt1(j,k,bi,bj)
          OBWs(j,k,bi,bj)   = bWght*OBWs0(j,k,bi,bj)  
     &                       +aWght*OBWs1(j,k,bi,bj)
#endif /* ALLOW_OBCS_WEST */
         ENDDO	
         DO i=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
          OBNu(i,k,bi,bj)   = bWght*OBNu0(i,k,bi,bj)  
     &                       +aWght*OBNu1(i,k,bi,bj)
          OBNv(i,k,bi,bj)   = bWght*OBNv0(i,k,bi,bj)  
     &                       +aWght*OBNv1(i,k,bi,bj)
          OBNt(i,k,bi,bj)   = bWght*OBNt0(i,k,bi,bj)  
     &                       +aWght*OBNt1(i,k,bi,bj)
          OBNs(i,k,bi,bj)   = bWght*OBNs0(i,k,bi,bj)  
     &                       +aWght*OBNs1(i,k,bi,bj)
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
          OBSu(i,k,bi,bj)   = bWght*OBSu0(i,k,bi,bj)  
     &                       +aWght*OBSu1(i,k,bi,bj)
          OBSv(i,k,bi,bj)   = bWght*OBSv0(i,k,bi,bj)  
     &                       +aWght*OBSv1(i,k,bi,bj)
          OBSt(i,k,bi,bj)   = bWght*OBSt0(i,k,bi,bj)  
     &                       +aWght*OBSt1(i,k,bi,bj)
          OBSs(i,k,bi,bj)   = bWght*OBSs0(i,k,bi,bj)  
     &                       +aWght*OBSs1(i,k,bi,bj)
#endif /* ALLOW_OBCS_SOUTH */
         ENDDO
        ENDDO
       ENDDO
      ENDDO
C     if not periodicForcing
      ELSE
C     read boundary values once and for all
       IF ( myIter .EQ. nIter0 ) THEN
        _BEGIN_MASTER(myThid)
C      Read constant boundary conditions only for myIter = nIter0
       WRITE(*,*)
     &  'S/R OBCS_EXTERNAl_FIELDS_LOAD: Reading new data',myTime,myIter
       inTime0 = 1
#ifdef ALLOW_OBCS_EAST
C     Eastern boundary
        IF ( OBEuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEuFile, readBinaryPrec,
     &        'RL', Nr, OBEu0, inTime0, myThid )
        ENDIF
        IF ( OBEvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEvFile, readBinaryPrec,
     &        'RL', Nr, OBEv0, inTime0, myThid )
        ENDIF
        IF ( OBEtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEtFile, readBinaryPrec,
     &        'RL', Nr, OBEt0, inTime0, myThid )
        ENDIF
        IF ( OBEsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBEsFile, readBinaryPrec,
     &        'RL', Nr, OBEs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_WEST
C     Western boundary
        IF ( OBWuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWuFile, readBinaryPrec,
     &        'RL', Nr, OBWu0, inTime0, myThid )
        ENDIF
        IF ( OBWvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWvFile, readBinaryPrec,
     &        'RL', Nr, OBWv0, inTime0, myThid )
        ENDIF
        IF ( OBWtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWtFile, readBinaryPrec,
     &        'RL', Nr, OBWt0, inTime0, myThid )
        ENDIF
        IF ( OBWsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDYZ ( OBWsFile, readBinaryPrec,
     &        'RL', Nr, OBWs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_WEST */
#ifdef ALLOW_OBCS_NORTH
C     Northern boundary
        IF ( OBNuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXz ( OBNuFile, readBinaryPrec,
     &        'RL', Nr, OBNu0, inTime0, myThid )
        ENDIF
        IF ( OBNvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNvFile, readBinaryPrec,
     &        'RL', Nr, OBNv0, inTime0, myThid )
        ENDIF
        IF ( OBNtFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNtFile, readBinaryPrec,
     &        'RL', Nr, OBNt0, inTime0, myThid )
        ENDIF
        IF ( OBNsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBNsFile, readBinaryPrec,
     &        'RL', Nr, OBNs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
C     Southern boundary
        IF ( OBSuFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXz ( OBSuFile, readBinaryPrec,
     &        'RL', Nr, OBSu0, inTime0, myThid )
        ENDIF
        IF ( OBSvFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBSvFile, readBinaryPrec,
     &        'RL', Nr, OBSv0, inTime0, myThid )
        ENDIF
        IF ( OBStFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBStFile, readBinaryPrec,
     &        'RL', Nr, OBSt0, inTime0, myThid )
        ENDIF
        IF ( OBSsFile .NE. ' '  ) THEN
         CALL MDSREADFIELDXZ ( OBSsFile, readBinaryPrec,
     &        'RL', Nr, OBSs0, inTime0, myThid )
        ENDIF
#endif /* ALLOW_OBCS_SOUTH */
        _END_MASTER(myThid)
C     endif myIter .EQ. nIter0
       ENDIF
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO K = 1, Nr
          DO j=1-Oly,sNy+Oly
#ifdef ALLOW_OBCS_EAST
           OBEu(j,k,bi,bj) = OBEu0(j,k,bi,bj)  
           OBEv(j,k,bi,bj) = OBEv0(j,k,bi,bj)  
           OBEt(j,k,bi,bj) = OBEt0(j,k,bi,bj)  
           OBEs(j,k,bi,bj) = OBEs0(j,k,bi,bj)  
#endif /* ALLOW_OBCS_EAST */
#ifdef ALLOW_OBCS_WEST
           OBWu(j,k,bi,bj) = OBWu0(j,k,bi,bj)  
           OBWv(j,k,bi,bj) = OBWv0(j,k,bi,bj)  
           OBWt(j,k,bi,bj) = OBWt0(j,k,bi,bj)  
           OBWs(j,k,bi,bj) = OBWs0(j,k,bi,bj)  
#endif /* ALLOW_OBCS_WEST */
          ENDDO
          DO i=1-Olx,sNx+Olx
#ifdef ALLOW_OBCS_NORTH
           OBNu(i,k,bi,bj) = OBNu0(i,k,bi,bj)  
           OBNv(i,k,bi,bj) = OBNv0(i,k,bi,bj)  
           OBNt(i,k,bi,bj) = OBNt0(i,k,bi,bj)  
           OBNs(i,k,bi,bj) = OBNs0(i,k,bi,bj)  
#endif /* ALLOW_OBCS_NORTH */
#ifdef ALLOW_OBCS_SOUTH
           OBSu(i,k,bi,bj) = OBSu0(i,k,bi,bj)  
           OBSv(i,k,bi,bj) = OBSv0(i,k,bi,bj)  
           OBSt(i,k,bi,bj) = OBSt0(i,k,bi,bj)  
           OBSs(i,k,bi,bj) = OBSs0(i,k,bi,bj)  
#endif /* ALLOW_OBCS_SOUTH */
          ENDDO
         ENDDO
        ENDDO
       ENDDO	
C     endif for periodicForcing
      ENDIF

#endif /* ALLOW_EXF */
#endif /* ALLOw_OBCS AND ALLOW_OBCS_PRESCRIBE */

      RETURN
      END

