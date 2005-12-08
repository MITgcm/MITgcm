
#include "CPP_OPTIONS.h"
#include "PACKAGES_CONFIG.h"
 
C     !ROUTINE: RBCS_FIELDS_LOAD
C     !INTERFACE:
      SUBROUTINE RBCS_FIELDS_LOAD( myTime, myIter, myThid )
C     *==========================================================*
C     | SUBROUTINE RBCS_FIELDS_LOAD                           
C     | o Control reading of fields from external source.         
C     *==========================================================*
C     | Offline External source field loading routine.                    
C     | This routine is called every time we want to              
C     | load a a set of external fields. The routine decides      
C     | which fields to load and then reads them in.              
C     | This routine needs to be customised for particular        
C     | experiments.                                              
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#endif
#include "RBCS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
C     myTime - Simulation time
C     myIter - Simulation timestep number
      INTEGER myThid
      _RL     myTime
      INTEGER myIter

c     fn      :: Temp. for building file name.
      CHARACTER*(MAX_LEN_FNAM) fn
      CHARACTER*(MAX_LEN_FNAM) fn2
      INTEGER prec

 

C     !LOCAL VARIABLES:
C     === Local arrays ===
C     [01]      :: End points for interpolation
C     Above use static heap storage to allow exchange.
C     aWght, bWght :: Interpolation weights
      COMMON /RBCFFIELDS/
     &                 rbct0, rbcs0,  
     &                 rbct1, rbcs1
      _RS  rbct0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbct1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs0    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
      _RS  rbcs1    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)

#ifdef ALLOW_PTRACERS
       COMMON /RBCFFTRACER/
     &               rbcptracer0, rbcptracer1
       _RS rbcptracer0(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
       _RS rbcptracer1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy,
     &              PTRACERS_num)
#endif

      INTEGER bi,bj,i,j,k,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
      INTEGER I1, I2
      INTEGER iTracer
      INTEGER  IFNBLNK, ILNBLNK
      EXTERNAL IFNBLNK, ILNBLNK

#ifdef ALLOW_RBCS
      CALL TIMER_START('RBCS_FIELDS_LOAD      [I/O]', myThid)
      prec = precFloat32

c     IF ( rbcsForcingCycle.GT.0.d0 ) THEN

C First call requires that we initialize everything to zero for safety
      IF ( myIter .EQ. nIter0 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        do k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           rbct0(i,j,k,bi,bj)=0.d0
           rbcs0(i,j,k,bi,bj)=0.d0
           rbct1(i,j,k,bi,bj)=0.d0
           rbcs1(i,j,k,bi,bj)=0.d0
          ENDDO
         ENDDO
        enddo
       ENDDO
       ENDDO
#ifdef ALLOW_PTRACERS
       DO iTracer = 1, PTRACERS_numInUse
        DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         do k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            rbcptracer0(i,j,k,bi,bj,iTracer)=0.d0
            rbcptracer1(i,j,k,bi,bj,iTracer)=0.d0
           ENDDO
          ENDDO
         enddo
        ENDDO
        ENDDO
       ENDDO
#endif
      ENDIF

C Now calculate whether it is time to update the forcing arrays
      if (rbcsForcingCycle.gt.0.d0) then
       rdt=1. _d 0 / deltaTclock
       nForcingPeriods=int(rbcsForcingCycle/rbcsForcingPeriod+0.5)
       Imytm=int((myTime-float(rbcsIniter)*deltaTclock)*rdt+0.5)
       Ifprd=int(rbcsForcingPeriod*rdt+0.5)
       Ifcyc=int(rbcsForcingCycle*rdt+0.5)
       Iftm=mod( Imytm+Ifcyc-Ifprd/2 ,Ifcyc)
 
       intime0=int(Iftm/Ifprd)
       intime1=mod(intime0+1,nForcingPeriods)
       aWght=float( Iftm-Ifprd*intime0 )/float( Ifprd )
       bWght=1.-aWght
 
       intime0=intime0+1
       INTIME1=intime1+1
      else
       intime1=1
       intime0=1
       Iftm=1
       Ifprd=0
       aWght=.5d0
       bWght=.5d0
      endif

      IF (
     &  Iftm-Ifprd*(intime0-1) .EQ. 0
     &  .OR. myIter .EQ. nIter0
     & ) THEN

       _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
       WRITE(*,*)
     &  'S/R RBCS_FIELDS_LOAD: Reading new data',myTime,myIter
     &            , nIter0, intime0,intime1

c
       IF ( relaxTFile      .NE. ' '  ) THEN
        CALL MDSREADFIELD ( relaxTFile, prec,
     &        'RS', Nr, rbct0, intime0, myThid )
        CALL MDSREADFIELD ( relaxTFile, prec,
     &        'RS', Nr, rbct1, intime1, myThid )
       ENDIF
       IF ( relaxSFile      .NE. ' '  ) THEN
        CALL MDSREADFIELD ( relaxSFile, prec,
     &        'RS', Nr, rbcs0, intime0, myThid )
        CALL MDSREADFIELD ( relaxSFile, prec,
     &        'RS', Nr, rbcs1, intime1, myThid )
       ENDIF

#ifdef ALLOW_PTRACERS
       if (useRBCptracers) then
         DO iTracer = 1, PTRACERS_numInUse
           if (useRBCptrnum(iTracer)) then
            WRITE(fn,'(A)') relaxPtracerFile(iTracer)
            CALL MDSREADFIELD ( fn, prec,
     &       'RS', Nr, rbcptracer0(1,1,1,1,1,iTracer), intime0, myThid )
            CALL MDSREADFIELD ( fn, prec,
     &       'RS', Nr, rbcptracer1(1,1,1,1,1,iTracer), intime1, myThid )
           endif
         ENDDO
       endif
#endif

c

       _END_MASTER(myThid)

C
       _EXCH_XYZ_R4(rbct0 , myThid )
       _EXCH_XYZ_R4(rbct1 , myThid )
       _EXCH_XYZ_R4(rbcs0 , myThid )
       _EXCH_XYZ_R4(rbcs1 , myThid )
#ifdef ALLOW_PTRACERS
       if (useRBCptracers) then
         DO iTracer = 1, PTRACERS_numInUse
           _EXCH_XYZ_R4(rbcptracer0(1,1,1,1,1,iTracer),myThid)
           _EXCH_XYZ_R4(rbcptracer1(1,1,1,1,1,iTracer),myThid)
         ENDDO
       endif
#endif
 
c
      ENDIF
c
    
C--   Interpolate 
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        do k=1,Nr
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          RBCtemp(i,j,k,bi,bj)   = bWght*rbct0(i,j,k,bi,bj)  
     &                       +aWght*rbct1(i,j,k,bi,bj)
          RBCsalt(i,j,k,bi,bj)    = bWght*rbcs0(i,j,k,bi,bj) 
     &                       +aWght*rbcs1(i,j,k,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDDO

#ifdef ALLOW_PTRACERS
      if (useRBCptracers) then
        DO iTracer = 1, PTRACERS_numInUse
         if (useRBCptrnum(iTracer)) then
          DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           do k=1,Nr
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             RBC_ptracers(i,j,k,bi,bj,iTracer)   = 
     &                      bWght*rbcptracer0(i,j,k,bi,bj,iTracer)
     &                     +aWght*rbcptracer1(i,j,k,bi,bj,iTracer)
            ENDDO
           ENDDO
           ENDDO
          ENDDO
          ENDDO
         endif
        ENDDO
      endif
#endif

CC-- Diagnostics
C      IF (myThid.EQ.1 .AND. myTime.LT.62208000.) THEN
C        write(*,'(a,1p5e12.4,3i6,2e12.4)')
C     &   'time,U,V,W,i0,i1,a,b = ',
C     &   myTime,
C     &   Uvel(1,sNy,1,1,1),Vvel(1,sNy,1,1,1),
C     &   Wvel(1,sNy,1,1,1),
C     &   intime0,intime1,aWght,bWght
C        write(*,'(a,1p4e12.4,2e12.4)')
C     &   'time,uvel0,uvel1,U = ',
C     &   myTime,
C     &   uvel0(1,sNy,1,1,1),uvel1(1,sNy,1,1,1),Uvel(1,sNy,1,1,1),
C     &   aWght,bWght
C      ENDIF

        CALL TIMER_STOP ('RBCS_FIELDS_LOAD      [I/O]', myThid)
C endif for periodicForcing
c     ENDIF

#endif   
c! ALLOW_RBCS

      RETURN
      END

