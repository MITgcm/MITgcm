C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_fields_load.F,v 1.7 2007/10/12 21:24:07 stephd Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CFC_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE CFC_FIELDS_LOAD  i                            |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "PTRACERS_SIZE.h"
c#include "PTRACERS.h"
#include "GCHEM.h"
#include "CFC.h"

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_PTRACERS
C     == Local variables ==
      COMMON /cfc_load/
     &    wspeed0, wspeed1, ice0, ice1, atmosp0,
     &    atmosp1
      _RS wspeed0  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wspeed1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS wind  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS ice1  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp0 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS atmosp1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
       INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
c
c
      IF ( gchem_ForcingCycle.gt.0.d0 ) THEN


C First call requires that we initialize everything to zero for safety
cQQQ need to check timing
       IF ( myIter .EQ. nIter0 ) THEN
         CALL LEF_ZERO( wspeed0,myThid )
         CALL LEF_ZERO( wspeed1,myThid )
         CALL LEF_ZERO( atmosp0,myThid )
         CALL LEF_ZERO( atmosp1,myThid )
         CALL LEF_ZERO( ice0,myThid )
         CALL LEF_ZERO( ice1,myThid )
         _BARRIER
       ENDIF


C Now calculate whether it is time to update the forcing arrays
       rdt=1. _d 0 / deltaTclock
       nForcingPeriods=
     &  int(gchem_ForcingCycle/gchem_ForcingPeriod+0.5)
cswd QQ change for placement of chem forcing (ie. after timestep)
       Imytm=int(myTime*rdt+0.5)
       Ifprd=int(gchem_ForcingPeriod*rdt+0.5)
       Ifcyc=int(gchem_ForcingCycle*rdt+0.5)
       Iftm=mod( Imytm+Ifcyc-Ifprd/2,Ifcyc)


       intime0=int(Iftm/Ifprd)
       intime1=mod(intime0+1,nForcingPeriods)
       aWght=float( Iftm-Ifprd*intime0 )
       bWght=float( Ifprd )
       aWght=aWght/bWght
       bWght=1. _d 0 - aWght

       intime0=intime0+1
       intime1=intime1+1


cswd QQ  need nIter0+1 since chem forcing after time step
       IF (
     &   Iftm-Ifprd*(intime0-1).EQ. 0
     &   .OR. myIter .EQ. nIter0
     &    ) THEN



C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
        _BEGIN_MASTER(myThid)
        WRITE(*,*)
     &    'S/R EXTERNAL_FIELDS_LOAD: Reading new cfc data',
     &                 myTime,myIter
       _END_MASTER(myThid)

       IF ( WindFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( WindFile,wspeed0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( WindFile,wspeed1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( AtmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( AtmospFile,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( AtmospFile,atmosp1,intime1,
     &        myIter,myThid )
       ENDIF 
       IF ( IceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( IceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( IceFile,ice1,intime1,
     &       myIter,myThid )
       ENDIF

C
       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
C
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( WindFile .NE. ' '  ) THEN
             WIND(i,j,bi,bj)    = bWght*wspeed0(i,j,bi,bj)
     &                        +aWght*wspeed1(i,j,bi,bj)
           ELSE
             WIND(i,j,bi,bj) = 5. _d 0*maskC(i,j,1,bi,bj)
           ENDIF
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c following Wannikof (1992)
           pisvel(i,j,bi,bj)=(0.31 _d 0*wind(i,j,bi,bj)**2)/3.6 _d 5
           IF ( AtmospFile .NE. ' '  ) THEN
             ATMOSP(i,j,bi,bj)    = bWght*atmosp0(i,j,bi,bj)
     &                        +aWght*atmosp1(i,j,bi,bj)
           ELSE
             ATMOSP(i,j,bi,bj) = maskC(i,j,1,bi,bj)
           ENDIF
           IF ( IceFile .NE. ' '  ) THEN
            FIce(i,j,bi,bj)    = bWght*ice0(i,j,bi,bj)
     &                          +aWght*ice1(i,j,bi,bj)
           ELSE
            FIce(i,j,bi,bj) =0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C endif for periodicForcing
       ENDIF

#endif
      RETURN
      END
