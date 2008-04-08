C $Header: /u/gcmpack/MITgcm/pkg/cfc/cfc_fields_load.F,v 1.13 2008/04/08 20:23:05 dfer Exp $
C $Name:  $

#include "GCHEM_OPTIONS.h"

CStartOfInterFace
      SUBROUTINE CFC_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C     /==========================================================\
C     | SUBROUTINE CFC_FIELDS_LOAD                               |
C     |==========================================================|
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "CFC.h"
#ifdef ALLOW_EXF
# include "EXF_FIELDS.h"
#endif
#ifdef ALLOW_SEAICE
# include "SEAICE.h"
#endif

C     == Routine arguments ==
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_PTRACERS
C     == Local variables ==
       INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
c
c
      IF ( cfc_ForcingCycle .GT. 0. _d 0 ) THEN

C First call requires that we initialize everything to zero for safety
cQQQ need to check timing
       IF ( myIter .EQ. nIter0 ) THEN
         CALL LEF_ZERO( wind0,myThid )
         CALL LEF_ZERO( wind1,myThid )
         CALL LEF_ZERO( atmosp0,myThid )
         CALL LEF_ZERO( atmosp1,myThid )
         CALL LEF_ZERO( ice0,myThid )
         CALL LEF_ZERO( ice1,myThid )
       ENDIF


C Now calculate whether it is time to update the forcing arrays
       rdt = 1. _d 0 / deltaTclock
       nForcingPeriods = NINT(cfc_ForcingCycle/cfc_ForcingPeriod)
cswd QQ change for placement of chem forcing (ie. after timestep)
       Imytm = NINT(myTime*rdt)
       Ifprd = NINT(cfc_ForcingPeriod*rdt)
       Ifcyc = NINT(cfc_ForcingCycle*rdt)
       Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

       intime0= 1 + INT(Iftm/Ifprd)
       intime1= 1 + MOD(intime0,nForcingPeriods)
c      aWght=DFLOAT( Iftm-Ifprd*(intime0-1) )/DFLOAT( Ifprd )
       aWght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
       bWght = FLOAT( Ifprd )
       aWght =  aWght / bWght
       bWght = 1. _d 0 - aWght

cswd QQ  need nIter0+1 since chem forcing after time step
       IF (
     &   Iftm-Ifprd*(intime0-1).EQ. 0
     &   .OR. myIter .EQ. nIter0
     &    ) THEN

         _BARRIER

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
        _BEGIN_MASTER(myThid)
        WRITE(*,*)
     &    'S/R CFC_FIELDS_LOAD: Reading new cfc data',
     &                 myTime,myIter
       _END_MASTER(myThid)

      IF ( WindFileCfc .NE. ' '  .AND. .NOT.useEXF ) THEN
         CALL READ_REC_XY_RS( WindFileCfc,wind0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( WindFileCfc,wind1,intime1,
     &        myIter,myThid )
       ENDIF
      IF ( AtmospFileCfc .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( AtmospFileCfc,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( AtmospFileCfc,atmosp1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( IceFileCfc .NE. ' '  .AND. .NOT.useSEAICE ) THEN
         CALL READ_REC_XY_RS( IceFileCfc,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( IceFileCfc,ice1,intime1,
     &       myIter,myThid )
       ENDIF

C
       IF (.NOT.useEXF) THEN
       _EXCH_XY_R4(wind0, myThid )
       _EXCH_XY_R4(wind1, myThid )
       ENDIF
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       IF (.NOT.useSEAICE) THEN
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
       ENDIF
C
       ENDIF

#ifdef ALLOW_EXF
       IF ( useEXF ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     sh = max(wspeed,umin), with default umin=0.5m/s
C           pisvel(i,j,bi,bj)=(0.31 _d 0*wspeed(i,j,bi,bj)**2)/3.6 _d 5
           pisvel(i,j,bi,bj)=(0.31 _d 0*sh(i,j,bi,bj)**2)/3.6 _d 5
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
#else
       IF (.TRUE.) THEN
#endif /* ALLOW_EXF */
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( WindFileCfc .NE. ' '  ) THEN
             WIND(i,j,bi,bj)    = bWght*wind0(i,j,bi,bj)
     &                           +aWght*wind1(i,j,bi,bj)
           ELSE
             WIND(i,j,bi,bj) = 5. _d 0*maskC(i,j,1,bi,bj)
           ENDIF
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c following Wannikof (1992)
           pisvel(i,j,bi,bj)=(0.31 _d 0*wind(i,j,bi,bj)**2)/3.6 _d 5
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDIF
C
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( AtmospFileCfc .NE. ' '  ) THEN
             ATMOSP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                          +aWght*atmosp1(i,j,bi,bj)
           ELSE
             ATMOSP(i,j,bi,bj) = maskC(i,j,1,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_SEAICE
       IF (useSEAICE) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           FIce(I,J,bi,bj) = AREA(I,J,1,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
#else
       IF (.TRUE.) THEN
#endif /* ALLOW_SEAICE */
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF ( IceFileCfc .NE. ' '  ) THEN
            FIce(i,j,bi,bj)    = bWght*ice0(i,j,bi,bj)
     &                          +aWght*ice1(i,j,bi,bj)
           ELSE
            FIce(i,j,bi,bj) =0. _d 0
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       ENDIF

C endif for periodicForcing
       ENDIF

#endif
      RETURN
      END
