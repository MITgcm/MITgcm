C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_fields_load.F,v 1.9 2005/10/13 16:25:12 stephd Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

CBOP
C !ROUTINE: DIC_FIELDS_LOAD

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_FIELDS_LOAD (
     I           myIter,myTime,myThid)

C !DESCRIPTION:
C  Read in fields needed for CO2,O2 fluxterms, silica for pH calculation

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#ifdef DIC_BIOTIC
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
C  myIter               :: current timestep
C  myTime               :: current time
      INTEGER myIter
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_PTRACERS

c !LOCAL VARIABLES: ===================================================
       INTEGER bi,bj,i,j,intime0,intime1
      _RL aWght,bWght,rdt
      INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
CEOP

c
      IF ( periodicExternalForcing ) THEN


C Now calculate whether it is time to update the forcing arrays
       rdt=1. _d 0 / deltaTclock
       nForcingPeriods=
     &  int(externForcingCycle/externForcingPeriod+0.5)
cswd QQ change for placement of chem forcing (ie. after timestep)
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
     &   Iftm-Ifprd*(intime0-1).EQ. 0
     &   .OR. myIter .EQ. nIter0
     &    ) THEN


        _BEGIN_MASTER(myThid)

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
        WRITE(*,*)
     &    'S/R EXTERNAL_FIELDS_LOAD: Reading new dic data',
     &                 myTime,myIter

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
       IF ( SilicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( SilicaFile,silica0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( SilicaFile,silica1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( IceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( IceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( IceFile,ice1,intime1,
     &       myIter,myThid )
       ENDIF
#ifdef READ_PAR
       IF ( Filename1 .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( Filename1,par0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( Filename1,par1,intime1,
     &       myIter,myThid )
       ENDIF
#endif
#ifdef ALLOW_FE
       IF ( IronFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( IronFile,feinput0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( IronFile,feinput1,intime1,
     &       myIter,myThid )
       ENDIF
#endif


       _END_MASTER(myThid)
C
       _EXCH_XY_R4(wspeed0, myThid )
       _EXCH_XY_R4(wspeed1, myThid )
       _EXCH_XY_R4(atmosp0, myThid )
       _EXCH_XY_R4(atmosp1, myThid )
       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )
       _EXCH_XY_R4(ice0, myThid )
       _EXCH_XY_R4(ice1, myThid )
#ifdef READ_PAR
       _EXCH_XY_R4(par0, myThid )
       _EXCH_XY_R4(par1, myThid )
#endif
#ifdef ALLOW_FE
       _EXCH_XY_R4(feinput0, myThid )
       _EXCH_XY_R4(feinput1, myThid )
#endif

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
             WIND(i,j,bi,bj) = 5.d0*maskC(i,j,1,bi,bj)
           ENDIF
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c QQ         also need to check units, and conversion factors
           pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    !QQQQ
           IF ( AtmospFile .NE. ' '  ) THEN
             ATMOSP(i,j,bi,bj)    = bWght*atmosp0(i,j,bi,bj)
     &                        +aWght*atmosp1(i,j,bi,bj)
           ELSE
             ATMOSP(i,j,bi,bj)   =1.d0*maskC(i,j,1,bi,bj)
           ENDIF
           IF ( SilicaFile .NE. ' '  ) THEN
             SILICA(i,j,bi,bj)    = bWght*silica0(i,j,bi,bj)
     &                        +aWght*silica1(i,j,bi,bj)
           ELSE
             SILICA(i,j,bi,bj)   =7.6838e-3*maskC(i,j,1,bi,bj)
           ENDIF
           IF ( IceFile .NE. ' '  ) THEN
            FIce(i,j,bi,bj)    = bWght*ice0(i,j,bi,bj)
     &                          +aWght*ice1(i,j,bi,bj)
           ELSE
            FIce(i,j,bi,bj) =0.d0
           ENDIF
           if (FIce(i,j,bi,bj).lt.1d-2) then
              FIce(i,j,bi,bj) = 0.d0
           endif
#ifdef READ_PAR
           IF ( Filename1 .NE. ' '  ) THEN
            PAR(i,j,bi,bj)    = bWght*par0(i,j,bi,bj)
     &                          +aWght*par1(i,j,bi,bj)
           ELSE
            PAR(i,j,bi,bj) =100.d0
           ENDIF
#endif
#ifdef ALLOW_FE
           IF ( IronFile .NE. ' '  ) THEN
            InputFe(i,j,bi,bj)   = bWght*feinput0(i,j,bi,bj)
     &                          +aWght*feinput1(i,j,bi,bj)
           ELSE
             InputFe(i,j,bi,bj)  =  0.d0*maskC(i,j,1,bi,bj)
           ENDIF
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C endif for periodicForcing
       ENDIF

#endif
      RETURN
      END
