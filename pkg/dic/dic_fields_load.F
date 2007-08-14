C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_fields_load.F,v 1.16 2007/08/14 19:32:40 dfer Exp $
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
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
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
      CHARACTER*(MAX_LEN_MBUF) msgBuf
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


        _BARRIER

C      If the above condition is met then we need to read in
C      data for the period ahead and the period behind myTime.
        _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,2I5,I10,1P1E20.12)')
     &  'S/R DIC_FIELDS_LOAD: Reading new dic data:',
     &  intime0, intime1, myIter, myTime
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

c       _EXCH_XY_R4(wspeed0, myThid )
c       _EXCH_XY_R4(wspeed1, myThid )
c       _EXCH_XY_R4(atmosp0, myThid )
c       _EXCH_XY_R4(atmosp1, myThid )
c       _EXCH_XY_R4(silica0, myThid )
c       _EXCH_XY_R4(silica1, myThid )
c       _EXCH_XY_R4(ice0, myThid )
c       _EXCH_XY_R4(ice1, myThid )
c#ifdef READ_PAR
c       _EXCH_XY_R4(par0, myThid )
c       _EXCH_XY_R4(par1, myThid )
c#endif
c#ifdef ALLOW_FE
c       _EXCH_XY_R4(feinput0, myThid )
c       _EXCH_XY_R4(feinput1, myThid )
c#endif

C
       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( WindFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             WIND(i,j,bi,bj) = bWght*wspeed0(i,j,bi,bj)
     &                       + aWght*wspeed1(i,j,bi,bj)
            ENDDO
           ENDDO
c calculate piston velocity
c QQ: note - we should have wind speed variance in here
c QQ         also need to check units, and conversion factors
c          pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    !QQQQ
         ENDIF
#ifndef USE_PLOAD
         IF ( AtmospFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             AtmosP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                         + aWght*atmosp1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
         IF ( SilicaFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             SILICA(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                         + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#ifdef ALLOW_THSICE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             FIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
            ENDDO
           ENDDO
#else
         IF ( IceFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             FIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
#ifdef READ_PAR
         IF ( Filename1 .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             PAR(i,j,bi,bj) = bWght*par0(i,j,bi,bj)
     &                      + aWght*par1(i,j,bi,bj)
            ENDDO
           ENDDO
         ELSE
            WRITE(msgBuf,'(2A)')
     &       ' DIC_FIELDS_LOAD: You need to provide ',
     &       ' a file if you want to use READ_PAR'
            CALL PRINT_ERROR( msgBuf, myThid )
            STOP 'ABNORMAL END: S/R DIC_FIELDS_LOAD'
         ENDIF
#endif
#ifdef ALLOW_FE
         IF ( IronFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             InputFe(i,j,bi,bj) = bWght*feinput0(i,j,bi,bj)
     &                          + aWght*feinput1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
        ENDDO
       ENDDO

C endif for periodicForcing
      ENDIF

#endif /* ALLOW_PTRACERS */
      RETURN
      END
