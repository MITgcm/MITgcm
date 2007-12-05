C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_fields_load.F,v 1.21 2007/12/05 16:52:32 dfer Exp $
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
c#include "PTRACERS_SIZE.h"
c#include "PTRACERS_PARAMS.h"
c#include "PTRACERS_FIELDS.h"
#include "GCHEM.h"
#include "DIC_ABIOTIC.h"
#include "DIC_BIOTIC.h"
#include "DIC_LOAD.h"
#ifdef ALLOW_THSICE
#include "THSICE_VARS.h"
#endif
#ifdef ALLOW_SEAICE
#include "SEAICE.h"
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
      IF (  gchem_ForcingCycle.gt.0. _d 0 ) THEN


C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(gchem_ForcingCycle/gchem_ForcingPeriod)
cswd QQ change for placement of chem forcing (ie. after timestep)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(gchem_ForcingPeriod*rdt)
      Ifcyc = NINT(gchem_ForcingCycle*rdt)
      Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

      intime0 = 1 + INT(Iftm/Ifprd)
      intime1 = 1 + MOD(intime0,nForcingPeriods)
c     aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
      aWght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
      bWght = FLOAT( Ifprd )
      aWght =  aWght / bWght
      bWght = 1. _d 0 - aWght

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
         IF ( IceFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             FIce(i,j,bi,bj) = bWght*ice0(i,j,bi,bj)
     &                       + aWght*ice1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

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

C endif for gchem_ForcingCycle
      ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
#ifdef ALLOW_THSICE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             FIce(i,j,bi,bj) = iceMask(i,j,bi,bj)
            ENDDO
           ENDDO
#elif ALLOW_SEAICE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             FIce(i,j,bi,bj) = AREA(i,j,1,bi,bj)
            ENDDO
           ENDDO
#endif
         ENDDO
        ENDDO


#endif /* ALLOW_PTRACERS */
      RETURN
      END
