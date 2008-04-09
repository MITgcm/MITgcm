C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_fields_load.F,v 1.25 2008/04/09 18:54:50 jmc Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

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
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DIC_VARS.h"
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
      IF (  DIC_forcingCycle.gt.0. _d 0 ) THEN


C Now calculate whether it is time to update the forcing arrays
      rdt = 1. _d 0 / deltaTclock
      nForcingPeriods = NINT(DIC_forcingCycle/DIC_forcingPeriod)
cswd QQ change for placement of chem forcing (ie. after timestep)
      Imytm = NINT(myTime*rdt)
      Ifprd = NINT(DIC_forcingPeriod*rdt)
      Ifcyc = NINT(DIC_forcingCycle*rdt)
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


       IF ( DIC_windFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_windFile,wspeed0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_windFile,wspeed1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( DIC_atmospFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_atmospFile,atmosp0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_atmospFile,atmosp1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( DIC_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_silicaFile,silica0,intime0,
     &        myIter,myThid )
         CALL READ_REC_XY_RS( DIC_silicaFile,silica1,intime1,
     &        myIter,myThid )
       ENDIF
       IF ( DIC_iceFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_iceFile,ice0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_iceFile,ice1,intime1,
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
       IF ( DIC_ironFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( DIC_ironFile,feinput0,intime0,
     &       myIter,myThid )
         CALL READ_REC_XY_RS( DIC_ironFile,feinput1,intime1,
     &       myIter,myThid )
       ENDIF
#endif

       ENDIF

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         IF ( DIC_windFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             WIND(i,j,bi,bj) = bWght*wspeed0(i,j,bi,bj)
     &                       + aWght*wspeed1(i,j,bi,bj)
            ENDDO
           ENDDO
C calculate piston velocity
C QQ: note - we should have wind speed variance in here
C QQ         also need to check units, and conversion factors
c          pisvel(i,j,bi,bj)  =0.337*wind(i,j,bi,bj)**2/3.6d5    !QQQQ
         ENDIF
#ifndef USE_PLOAD
         IF ( DIC_atmospFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             AtmosP(i,j,bi,bj) = bWght*atmosp0(i,j,bi,bj)
     &                         + aWght*atmosp1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif
         IF ( DIC_silicaFile .NE. ' '  ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             SILICA(i,j,bi,bj) = bWght*silica0(i,j,bi,bj)
     &                         + aWght*silica1(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( DIC_iceFile .NE. ' '  ) THEN
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
         IF ( DIC_ironFile .NE. ' '  ) THEN
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

C endif for DIC_forcingCycle
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
