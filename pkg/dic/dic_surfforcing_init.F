C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_surfforcing_init.F,v 1.22 2008/04/08 20:21:35 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
C !ROUTINE: DIC_SURFFORCING_INIT

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SURFFORCING_INIT(
     I          myThid)

C !DESCRIPTION:
C  Calculate first guess of pH

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "DIC_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "DIC_LOAD.h"

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER  myThid

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
       INTEGER i,j, kLev, it
       INTEGER intime0,intime1
       _RL otime
       _RL aWght,bWght,rdt
       INTEGER nForcingPeriods,Imytm,Ifprd,Ifcyc,Iftm
C Number of iterations for pCO2 solvers...
C Solubility relation coefficients
C local variables for carbon chem
      _RL  PTR_CO2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)
      INTEGER iMin,iMax,jMin,jMax, bi, bj
      _RL surfalk(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfphos(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL surfsi(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iprt,jprt
      LOGICAL pH_isLoaded
CEOP

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      kLev=1

      CALL DIC_ATMOS(0, startTime, nIter0, myThid )

ccccccccccccccccccccccccccccccccccccccccc
      IF ( periodicExternalForcing ) THEN

c read in silica field
         CALL LEF_ZERO( silica0,myThid )
         CALL LEF_ZERO( silica1,myThid )

       rdt = 1. _d 0 / deltaTclock
       nForcingPeriods = NINT(externForcingCycle/externForcingPeriod)
cswd QQ change for placement of chem forcing (ie. after timestep)
       Imytm = NINT(startTime*rdt)
       Ifprd = NINT(externForcingPeriod*rdt)
       Ifcyc = NINT(externForcingCycle*rdt)
       Iftm  = MOD( Imytm+Ifcyc-Ifprd/2, Ifcyc)

       intime0 = 1 + INT(Iftm/Ifprd)
       intime1 = 1 + MOD(intime0,nForcingPeriods)
c      aWght = DFLOAT( Iftm-Ifprd*(intime0 - 1) ) / DFLOAT( Ifprd )
       aWght = FLOAT( Iftm-Ifprd*(intime0 - 1) )
       bWght = FLOAT( Ifprd )
       aWght =  aWght / bWght
       bWght = 1. _d 0 - aWght

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(*,*)
     &    'S/R EXTERNAL_FIELDS_LOAD: Reading new dic data',
     &                 intime0, intime1
       _END_MASTER(myThid)

      IF ( SilicaFileDic .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( SilicaFileDic,silica0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( SilicaFileDic,silica1,intime1,
     &        nIter0,myThid )
       ENDIF

#ifdef ALLOW_OFFLINE
       IF ( useOffLine ) THEN
         otime=nIter0*deltaTclock
         CALL OFFLINE_FIELDS_LOAD( otime, nIter0, myThid )
       ENDIF
#endif

       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )

       IF ( SilicaFileDic .NE. ' '  ) THEN
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
             SILICA(i,j,bi,bj)= bWght*silica0(i,j,bi,bj)
     &                        + aWght*silica1(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

c end periodicExternalForcing
      ENDIF

C =================================================================

      jMin=1
      jMax=sNy
      iMin=1
      iMax=sNx

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-Olx,sNx+OLx
            pH(i,j,bi,bj) = 8. _d 0
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          ak0(i,j,bi,bj)=0. _d 0
          ak1(i,j,bi,bj)=0. _d 0
          ak2(i,j,bi,bj)=0. _d 0
          akw(i,j,bi,bj)=0. _d 0
          akb(i,j,bi,bj)=0. _d 0
          akf(i,j,bi,bj)=0. _d 0
          ak1p(i,j,bi,bj)=0. _d 0
          ak2p(i,j,bi,bj)=0. _d 0
          ak3p(i,j,bi,bj)=0. _d 0
          aksi(i,j,bi,bj)=0. _d 0
          ff(i,j,bi,bj)=0. _d 0
          ft(i,j,bi,bj)=0. _d 0
          st(i,j,bi,bj)=0. _d 0
          bt(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      pH_isLoaded = .FALSE.
      IF ( nIter0.GT.PTRACERS_Iter0 ) THEN
C       Read pH from a pickup file if needed
        CALL DIC_READ_PICKUP(
     O                        pH_isLoaded,
     I                        nIter0, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C determine inorganic carbon chem coefficients
        DO j=jMin,jMax
         DO i=iMin,iMax

#ifdef DIC_BIOTIC
             surfalk(i,j) = PTRACER(i,j,klev,bi,bj,2)
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)= PTRACER(i,j,klev,bi,bj,3)
     &                          * maskC(i,j,kLev,bi,bj)
#else
             surfalk(i,j) = 2.366595 _d 0 * salt(i,j,kLev,bi,bj)/gsm_s
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)= 5.1225 _d -4 * maskC(i,j,kLev,bi,bj)
#endif
C FOR NON-INTERACTIVE Si
             surfsi(i,j)   =  Silica(i,j,bi,bj) * maskC(i,j,kLev,bi,bj)
             PTR_CO2(i,j,kLev) = PTRACER(i,j,klev,bi,bj,1)
     &                          * maskC(i,j,kLev,bi,bj)
         ENDDO
        ENDDO

        CALL CARBON_COEFFS(
     I                       theta,salt,
     I                       bi,bj,iMin,iMax,jMin,jMax)

C====================================================================

        IF ( .NOT.pH_isLoaded ) THEN
C set guess of pH for first step here

          print*,'QQ: pCO2 approximation method'
c first approximation
C$TAF LOOP = parallel
          DO j=jMin,jMax
C$TAF LOOP = parallel
           DO i=iMin,iMax
            IF ( maskC(i,j,kLev,bi,bj) .NE. 0. _d 0) THEN
C$TAF init dic_surf = static, 10
             DO it=1,10
C$TAF STORE pH(i,j,bi,bj), PTR_CO2(i,j,kLev)           = dic_surf
C$TAF STORE surfalk(i,j), surfphos(i,j), surfsi(i,j)   = dic_surf
              CALL CALC_PCO2_APPROX(
     I        theta(i,j,kLev,bi,bj),salt(i,j,kLev,bi,bj),
     I        PTR_CO2(i,j,kLev), surfphos(i,j),
     I        surfsi(i,j),surfalk(i,j),
     I        ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I        ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I        aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I        aksi(i,j,bi,bj),akf(i,j,bi,bj),ff(i,j,bi,bj),
     I        bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U        pH(i,j,bi,bj),pCO2(i,j,bi,bj) )
             ENDDO
            ENDIF
           ENDDO
          ENDDO
          iprt = MIN(20,sNx)
          jprt = MIN(20,sNy)
          print*,'QQ first guess pH', pH(iprt,jprt,bi,bj),
     &        theta(iprt,jprt,1,bi,bj), salt(iprt,jprt,1,bi,bj),
     &        PTR_CO2(iprt,jprt,1), surfphos(iprt,jprt),
     &        surfsi(iprt,jprt),surfalk(iprt,jprt)

        ENDIF

C     end bi,bj loops
       ENDDO
      ENDDO

#endif
      RETURN
      END
