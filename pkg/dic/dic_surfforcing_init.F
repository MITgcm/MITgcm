C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_surfforcing_init.F,v 1.11 2007/10/26 21:08:13 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"
#include "GCHEM_OPTIONS.h"

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
#include "DIC_ABIOTIC.h"
#include "GCHEM.h"
#ifdef DIC_BIOTIC
#include "PTRACERS_SIZE.h"
#include "PTRACERS.h"
#include "DIC_LOAD.h"
#endif

C !INPUT PARAMETERS: ===================================================
C  myThid               :: thread number
      INTEGER  myThid

#ifdef ALLOW_PTRACERS

C !LOCAL VARIABLES: ====================================================
       INTEGER I,J, kLev, it
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
CEOP

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      kLev=1

ccccccccccccccccccccccccccccccccccccccccc
      IF ( periodicExternalForcing ) THEN

c read in silica field
         CALL LEF_ZERO( silica0,myThid )
         CALL LEF_ZERO( silica1,myThid )

       rdt=1. _d 0 / deltaTclock
       nForcingPeriods=
     &  int(externForcingCycle/externForcingPeriod+0.5)
cswd QQ change for placement of chem forcing (ie. after timestep)
       Imytm=int((nIter0*deltaTclock)*rdt+0.5)
       Ifprd=int(externForcingPeriod*rdt+0.5)
       Ifcyc=int(externForcingCycle*rdt+0.5)
       Iftm=mod( Imytm+Ifcyc-Ifprd/2 ,Ifcyc)
       intime0=int(Iftm/Ifprd)
       intime1=mod(intime0+1,nForcingPeriods)
       aWght=float( Iftm-Ifprd*intime0 )/float( Ifprd )
       bWght=1.-aWght

       intime0=intime0+1
       intime1=intime1+1
       
       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(*,*)
     &    'S/R EXTERNAL_FIELDS_LOAD: Reading new dic data',
     &                 intime0, intime1
       _END_MASTER(myThid)

      IF ( SilicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( SilicaFile,silica0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( SilicaFile,silica1,intime1,
     &        nIter0,myThid )
       ENDIF

#ifdef ALLOW_OFFLINE
         otime=nIter0*deltaTclock
         call OFFLINE_FIELDS_LOAD( otime, nIter0, myThid )
#endif

       _EXCH_XY_R4(silica0, myThid )
       _EXCH_XY_R4(silica1, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
          IF ( SilicaFile .NE. ' '  ) THEN
             SILICA(i,j,bi,bj)    = bWght*silica0(i,j,bi,bj)
     &                        +aWght*silica1(i,j,bi,bj)
           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO

c end periodicExternalForcing
      ENDIF

C =================================================================
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

        jMin=1
        jMax=sNy
        iMin=1
        iMax=sNx

C determine inorganic carbon chem coefficients
        DO j=jmin,jmax
         DO i=imin,imax

#ifdef DIC_BIOTIC
cQQQQ check ptracer numbers
             surfalk(i,j) = PTRACER(i,j,klev,bi,bj,2)
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = PTRACER(i,j,klev,bi,bj,3)
     &                          * maskC(i,j,kLev,bi,bj)
#else
             surfalk(i,j) = 2.366595 _d 0 * salt(i,j,kLev,bi,bj)/gsm_s
     &                          * maskC(i,j,kLev,bi,bj)
             surfphos(i,j)  = 5.1225 _d -4 * maskC(i,j,kLev,bi,bj)
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

c set number of iterations for [H+] solvers
C set guess of pH for first step here

        DO j=jmin,jmax
         DO i=imin,imax
                  pH(i,j,bi,bj) = 8. _d 0
         ENDDO
        ENDDO
         print*,'QQ: pCO2 approximation method'
c first approxmation
C$TAF LOOP = parallel
        DO j=jmin,jmax
C$TAF LOOP = parallel
         DO i=imin,imax
          IF ( maskC(i,j,kLev,bi,bj) .NE. 0. _d 0) THEN
C$TAF init dic_surf = static, 10
          do it=1,10
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
          enddo
          ENDIF
         ENDDO
        ENDDO

       ENDDO
      ENDDO
        print*,'QQ first guess pH', pH(20,20,1,1), theta(20,20,1,1,1),
     &         salt(20,20,1,1,1),
     &        PTR_CO2(20,20,1), surfphos(20,20),
     &        surfsi(20,20),surfalk(20,20)
#endif
        RETURN
        END
