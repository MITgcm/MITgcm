C $Header: /u/gcmpack/MITgcm/pkg/bling/bling_carbonate_init.F,v 1.2 2016/09/12 20:00:28 mmazloff Exp $
C $Name:  $

#include "BLING_OPTIONS.h"
#include "PTRACERS_OPTIONS.h"

CBOP
      subroutine BLING_CARBONATE_INIT( myThid )

C     ==========================================================
C     | subroutine bling_carbonate_init
C     | o Calculate first guess of pH
C     |   Adapted from pkg/dic/dic_surfforcing_init.F
C     ==========================================================

      implicit none
      
C     === Global variables ===
#include "SIZE.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "BLING_VARS.h"
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "BLING_LOAD.h"

C     === Routine arguments ===
C     myThid               :: thread Id. number
      INTEGER  myThid

#ifdef ALLOW_BLING
C     === Local variables ===
       INTEGER i,j, k, it
       INTEGER intimeP, intime0, intime1
       _RL aWght, bWght, co3dummy
C Number of iterations for pCO2 solvers...
C Solubility relation coefficients
C local variables for carbon chem
      INTEGER iMin,iMax,jMin,jMax, bi, bj
      _RL alktmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL phostmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL sitmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL thetatmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL salttmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dictmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iprt,jprt
      LOGICAL pH_isLoaded
CEOP

      IF ( periodicExternalForcing ) THEN

c read in silica field
         CALL LEF_ZERO( silica0,myThid )
         CALL LEF_ZERO( silica1,myThid )

C--   Now calculate whether it is time to update the forcing arrays
       CALL GET_PERIODIC_INTERVAL(
     O                   intimeP, intime0, intime1, bWght, aWght,
     I                   externForcingCycle, externForcingPeriod,
     I                   deltaTclock, startTime, myThid )

       _BARRIER
       _BEGIN_MASTER(myThid)
        WRITE(standardMessageUnit,'(A,I10,A,2(2I5,A))')
     &   ' BLING_SURFFORCING_INIT, it=', nIter0,
     &   ' : Reading new data, i0,i1=', intime0, intime1
       _END_MASTER(myThid)

       IF ( BLING_silicaFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS( BLING_silicaFile,silica0,intime0,
     &        nIter0,myThid )
         CALL READ_REC_XY_RS( BLING_silicaFile,silica1,intime1,
     &        nIter0,myThid )
       ENDIF

#ifdef ALLOW_OFFLINE
       IF ( useOffLine ) THEN
         CALL OFFLINE_FIELDS_LOAD( startTime, nIter0, myThid )
       ENDIF
#endif

       _EXCH_XY_RS(silica0, myThid )
       _EXCH_XY_RS(silica1, myThid )

       IF ( BLING_silicaFile .NE. ' '  ) THEN
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

      DO k=1,Nr
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-Olx,sNx+OLx
            pH(i,j,k,bi,bj) = 8. _d 0
          ENDDO
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
          fugf(i,j,bi,bj)=0. _d 0
          ff(i,j,bi,bj)=0. _d 0
          ft(i,j,bi,bj)=0. _d 0
          st(i,j,bi,bj)=0. _d 0
          bt(i,j,bi,bj)=0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      pH_isLoaded = .FALSE.
      IF ( nIter0.GT.PTRACERS_Iter0 .OR.
     &    (nIter0.EQ.PTRACERS_Iter0 .AND. pickupSuff.NE.' ')
     &   ) THEN
C       Read pH from a pickup file if needed
        CALL BLING_READ_PICKUP(
     O                        pH_isLoaded,
     I                        nIter0, myThid )
      ENDIF

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C determine inorganic carbon chem coefficients

        IF ( .NOT.pH_isLoaded ) THEN
C set guess of pH for first step here

        DO k=1,Nr
        DO j=jMin,jMax
         DO i=iMin,iMax
             alktmp(i,j) = PTRACER(i,j,k,bi,bj,2)
     &                          * maskC(i,j,k,bi,bj)
             phostmp(i,j)= PTRACER(i,j,k,bi,bj,5)
     &                          * maskC(i,j,k,bi,bj)
C FOR NON-INTERACTIVE Si
             IF ( k.eq.1 ) THEN
             sitmp(i,j) =  Silica(i,j,bi,bj) * maskC(i,j,k,bi,bj)
             ELSE
             sitmp(i,j) =  0.03 * maskC(i,j,k,bi,bj)
             ENDIF
             dictmp(i,j) = PTRACER(i,j,k,bi,bj,1)
     &                          * maskC(i,j,k,bi,bj)
             thetatmp(i,j) = theta(i,j,k,bi,bj)
             salttmp(i,j) = salt(i,j,k,bi,bj)
         ENDDO
        ENDDO

        CALL CARBON_COEFFS_PRESSURE_DEP(
     I                       thetatmp,salttmp,
     I                       bi,bj,iMin,iMax,jMin,jMax,k,myThid)

C====================================================================

c first approximation

          DO j=jMin,jMax
           DO i=iMin,iMax
            IF ( maskC(i,j,k,bi,bj) .NE. 0. _d 0) THEN
             DO it=1,10
              CALL CALC_PCO2_APPROX(
     I        thetatmp(i,j),salttmp(i,j),
     I        dictmp(i,j), phostmp(i,j),
     I        sitmp(i,j),alktmp(i,j),
     I        ak1(i,j,bi,bj),ak2(i,j,bi,bj),
     I        ak1p(i,j,bi,bj),ak2p(i,j,bi,bj),ak3p(i,j,bi,bj),
     I        aks(i,j,bi,bj),akb(i,j,bi,bj),akw(i,j,bi,bj),
     I        aksi(i,j,bi,bj),akf(i,j,bi,bj),
     I        ak0(i,j,bi,bj), fugf(i,j,bi,bj),
     I        ff(i,j,bi,bj),
     I        bt(i,j,bi,bj),st(i,j,bi,bj),ft(i,j,bi,bj),
     U        pH(i,j,k,bi,bj),pCO2(i,j,bi,bj),co3dummy,
     I        i,j,k,bi,bj, it ,  myThid )
             ENDDO
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDIF

C     end bi,bj loops
       ENDDO
      ENDDO

#endif /* ALLOW_BLING */

      RETURN
      END
