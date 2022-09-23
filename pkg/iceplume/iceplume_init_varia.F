
#include "ICEPLUME_OPTIONS.h"

CBOP
      SUBROUTINE ICEPLUME_INIT_VARIA( myThid )
C     *============================================================*
C     | SUBROUTINE ICEPLUME_INIT_VARIA                             |
C     | o Routine to initialize ICEPLUME variables.                |
C     *============================================================*
C     | Initialize ICEPLUME parameters and variables.              |
C     *============================================================*
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "ICEPLUME.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#endif /* ALLOW_PTRACERS */

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_VARIA
      INTEGER myThid
      INTEGER myIter
      INTEGER fp, iRec0
      INTEGER iTracer
      _RL aWght, bWght
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL     myTime
CEndOfInterface

#ifdef ALLOW_ICEPLUME
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER I, J, K, bi, bj
CEOP
      fp = readBinaryPrec
      iRec0 = 1

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO J = 1, sNy
          DO I = 1, sNx
           icefront_TendT (i,j,k,bi,bj) = 0. _d 0
           icefront_TendS (i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO k = 1,Nr
       sProf  (K) = 0. _d 0
       tProf  (K) = 0. _d 0
       ptProf (K) = 0. _d 0
       prProf (K) = 0. _d 0
       uProf  (K) = 0. _d 0
       vProf  (K) = 0. _d 0
       mProf  (K) = 0. _d 0
       mProfAv (K) =0. _d 0
       rProfPlume (K) = 0. _d 0
       wProfPlume (K) = 0. _d 0
       tProfPlume (K) = 0. _d 0
       sProfPlume (K) = 0. _d 0
       uProfPlume (K) = 0. _d 0
       mProfPlume (K) = 0. _d 0
       mIntProfPlume (K) = 0. _d 0
       volFlux     (K) = 0. _d 0
       volFluxDiff (K) = 0. _d 0
       HeatFlux(K) = 0. _d 0
       FwFlux  (K) = 0. _d 0
       delta_z     (K) = 0. _d 0
      ENDDO
catn: some variables are defined down to Nr+1
      DO k = Nr+1,Nr+1
       rProfPlume (K) = 0. _d 0
       wProfPlume (K) = 0. _d 0
       tProfPlume (K) = 0. _d 0
       sProfPlume (K) = 0. _d 0
       aProfPlume (K) = 0. _d 0
       mIntProfPlume (K) = 0. _d 0
       volFlux     (K) = 0. _d 0
      ENDDO

      DO bj = 1,nSy
      DO bi = 1,nSx
       DO K = 1,Nr
        DO J = 1-Oly,sNy+Oly
         DO I = 1-OLx,sNx+OLx
          HeatFlux3D  (I,J,K,bi,bj) = 0. _d 0
          FwFlux3D    (I,J,K,bi,bj) = 0. _d 0
          rProfPlume3D (I,J,K,bi,bj) = 0. _d 0
          wProfPlume3D (I,J,K,bi,bj) = 0. _d 0
          tProfPlume3D (I,J,K,bi,bj) = 0. _d 0
          sProfPlume3D (I,J,K,bi,bj) = 0. _d 0
          mProfPlume3D (I,J,K,bi,bj) = 0. _d 0
          mProfAv3D    (I,J,K,bi,bj) = 0. _d 0
          temp_addMass3D (I,J,K,bi,bj) = 0. _d 0
          salt_addMass3D (I,J,K,bi,bj) = 0. _d 0
          addMass3Dbg    (I,J,K,bi,bj) = 0. _d 0
          addMass3Dplume (I,J,K,bi,bj) = 0. _d 0
          volFluxDiff3D (I,J,K,bi,bj) = 0. _d 0
#ifdef ICEPLUME_ALLOW_DETACHED_PLUME 
           thetaProfPlume3D(i,j,k,bi,bj)=0. _d 0
           distanceProfPlume3D(i,j,k,bi,bj)=0. _d 0
#endif /* ICEPLUME_ALLOW_DETACHED_PLUME */
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDDO

       DO K = 1,Nr
        DO J = 1,sNy
         DO I = 1,sNx
          rProfPlume3DLocal (I,J,K) = 0. _d 0
          wProfPlume3DLocal (I,J,K) = 0. _d 0
          tProfPlume3DLocal (I,J,K) = 0. _d 0
          sProfPlume3DLocal (I,J,K) = 0. _d 0
          mProfPlume3DLocal (I,J,K) = 0. _d 0
          mProfAv3DLocal    (I,J,K) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO

       DO I = 1,100
        runoffLocation(I,1) = 0. _d 0
        runoffLocation(I,2) = 0. _d 0
        plumeInputLayer(I)  = 0. _d 0
        Qin(I)              = 0. _d 0
       ENDDO

C Clear addMass from previous timestep
catn: Cannot reset the global field addMass inside a pkg
catn: because there are other processes that also contrib to addMass.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO K = 1,Nr
         DO J = 1-OLy,sNy+OLy
          DO I = 1-OLx,sNx+OLx
            temp_AddMass3D(I,J,K,bi,bj) = theta(I,J,K,bi,bj)
            salt_AddMass3D(I,J,K,bi,bj) = salt(I,J,K,bi,bj)
#ifdef ALLOW_PTRACERS
            DO iTracer = 1,PTRACERS_num
             ptr_Addmass3D(I,J,K,bi,bj,iTracer)
     &                 = pTracer(I,J,K,bi,bj,iTracer)
            ENDDO
#endif /* ALLOW_PTRACERS */

catn: Cannot initialize the global addMass field here just in case there 
catn: are other processes that also adds mass. So we must define independent
catn: addMass* specific to the process of plume or background melt.
            addMass(I,j,k,bi,bj) = 0.D0

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


#endif /* ALLOW_ICEPLUME */

      RETURN
      END
