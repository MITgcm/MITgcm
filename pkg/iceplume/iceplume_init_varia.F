
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

catn prior to being set in iceplume_calc we reset here
catn 19sep2022: even though these are set inside i,j loops in
catn iceplume_calc.F, here we initialize to highlight the fact
catn that they are time-varying and should not be read in from 
catn namelists or initialized in iceplume_readparms.F
      dLtangential = 0. _d 0
      dLnormal     = 0. _d 0
      Q_sg = 0. _d 0
      r_sg = 0. _d 0
      L_sg = 0. _d 0
      w_sg = 0. _d 0

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO K = 1, Nr
         DO J = 1-OLy, sNy+OLy
          DO I = 1-OLx,sNx+OLx
           iceplumeBG_TendT (i,j,k,bi,bj) = 0. _d 0
           iceplumeBG_TendS (i,j,k,bi,bj) = 0. _d 0
           temp_addMass3Dplume (I,J,K,bi,bj) = 0. _d 0
           salt_addMass3Dplume (I,J,K,bi,bj) = 0. _d 0
catn: move below now to local inside iceplume_calc
c          addMass3Dbg    (I,J,K,bi,bj) = 0. _d 0
           addMass3Dplume (I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

catn: I think all these are local variables for iceplume_calc and
catn: should be initialized in there. A quick check in there shows 
catn  that it's being set inside i,j loops.  But perhaps it's safe
catn  to just initialize once in case various checks inside still
catn  yield a loop without initialization, which can cause problem
catn  for adjoint if implemented later.
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
catn: should we be doing this? because if we're not careful on how to 
catn: get the correct plume column then temp_AddMass3D takes theta
catn: which makes gT zero even if we have massive entrainment of plume
catn: (which supposed to have its own Tprofile) and the surrounding
catn            temp_AddMass3D(I,J,K,bi,bj) = theta(I,J,K,bi,bj)
catn            salt_AddMass3D(I,J,K,bi,bj) = salt(I,J,K,bi,bj)
#ifdef ALLOW_PTRACERS
            DO iTracer = 1,PTRACERS_num
             ptr_Addmass3D(I,J,K,bi,bj,iTracer)
     &                 = pTracer(I,J,K,bi,bj,iTracer)
            ENDDO
#endif /* ALLOW_PTRACERS */

catn: Cannot initialize the global addMass field here just in case there 
catn: are other processes that also adds mass. So we must define independent
catn: addMass* specific to the process of plume or background melt.
catn: After introduce process-specific, note they were already reinit above.
catn            addMass3Dplume(I,j,k,bi,bj) = 0.D0
catn            addMass3Dbg(I,j,k,bi,bj) = 0.D0

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


#endif /* ALLOW_ICEPLUME */

      RETURN
      END
