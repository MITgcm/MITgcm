
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
#include "ICEPLUME_PARAMS.h"
#include "ICEPLUME_FIELDS.h"
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_PARAMS.h"
#include "PTRACERS_FIELDS.h"
#include "ICEPLUME_PTRACERS_FIELDS.h"
#endif /* ALLOW_PTRACERS */

C     === Routine arguments ===
C     myThid -  Number of this instance of ICEFRONT_INIT_VARIA
      INTEGER myThid
      INTEGER fp, iRec0
#ifdef ALLOW_PTRACERS
      INTEGER iTracer
#endif /* ALLOW_PTRACERS */
CEndOfInterface

#ifdef ALLOW_ICEPLUME
C     === Local variables ===
C     I,J,K,bi,bj - Loop counters
      INTEGER i, j, k, bi, bj
CEOP
      fp = readBinaryPrec
      iRec0 = 1

C Prior to being set in iceplume_calc we reset here
C 19sep2022: Even though these are set inside i,j loops in
C iceplume_calc.F, here we initialize to highlight the fact
C that they are time-varying and should not be read in from
C namelists or initialized in iceplume_readparms.F
      dLtangential = 0. _d 0
      dLnormal     = 0. _d 0
      Q_sg = 0. _d 0
      r_sg = 0. _d 0
      L_sg = 0. _d 0
      w_sg = 0. _d 0

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1, Nr
         DO j = 1-OLy, sNy+OLy
          DO i = 1-OLx,sNx+OLx
           iceplumeBG_TendT (i,j,k,bi,bj) = 0. _d 0
           iceplumeBG_TendS (i,j,k,bi,bj) = 0. _d 0
           addMass3Dbg    (i,j,k,bi,bj) = 0. _d 0
           addMass3Dplume (i,j,k,bi,bj) = 0. _d 0
           HeatFlux3Dbg (i,j,k,bi,bj) = 0. _d 0
           SaltFlux3Dbg (i,j,k,bi,bj) = 0. _d 0
           HeatFlux3Dpl (I,J,K,bi,bj) = 0. _d 0
           SaltFlux3Dpl (I,J,K,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C atn thinks all these are local variables for iceplume_calc and
C should be initialized in there. A quick check in there shows
C that they are being set inside i,j loops.  But perhaps it's safe
C to just initialize once in case various checks inside still
C yield a loop without initialization, which can cause problem
C for adjoint if implemented later.
      DO k = 1,Nr
       sProf  (k) = 0. _d 0
       tProf  (k) = 0. _d 0
       ptProf (k) = 0. _d 0
       prProf (k) = 0. _d 0
       uProf  (k) = 0. _d 0
       vProf  (k) = 0. _d 0
       mProf  (k) = 0. _d 0
       mProfAv (k) =0. _d 0
       rProfPlume (k) = 0. _d 0
       wProfPlume (k) = 0. _d 0
       tProfPlume (k) = 0. _d 0
       sProfPlume (k) = 0. _d 0
       uProfPlume (k) = 0. _d 0
       mProfPlume (k) = 0. _d 0
       mIntProfPlume (k) = 0. _d 0
       volFlux     (k) = 0. _d 0
       volFluxDiff (k) = 0. _d 0
c       HeatFlux1dbg(k) = 0. _d 0
c       FwFlux1dbg  (k) = 0. _d 0
c       delta_z     (k) = 0. _d 0
      ENDDO
C Some variables are defined down to Nr+1
      DO k = Nr+1,Nr+1
       rProfPlume (k) = 0. _d 0
       wProfPlume (k) = 0. _d 0
       tProfPlume (k) = 0. _d 0
       sProfPlume (k) = 0. _d 0
       aProfPlume (k) = 0. _d 0
       mIntProfPlume (k) = 0. _d 0
       volFlux     (k) = 0. _d 0
      ENDDO

C Clear addMass from previous timestep
C Cannot reset the global field addMass inside a pkg
C because there are other processes that also contrib to addMass.

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k = 1,Nr
         DO j = 1-OLy,sNy+OLy
          DO i = 1-OLx,sNx+OLx
C Should we be doing this? because if we are not careful on how to
C get the correct plume column then temp_AddMass3D takes theta
C which makes gT zero even if we have massive entrainment of plume
C (which supposed to have its own Tprofile) and the surrounding
c           temp_AddMass3D(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
c           salt_AddMass3D(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
#ifdef ALLOW_PTRACERS
            DO iTracer = 1,PTRACERS_num
             ptr_Addmass3D(i,j,k,bi,bj,iTracer)
     &                 = pTracer(i,j,k,bi,bj,iTracer)
            ENDDO
#endif /* ALLOW_PTRACERS */

C Cannot initialize the global addMass field here just in case there
C are other processes that also adds mass. So we must define independent
C addMass* specific to the process of plume or background melt.
C After introduce process-specific, note they were already reinit above.
c            addMass3Dplume(i,j,k,bi,bj) = 0.D0
c            addMass3Dbg(i,j,k,bi,bj) = 0.D0

          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO


#endif /* ALLOW_ICEPLUME */

      RETURN
      END
