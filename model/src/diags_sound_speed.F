#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_SOUND_SPEED
C     !INTERFACE:
      SUBROUTINE DIAGS_SOUND_SPEED(
     I                       myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_SOUND_SPEED
C     | o Diagnose speed of sound in seawater 
C     | o from the algorithm by Del Grasso (1974).
C     | o This is NOT the sound-speed that can be derived from  
C     | o the equation of state (EOS). It is independent of 
C     | o the model setup specific EOS.
C     | 
C     | reference:
C     | V. A. Del Grosso, "New equation for the speed of sound in  
C     | natural waters (with comparisons to other equations),"
C     | J. Acoust. Soc. Am. 56, 1084-1091 (1974). 
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "EOS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef INCLUDE_SOUNDSPEED_CALC_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      _RL c0,ct,cs,cp,cstp
      _RL pres,sal,temp
      _RL SW_TEMP

C     == External functions ==
      EXTERNAL SW_TEMP
CEOP

      c0   = 1402.392 _d 0
      ct   = 0. _d 0
      cs   = 0. _d 0
      cp   = 0. _d 0
      cstp = 0. _d 0
      pres = 0. _d 0
      sal  = 0. _d 0
      temp = 0. _d 0
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           csound(i,j,k,bi,bj) = 0.0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1,sNy
          DO i=1,sNx
           IF (hFacC(i,j,k,bi,bj).gt.0.0) THEN
C pressure in dbar (for SW_TEMP)
            pres = rhoConst*( totPhiHyd(i,j,k,bi,bj)
     &               - rC(k)*gravity )*SItodBar
            temp = SW_TEMP(SALT(i,j,k,bi,bj),
     &               THETA(i,j,k,bi,bj),pres,0. _d 0)
            sal = SALT(i,j,k,bi,bj)
C convert pressure to kg/cm^2 for Del Grasso algorithm
            pres = pres/gravity
            ct = (5.01109398873 - (0.550946843172 _d -1
     &               - 0.221535969240 _d -3*temp)*temp)*temp
            cs = (1.32952290781 + 0.128955756844 _d -3*sal)*sal
            cp = (0.156059257041 + (0.244998688441 _d -4
     &               - 0.883392332513 _d -8*pres)*pres)*pres
            cstp = -0.127562783426 _d -1*temp*sal
     &               + 0.635191613389 _d -2*temp*pres
     &               + 0.265484716608 _d -7*temp*temp*pres*pres
     &               - 0.159349479045 _d -5*temp*pres*pres
     &               + 0.522116437235 _d -9*temp*pres*pres*pres
     &               - 0.438031096213 _d -6*temp*temp*temp*pres
     &               - 0.161674495909 _d -8*sal*sal*pres*pres
     &               + 0.968403156410 _d -4*temp*temp*sal
     &               + 0.485639620015 _d -5*temp*sal*sal*pres
     &               - 0.340597039004 _d -3*temp*sal*pres
            csound(i,j,k,bi,bj) = c0+ct+cs+cp+cstp
           ENDIF
          ENDDO
         ENDDO
        ENDDO
        IF ( useDiagnostics ) THEN
         CALL DIAGNOSTICS_FILL(csound,'CSound  ',0,Nr,1,bi,bj,myThid)
        ENDIF
       ENDDO
      ENDDO

#endif /* INCLUDE_SOUNDSPEED_CALC_CODE */

      RETURN
      END

