#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: DIAGS_SOUND_SPEED
C     !INTERFACE:
      SUBROUTINE DIAGS_SOUND_SPEED(
     I                       myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R DIAGS_SOUND_SPEED
C     | o Diagnose speed of sound in seawater
C     |   from the algorithm by Del Grasso (1974).
C     |   This is NOT the sound-speed that can be derived from
C     |   the equation of state (EOS). It is independent of
C     |   the model setup specific EOS.
C     |
C     | o Reference:
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
C     !FUNCTIONS:
      _RL SW_TEMP
      EXTERNAL SW_TEMP
#ifdef ALLOW_DIAGNOSTICS
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !LOCAL VARIABLES:
C     == Local variables ==
      INTEGER bi,bj
      INTEGER i,j,k
      _RL c0,ct,cs,cp,cstp
      _RL pres,sal,temp
      LOGICAL calc_soundSpeed
CEOP

      calc_soundSpeed = .FALSE.

C--   switch on this flag if Sound-Speed needed in any cost fct
c#ifdef ALLOW_CSOUND_COST
c     calc_soundSpeed = .TRUE.
c#endif

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        calc_soundSpeed = calc_soundSpeed
     &               .OR. DIAGNOSTICS_IS_ON( 'CSound  ', myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      IF ( calc_soundSpeed ) THEN
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
            cSound(i,j,k,bi,bj) = 0.0 _d 0
           ENDDO
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1,sNy
           DO i=1,sNx
            IF ( maskC(i,j,k,bi,bj).EQ.oneRS ) THEN
C pressure in dbar (for SW_TEMP)
             pres = rhoConst*( totPhiHyd(i,j,k,bi,bj)
     &               - rC(k)*gravity )*SItodBar
             temp = SW_TEMP( SALT(i,j,k,bi,bj),
     &                 THETA(i,j,k,bi,bj), pres, zeroRL )
             sal  = SALT(i,j,k,bi,bj)
C convert pressure to kg/cm^2 for Del Grasso algorithm
             pres = pres/gravity
             ct   = ( 5.01109398873 _d 0 - ( 0.550946843172 _d -1
     &              - 0.221535969240 _d -3 * temp ) * temp ) * temp
             cs   = ( 1.32952290781 _d 0 + 0.128955756844 _d -3 * sal
     &              ) * sal
             cp   = ( 0.156059257041 _d 0 + ( 0.244998688441 _d -4
     &              - 0.883392332513 _d -8 * pres ) * pres ) * pres
             cstp = ( - 0.127562783426 _d -1
     &                + 0.968403156410 _d -4 *temp ) * temp * sal
     &            + ((  0.635191613389 _d -2
     &                +(0.265484716608 _d -7 *temp
     &                - 0.159349479045 _d -5
     &                + 0.522116437235 _d -9 *pres ) * pres
     &                - 0.438031096213 _d -6 * temp * temp
     &                +(0.485639620015 _d -5 * sal
     &                - 0.340597039004 _d -3 ) *sal ) * temp
     &                - 0.161674495909 _d -8 * sal * sal * pres
     &              ) * pres
             cSound(i,j,k,bi,bj) = c0+ct+cs+cp+cstp
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(cSound,'CSound  ',0,Nr,0,1,1,myThid)
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

C-    end-if calc_soundSpeed
      ENDIF

#endif /* INCLUDE_SOUNDSPEED_CALC_CODE */

      RETURN
      END
