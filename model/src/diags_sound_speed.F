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


C original aog_vsdg script
C function [cs]=aog_vsdg( pr, tem, sal)
C % pr, dbar
C % tem, IN DEGREES CELSIUS
C % sal IN PARTS PER THOUSAND.
C % SOUND SPEED IN METERS/SECOND
C   pr=pr/9.80665; %convert pressure to kg/cm**2;
C   c0=1402.392;
C   vt=(0.501109398873e01-(0.550946843172e-01...
C       - 0.221535969240e-03*tem).*tem).*tem;
C   vs=(0.132952290781e01 + 0.128955756844e-03*sal).*sal;
C   vp=(0.156059257041e00 + (0.244998688441e-04...
C       - 0.883392332513e-08*pr).*pr).*pr;
C   vstp=-0.127562783426e-01*tem.*sal + 0.635191613389e-02*tem.*pr...
C        +0.265484716608e-07*tem.*tem.*pr.*pr...
C        - 0.159349479045e-05*tem.*pr.*pr...
C        +0.522116437235e-09*tem.*pr.*pr.*pr...
C        - 0.438031096213e-06*tem.*tem.*tem.*pr...
C        -0.161674495909e-08*sal.*sal.*pr.*pr...
C        + 0.968403156410e-04*tem.*tem.*sal...
C        +0.485639620015e-05*tem.*sal.*sal.*pr...
C        - 0.340597039004e-03*tem.*sal.*pr;
C   cs=c0+vt+vs+vp+vstp;
C return




C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "obsfit.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myThid     :: my Thread Id number
      INTEGER myThid

#ifdef INCLUDE_SOUNDSPEED_CALC_CODE

C     !LOCAL VARIABLES:
C     == Local variables ==
      integer bi,bj
      integer i,j,k
      integer ig,jg
      integer itlo,ithi
      integer jtlo,jthi
      integer jmin,jmax
      integer imin,imax
      _RL iestau(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      _RL c0,pr,sal,vt,tmp,vs,vp,vstp
      _RL SW_TEMP

C     == External functions ==
      external SW_TEMP
CEOP

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)
      jmin = 1
      jmax = sny
      imin = 1
      imax = snx

      c0=1402.392
      pr = 0.0
      sal = 0.0
      vt = 0.0
      tmp = 0.0
      vs = 0.0
      vp = 0.0
      vstp = 0.0
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                csound(i,j,k,bi,bj) = 0.0
              enddo
            enddo
          enddo
        enddo
      enddo
      do bj = jtlo,jthi
        do bi = itlo,ithi
          do k = 1,nr
            do j = jmin,jmax
              do i =  imin,imax
                if (hFacC(i,j,k,bi,bj).gt.0.0) then
C pr units: meters
                  pr = rhoConst*( totPhiHyd(i,j,k,bi,bj)
     &                  -rC(k)*gravity )*SItodBar
                  tmp = SW_TEMP(SALT(i,j,k,bi,bj),
     &                   THETA(i,j,k,bi,bj),pr,0. _d 0)
C convert pressure to kg/cm^2 for compliance with aog_vsdg script
                  pr = pr/gravity
                  sal = SALT(i,j,k,bi,bj)
                  vt = (5.01109398873-(0.0550946843172
     &                   - 0.000221535969240*tmp)*tmp)*tmp
                  vs = (1.32952290781 + 0.000128955756844*sal)*sal
                  vp = (0.156059257041 + (0.0000244998688441
     &                   - 0.00000000883392332513*pr)*pr)*pr
                  vstp = -0.0127562783426*tmp*sal
     &                   + 0.00635191613389*tmp*pr
     &                   + 0.0000000265484716608*tmp*tmp*pr*pr
     &                   - 0.00000159349479045*tmp*pr*pr
     &                   + 0.000000000522116437235*tmp*pr*pr*pr
     &                   - 0.000000438031096213*tmp*tmp*tmp*pr
     &                   - 0.00000000161674495909*sal*sal*pr*pr
     &                   + 0.0000968403156410*tmp*tmp*sal
     &                   + 0.00000485639620015*tmp*sal*sal*pr
     &                   - 0.000340597039004*tmp*sal*pr
                  csound(i,j,k,bi,bj) = c0+vt+vs+vp+vstp
                endif
              enddo
            enddo
          enddo

      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL(csound,'CSound  ',0,Nr,1,bi,bj,myThid)
      ENDIF

        enddo
      enddo

#endif /* INCLUDE_SOUNDSPEED_CALC_CODE */

      RETURN
      END






