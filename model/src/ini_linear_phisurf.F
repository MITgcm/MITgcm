C $Header: /u/gcmpack/MITgcm/model/src/ini_linear_phisurf.F,v 1.3 2001/09/26 18:09:15 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_LINEAR_PHISURF
C     !INTERFACE:
      SUBROUTINE INI_LINEAR_PHISURF( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_LINEAR_PHISURF                             
C     | o Initialise the Linear Relation Phi_surf(eta)            
C     *==========================================================*
C     | Presently: Initialise -Boyancy at surface level (Bo_surf) 
C     |  to setup the Linear relation: Phi_surf(eta)=Bo_surf*eta  
C     | Futur: might add other things for Non-Linear FreeSurface  
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid - Thread no. that called this routine.
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local variables ===
C     bi,bj  - Loop counters
C     I,J,K
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER bi, bj
      INTEGER I, J, K
      _RL     dPIdp
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Initialise -Boyancy at surface level : Bo_surf
C   Bo_surf is defined as d/dr(Phi_surf) and set to g/rtoz (linear free surface)
C     with rtoz = conversion factor from r-unit to z-unit  (=horiVertRatio)
C   an accurate formulation includes P_surf and T,S_surf effects on rho_surf:
C    (setting uniformLin_PhiSurf=.FALSE.):
C    z-ocean (rtoz=1) : Bo_surf = - Boyancy = gravity * rho_surf/rho_0 
C    p-atmos (rtoz=rho_c*g) : Bo_surf = (1/rho)_surf  
C Note on Phi_surf splitting : Non-linear Time-dependent effects on b_surf 
C    [through eta & (T-tRef)_surf] are included in PhiHyd rather than in Bo_surf
C--
      IF ( buoyancyRelation .eq. 'OCEANIC' ) THEN
C-  gBaro = gravity (except for External mode test with reduced gravity)
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
             Bo_surf(I,J,bi,bj) = gBaro
             recip_Bo(I,J,bi,bj) = 1. _d 0 / gBaro
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSEIF ( uniformLin_PhiSurf ) THEN
C-  use a linear (in ps) uniform relation : Phi'_surf = 1/rhoConst * ps'_surf
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
             Bo_surf(I,J,bi,bj) = recip_rhoConst
             recip_Bo(I,J,bi,bj) = rhoConst
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ELSE
C-  use a linearized (in ps) Non-uniform relation : Bo_surf(Po_surf,tRef_surf)
C--- Bo = d/d_p(Phi_surf) = tRef_surf*d/d_p(PI) ; PI = Cp*(p/Po)^kappa
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO J=1-Oly,sNy+Oly
           DO I=1-Olx,sNx+Olx
            IF ( Ro_surf(I,J,bi,bj).GT.0. _d 0 
     &          .AND. ksurfC(I,J,bi,bj).LE.Nr ) THEN
              dPIdp = (atm_cp*atm_kappa/atm_po)*
     &         (Ro_surf(I,J,bi,bj)/atm_po)**(atm_kappa-1. _d 0)
              Bo_surf(I,J,bi,bj) = dPIdp*tRef(ksurfC(I,J,bi,bj))
              recip_Bo(I,J,bi,bj) = 1. _d 0 / Bo_surf(I,J,bi,bj)
            ELSE
              Bo_surf(I,J,bi,bj) = 0.
              recip_Bo(I,J,bi,bj) = 0.
            ENDIF
           ENDDO
          ENDDO
         ENDDO
        ENDDO
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C--   Update overlap regions
      _EXCH_XY_R8(Bo_surf, myThid)
      _EXCH_XY_R8(recip_Bo, myThid)

      IF ( buoyancyRelation .eq. 'ATMOSPHERIC' .AND. 
     &     .NOT.uniformLin_PhiSurf ) THEN
        CALL WRITE_FLD_XY_RL( 'Bo_surf',' ',Bo_surf,0,myThid) 
      ENDIF

      RETURN
      END
