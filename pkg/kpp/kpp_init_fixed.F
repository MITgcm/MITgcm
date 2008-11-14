C $Header: /u/gcmpack/MITgcm/pkg/kpp/kpp_init_fixed.F,v 1.2 2008/11/14 21:34:48 dfer Exp $
C $Name:  $

#include "KPP_OPTIONS.h"

CBOP
C     !ROUTINE: KPP_INIT_FIXED
C     !INTERFACE:
      SUBROUTINE KPP_INIT_FIXED( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE KPP_INIT_FIXED
C     | o Routine to initialize GM/Redi variables 
C     |   that are kept fixed during the run.
C     *==========================================================*
C     \ev
C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "KPP_PARAMS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     === Local variables ===
C     i,j,k,bi,bj - Loop counters
C     zehat       - zeta * ustar**3
C     zeta        - Stability parameter d/l
      INTEGER i, j, k, bi, bj
      _RL zehat
      _RL zeta
      _RL usta
      _RL p25, p33


      _BEGIN_MASTER(myThid)

      p25 = 0.25 _d 0
      p33 = 1. _d 0 / 3. _d 0

C-----------------------------------------------------------------------
C Initialize constants that depend on parameters in data.kpp
C-----------------------------------------------------------------------

      Vtc     = concv * SQRT(0.2 _d 0 /concs/epsilon) / vonk**2 / Ricr
      cg      = cstar * vonk * (concs * vonk * epsilon)**p33

c-----------------------------------------------------------------------
c construct the wm and ws lookup tables
c-----------------------------------------------------------------------

      deltaz = (zmax - zmin)/(nni + 1)
      deltau = (umax - umin)/(nnj + 1)

      DO i = 0, nni + 1
         zehat = deltaz*i + zmin
         DO j = 0, nnj + 1
            usta = deltau*j + umin
            zeta = zehat / max(phepsi,usta**3)
            IF (zehat .GE. 0.) THEN
               wmt(i,j) = vonk*usta/(1. + conc1*zeta)
               wst(i,j) = wmt(i,j)
            ELSE
               IF (zeta .GT. zetam) THEN
                  wmt(i,j) = vonk*usta*(1. - conc2*zeta)**p25
               ELSE
                  wmt(i,j) = vonk*(conam*usta**3 - concm*zehat)**p33
               ENDIF
               IF (zeta .GT. zetas) THEN
                  wst(i,j) = vonk*usta*SQRT(1. _d 0 - conc3*zeta)
               ELSE
                  wst(i,j) = vonk*(conas*usta**3 - concs*zehat)**p33
               ENDIF
            ENDIF
         ENDDO
      ENDDO

C-----------------------------------------------------------------------
C     vertical grid
C-----------------------------------------------------------------------

      IF (minKPPhbl .EQ. UNSET_RL) THEN
         minKPPhbl = -rC(1)
      ENDIF
      zgrid(0)  =  phepsi
      hwide(0)  =  phepsi
c     zgrid(1)  = -drF(1)*0.5
c     hwide(1)  =  drF(1)
c     DO k = 2, Nr
c        zgrid(k) = zgrid(k-1) - (drF(k-1)+drF(k))*0.5
c        hwide(k) = drF(k)
c     ENDDO
C- jmc : use the model vertical grid :
      DO k = 1, Nr
         zgrid(k) = rC(k)
         hwide(k) = drF(k)
      ENDDO

      zgrid(Nrp1) = zgrid(Nr) * 100.

      hwide(Nrp1) = phepsi

      _END_MASTER(myThid)
      _BARRIER

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL KPP_DIAGNOSTICS_INIT( myThid )
      ENDIF
#endif

      RETURN
      END
