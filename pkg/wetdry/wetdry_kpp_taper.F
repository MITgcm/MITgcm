#include "WETDRY_OPTIONS.h"
#ifdef ALLOW_KPP
# include "KPP_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_KPP_TAPER

C !INTERFACE:
      SUBROUTINE WETDRY_KPP_TAPER( bi, bj, myThid )

C !DESCRIPTION:
C     Limit the KPP non-local transport in thin water (r* columns):
C     1) Cap the non-local flux fraction. The non-local flux through the
C        top of level k is  Kz(k)*KPPghat(k-1)*Q  (Q the surface flux) and
C        must not exceed Q. With weak surface forcing KPPghat ~ 1/(ws*hbl)
C        is large, and in fast thin-water flow the shear-driven interior
C        part of Kz is large too, so the product can exceed 1: the term
C        then moves more heat or salt than the surface supplies (in a
C        test with -1.5 C water on a tidal flat, T went to -2.6..+4.6 C).
C        KPPghat(k-1) is limited to 1/max(KPPdiffKzT(k), KPPdiffKzS(k)),
C        only in columns shallower than wdKPPCapDepth (deeper columns
C        keep stock KPP).
C     2) Optionally (wdKPPDepth > wdCritDepth) ramp KPPghat to zero in
C        columns shallower than wdKPPDepth, with the factor
C        fC = MIN(1, MAX(0, (D - wdCritDepth)/(wdKPPDepth - wdCritDepth)))
C     Called at the end of S/R KPP_CALC when useWETDRY.

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "WETDRY.h"
#ifdef ALLOW_KPP
# include "KPP.h"
#endif

C !INPUT PARAMETERS:
      INTEGER bi, bj
      INTEGER myThid
CEOP

#if (defined ALLOW_WETDRY) && (defined ALLOW_KPP) && (defined KPP_GHAT)
C !LOCAL VARIABLES:
      INTEGER i, j, k
      _RL fC, kpRange, kzMax

      IF ( .NOT.useWETDRY ) RETURN

C--   cap the non-local flux fraction at 1 in shallow columns
      IF ( wdKPPCapDepth.GT.zeroRL ) THEN
       DO k=2,Nr
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          kzMax = MAX( KPPdiffKzT(i,j,k,bi,bj),
     &                 KPPdiffKzS(i,j,k,bi,bj) )
          IF ( kzMax*KPPghat(i,j,k-1,bi,bj).GT.oneRL .AND.
     &         etaH(i,j,bi,bj) + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
     &         .LT.wdKPPCapDepth )
     &      KPPghat(i,j,k-1,bi,bj) = oneRL/kzMax
         ENDDO
        ENDDO
       ENDDO
      ENDIF

      kpRange = wdKPPDepth - wdCritDepth
      IF ( kpRange.LE.zeroRL ) RETURN

      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        fC = MIN( oneRL, MAX( zeroRL,
     &     ( etaH(i,j,bi,bj) + Ro_surf(i,j,bi,bj) - R_low(i,j,bi,bj)
     &       - wdCritDepth )/kpRange ) )
        IF ( fC.LT.oneRL ) THEN
         DO k=1,Nr
          KPPghat(i,j,k,bi,bj) = KPPghat(i,j,k,bi,bj)*fC
         ENDDO
        ENDIF
       ENDDO
      ENDDO
#endif /* ALLOW_WETDRY and ALLOW_KPP and KPP_GHAT */

      RETURN
      END
