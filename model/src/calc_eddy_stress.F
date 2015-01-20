C $Header: /u/gcmpack/MITgcm/model/src/calc_eddy_stress.F,v 1.3 2015/01/20 20:47:42 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

      SUBROUTINE CALC_EDDY_STRESS( bi, bj, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R CALC_EDDY_STRESS
C     | o Calculates the eddy stress when running a residual
C     |   ocean model
C     *==========================================================*
C     | Calculates the eddy stress.  Later this will be added to
C     | gU the same as external sources (e.g. wind stress, bottom
C     | friction, etc.
C     *==========================================================*
C     \ev

      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
      INTEGER bi,bj
      INTEGER myThid
#ifdef ALLOW_EDDYPSI

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER i,j,k
C     Interpolated stream function and coriolis
      _RL psix, psiy, coriU, coriV

C     Calculate the eddy stress from the eddy induced streamfunction
#ifdef ALLOW_GMREDI
      IF ( GM_InMomAsStress ) THEN
#endif
        DO k=1,Nr

         DO j=1-OLy,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx
#ifdef ALLOW_GMREDI
           psiy = op25*(GM_PsiY(i,  j  ,k,bi,bj)
     &                 +GM_PsiY(i,  j+1,k,bi,bj)
     &                 +GM_PsiY(i-1,j  ,k,bi,bj)
     &                 +GM_PsiY(i-1,j+1,k,bi,bj))
#else
           psiy = op25*(eddyPsiY(i,  j  ,k,bi,bj)
     &                 +eddyPsiY(i,  j+1,k,bi,bj)
     &                 +eddyPsiY(i-1,j  ,k,bi,bj)
     &                 +eddyPsiY(i-1,j+1,k,bi,bj))
#endif
           coriU = op5*(fcori(i-1,j,bi,bj)
     &                 +fCori(i  ,j,bi,bj))
           tauxEddy(i,j,k,bi,bj) =  rhoConst*coriU*psiy
          ENDDO
         ENDDO

         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx,sNx+OLx-1
#ifdef ALLOW_GMREDI
           psix = op25*(GM_PsiX(i,  j  ,k,bi,bj)
     &                 +GM_PsiX(i+1,j  ,k,bi,bj)
     &                 +GM_PsiX(i  ,j-1,k,bi,bj)
     &                 +GM_PsiX(i+1,j-1,k,bi,bj))
#else
           psix = op25*(eddyPsiX(i,  j  ,k,bi,bj)
     &                 +eddyPsiX(i+1,j  ,k,bi,bj)
     &                 +eddyPsiX(i  ,j-1,k,bi,bj)
     &                 +eddyPsiX(i+1,j-1,k,bi,bj))
#endif
           coriV = op5*(fcori(i,j-1,bi,bj)
     &                 +fCori(i,j  ,bi,bj))
           tauyEddy(i,j,k,bi,bj) = -rhoConst*coriV*psix
          ENDDO
         ENDDO

C-    end k loop
        ENDDO

#ifdef ALLOW_DIAGNOSTICS
        IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( tauxEddy, 'TAUXEDDY',
     &                           0, Nr, 1, bi, bj, myThid )
          CALL DIAGNOSTICS_FILL( tauyEddy, 'TAUYEDDY',
     &                           0, Nr, 1, bi, bj, myThid )
        ENDIF
#endif
#ifdef ALLOW_GMREDI
      ENDIF
#endif
#endif /* ALLOW_EDDYPSI */
      RETURN
      END
