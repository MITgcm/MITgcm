#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_NLFS_VARS
C     !INTERFACE:
      SUBROUTINE INI_NLFS_VARS( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_NLFS_VARS
C     | o Initialise variables for Non-Linear Free-Surface
C     |   formulations (formerly INI_SURF_DR & INI_R_STAR)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: my Thread Id. number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
      INTEGER bi,bj
#ifdef NONLIN_FRSURF
      INTEGER i, j, ks
# ifdef ALLOW_AUTODIFF
      INTEGER k
# endif
      _RL hFacInfMOM, Rmin_tmp
#else /* NONLIN_FRSURF */
# ifdef EXACT_CONSERV
      INTEGER i, j
# endif
#endif /* NONLIN_FRSURF */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INI_NLFS_VARS',myThid)
#endif

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    1rst bi,bj loop :

#ifdef EXACT_CONSERV
C-- Initialise arrays (defined within ifdef EXACT_CONSERV):
C   note: should be done elsewhere, outside ifdef NONLIN_FRSURF bloc
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaHnm1(i,j,bi,bj) = 0.
            dEtaHdt(i,j,bi,bj) = 0.
            PmEpR  (i,j,bi,bj) = 0.
          ENDDO
         ENDDO
#endif /* EXACT_CONSERV */

#ifdef NONLIN_FRSURF
C-- Initialise arrays (NLFS using r-coordinate):
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           hFac_surfC(i,j,bi,bj) = 0.
           hFac_surfW(i,j,bi,bj) = 0.
           hFac_surfS(i,j,bi,bj) = 0.
           hFac_surfNm1C(i,j,bi,bj) = 0.
           hFac_surfNm1W(i,j,bi,bj) = 0.
           hFac_surfNm1S(i,j,bi,bj) = 0.
           Rmin_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
          ENDDO
         ENDDO

C-- Initialise arrays (NLFS using r* coordinate):
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            rStarFacC(i,j,bi,bj) = 1.
            rStarFacW(i,j,bi,bj) = 1.
            rStarFacS(i,j,bi,bj) = 1.
            pStarFacK(i,j,bi,bj) = 1.
            rStarFacNm1C(i,j,bi,bj) = 1.
            rStarFacNm1W(i,j,bi,bj) = 1.
            rStarFacNm1S(i,j,bi,bj) = 1.
            rStarExpC(i,j,bi,bj) = 1.
            rStarExpW(i,j,bi,bj) = 1.
            rStarExpS(i,j,bi,bj) = 1.
            rStarDhCDt(i,j,bi,bj) = 0.
            rStarDhWDt(i,j,bi,bj) = 0.
            rStarDhSDt(i,j,bi,bj) = 0.
          ENDDO
         ENDDO

C-- Initialise arrays (NLFS using hybrid sigma-coordinate):
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            etaHw  (i,j,bi,bj) = 0.
            etaHs  (i,j,bi,bj) = 0.
            dEtaWdt(i,j,bi,bj) = 0.
            dEtaSdt(i,j,bi,bj) = 0.
          ENDDO
         ENDDO

# ifdef ALLOW_AUTODIFF
C--  to make TAF happy: reset hFac to h0Fac (copied from hFac in ini_linear_phisurf)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            hFacC(i,j,k,bi,bj) = h0FacC(i,j,k,bi,bj)
            hFacW(i,j,k,bi,bj) = h0FacW(i,j,k,bi,bj)
            hFacS(i,j,k,bi,bj) = h0FacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
#  ifdef USE_MASK_AND_NO_IF
            recip_hFacC(i,j,k,bi,bj) = maskC(i,j,k,bi,bj) /
     &        ( _hFacC(i,j,k,bi,bj) + (oneRS - maskC(i,j,k,bi,bj)) )
            recip_hFacW(i,j,k,bi,bj) = maskW(i,j,k,bi,bj) /
     &        ( _hFacW(i,j,k,bi,bj) + (oneRS - maskW(i,j,k,bi,bj)) )
            recip_hFacS(i,j,k,bi,bj) = maskS(i,j,k,bi,bj) /
     &        ( _hFacS(i,j,k,bi,bj) + (oneRS - maskS(i,j,k,bi,bj)) )
#  else
            IF ( maskC(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacC(i,j,k,bi,bj) = oneRS / _hFacC(i,j,k,bi,bj)
            IF ( maskW(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacW(i,j,k,bi,bj) = oneRS / _hFacW(i,j,k,bi,bj)
            IF ( maskS(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacS(i,j,k,bi,bj) = oneRS / _hFacS(i,j,k,bi,bj)
#  endif
           ENDDO
          ENDDO
         ENDDO
# endif /* ALLOW_AUTODIFF */
#endif /* NONLIN_FRSURF */

C-    end 1rst bi,bj loop.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef NONLIN_FRSURF

      hFacInfMOM = hFacInf

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C-- Compute the mimimum value of r_surf (used for computing hFac_surfC)
         DO j=1,sNy
          DO i=1,sNx
           ks = kSurfC(i,j,bi,bj)
           IF (ks.LE.Nr) THEN
             Rmin_tmp = rF(ks+1)
             IF ( ks.EQ.kSurfW(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i-1,j,bi,bj))
             IF ( ks.EQ.kSurfW(i+1,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i+1,j,bi,bj))
             IF ( ks.EQ.kSurfS(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j-1,bi,bj))
             IF ( ks.EQ.kSurfS(i,j+1,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j+1,bi,bj))

             Rmin_surf(i,j,bi,bj) =
     &        MAX( MAX(rF(ks+1),R_low(i,j,bi,bj)) + hFacInf*drF(ks),
     &                                Rmin_tmp + hFacInfMOM*drF(ks)
     &           )
           ENDIF
          ENDDO
         ENDDO

C-    end bi,bj loop.
       ENDDO
      ENDDO

      CALL EXCH_XY_RL( Rmin_surf, myThid )

#endif /* NONLIN_FRSURF */
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INI_NLFS_VARS',myThid)
#endif

      RETURN
      END
