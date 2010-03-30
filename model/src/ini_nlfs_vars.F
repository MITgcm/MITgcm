C $Header: /u/gcmpack/MITgcm/model/src/ini_nlfs_vars.F,v 1.1 2010/03/30 17:45:11 jmc Exp $
C $Name:  $

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

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
      INTEGER i,j,k,bi,bj
      INTEGER ks
      _RL hFacInfMOM, Rmin_tmp
c     CHARACTER*(MAX_LEN_MBUF) suff
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_ENTER('INI_NLFS_VARS',myThid)
#endif

      hFacInfMOM = hFacInf

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
C-    1rst bi,bj loop :

C-- Initialise arrays (NLFS using r-coordinate):
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           hFac_surfC(i,j,bi,bj) = 0.
           hFac_surfW(i,j,bi,bj) = 0.
           hFac_surfS(i,j,bi,bj) = 0.
           PmEpR(i,j,bi,bj) = 0.
           Rmin_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
          ENDDO
         ENDDO

C-- Initialise arrays (NLFS using r* coordinate):
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
            rStarFacC(i,j,bi,bj) = 1.
            rStarFacW(i,j,bi,bj) = 1.
            rStarFacS(i,j,bi,bj) = 1.
            rStarExpC(i,j,bi,bj) = 1.
            rStarExpW(i,j,bi,bj) = 1.
            rStarExpS(i,j,bi,bj) = 1.
            rStarDhCDt(i,j,bi,bj) = 0.
            rStarDhWDt(i,j,bi,bj) = 0.
            rStarDhSDt(i,j,bi,bj) = 0.
            PmEpR(i,j,bi,bj) = 0.
          ENDDO
         ENDDO
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
            h0FacC(i,j,k,bi,bj) = _hFacC(i,j,k,bi,bj)
            h0FacW(i,j,k,bi,bj) = _hFacW(i,j,k,bi,bj)
            h0FacS(i,j,k,bi,bj) = _hFacS(i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO

C-    end 1rst bi,bj loop.
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C-- Compute the mimimum value of r_surf (used for computing hFac_surfC)
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           IF (ks.LE.Nr) THEN
             Rmin_tmp = rF(ks+1)
             IF ( ks.EQ.ksurfW(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i-1,j,bi,bj))
             IF ( ks.EQ.ksurfW(i+1,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i+1,j,bi,bj))
             IF ( ks.EQ.ksurfS(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j-1,bi,bj))
             IF ( ks.EQ.ksurfS(i,j+1,bi,bj))
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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_DEBUG
      IF (debugMode) CALL DEBUG_LEAVE('INI_NLFS_VARS',myThid)
#endif

#endif /* NONLIN_FRSURF */

      RETURN
      END
