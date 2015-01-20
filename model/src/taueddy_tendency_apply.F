C $Header: /u/gcmpack/MITgcm/model/src/taueddy_tendency_apply.F,v 1.2 2015/01/20 20:47:42 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

C--  File taueddy_tendency_apply.F: Routines to apply TAUEDDY tendencies
C--   Contents
C--   o TAUEDDY_TENDENCY_APPLY_U
C--   o TAUEDDY_TENDENCY_APPLY_V

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: TAUEDDY_TENDENCY_APPLY_U
C     !INTERFACE:
      SUBROUTINE TAUEDDY_TENDENCY_APPLY_U(
     U                     gU_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TAUEDDY_TENDENCY_APPLY_U
C     | o Contains problem specific forcing for zonal velocity.
C     *==========================================================*
C     | Adds terms to gU for forcing by external sources
C     | e.g. wind stress, bottom friction etc..................
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     gU_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EDDYPSI
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i, j
      INTEGER kp1
      _RL maskm1, maskp1

C     Add zonal eddy momentum impulse into the layer
#ifdef ALLOW_GMREDI
      IF ( GM_InMomAsStress ) THEN
#endif
      kp1 = MIN(k+1,Nr)
      maskp1 = 1.
      maskm1 = 1.
      IF (k.EQ.Nr) maskp1 = 0.
      IF (k.EQ.1)  maskm1 = 0.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU_arr(i,j) = gU_arr(i,j)
     &  +foFacMom*recip_rhoConst*
     &  ( maskm1*_maskW(i,j, k ,bi,bj)*tauxEddy(i,j, k ,bi,bj)
     &  - maskp1*_maskW(i,j,kp1,bi,bj)*tauxEddy(i,j,kp1,bi,bj) )
     &  *recip_drF(k)*_recip_hFacW(i,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      ENDIF
#endif

#endif /* ALLOW_EDDYPSI */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: TAUEDDY_TENDENCY_APPLY_V
C     !INTERFACE:
      SUBROUTINE TAUEDDY_TENDENCY_APPLY_V(
     U                     gV_arr,
     I                     iMin,iMax,jMin,jMax, k, bi, bj,
     I                     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TAUEDDY_TENDENCY_APPLY_V
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gV for forcing by external sources
C     | e.g. wind stress, bottom friction etc..................
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     gV_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_EDDYPSI
C     !LOCAL VARIABLES:
C     i, j      :: Loop counters
      INTEGER i, j
      INTEGER kp1
      _RL maskm1, maskp1

C     Add meridional eddy momentum impulse into the layer
#ifdef ALLOW_GMREDI
      IF ( GM_InMomAsStress ) THEN
#endif
      kp1 = MIN(k+1,Nr)
      maskp1 = 1.
      maskm1 = 1.
      IF (k.EQ.Nr) maskp1 = 0.
      IF (k.EQ.1)  maskm1 = 0.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV_arr(i,j) = gV_arr(i,j)
     &  +foFacMom*recip_rhoConst*
     &  ( maskm1*_maskS(i,j, k ,bi,bj)*tauyEddy(i,j, k ,bi,bj)
     &  - maskp1*_maskS(i,j,kp1,bi,bj)*tauyEddy(i,j,kp1,bi,bj) )
     &  *recip_drF(k)*_recip_hFacS(i,j,k,bi,bj)
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      ENDIF
#endif

#endif /* ALLOW_EDDYPSI */

      RETURN
      END
