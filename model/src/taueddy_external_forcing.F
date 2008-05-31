C $Header: /u/gcmpack/MITgcm/model/src/Attic/taueddy_external_forcing.F,v 1.4 2008/05/31 16:50:35 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: TAUEDDY_EXTERNAL_FORCING_U
C     !INTERFACE:
      SUBROUTINE TAUEDDY_EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TAUEDDY_EXTERNAL_FORCING_U
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
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEOP

#ifdef ALLOW_EDDYPSI
C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
C     number of surface interface layer
      INTEGER kSurface, Kp1
      _RL maskm1, maskp1

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C     Add zonal eddy momentum impulse into the layer
#ifdef ALLOW_GMREDI
      if ( GM_InMomAsStress ) then
#endif
      Kp1=min(klev+1,Nr)
      maskp1=1.
      maskm1=1.
      IF (klev.EQ.Nr) maskp1=0.
      IF (klev.EQ.1)  maskm1=0.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &  +foFacMom*
#ifdef ALLOW_GMREDI
     &  (maskm1*_maskW(i,j,klev,bi,bj)*GM_PsiX(i,j,klev,bi,bj)
     &  -maskp1*_maskW(i,j,kp1,bi,bj)*GM_PsiX(i,j,kp1,bi,bj))
#else
     &  (maskm1*_maskW(i,j,klev,bi,bj)*eddyPsiX(i,j,klev,bi,bj)
     &  -maskp1*_maskW(i,j,kp1,bi,bj)*eddyPsiX(i,j,kp1,bi,bj))
#endif
     &  *recip_drF(klev)*_recip_hFacW(i,j,klev,bi,bj)
     &  *0.5*(_fCori(i,j,bi,bj)+_fCori(i-1,j,bi,bj))
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      endif
#endif

#endif

      RETURN
      END
CBOP
C     !ROUTINE: TAUEDDY_EXTERNAL_FORCING_V
C     !INTERFACE:
      SUBROUTINE TAUEDDY_EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TAUEDDY_EXTERNAL_FORCING_V
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
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEOP

#ifdef ALLOW_EDDYPSI
C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
C     number of surface interface layer
      INTEGER kSurface, Kp1
      _RL maskm1, maskp1

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C     Add meridional eddy momentum impulse into the layer
#ifdef ALLOW_GMREDI
      if ( GM_InMomAsStress ) then
#endif
      Kp1=min(klev+1,Nr)
      maskp1=1.
      maskm1=1.
      IF (klev.EQ.Nr) maskp1=0.
      IF (klev.EQ.1)  maskm1=0.
      DO j=jMin,jMax
       DO i=iMin,iMax
        gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &  +foFacMom*
#ifdef ALLOW_GMREDI
     &  (maskm1*_maskS(i,j,klev,bi,bj)*GM_PsiY(i,j,klev,bi,bj)
     &  -maskp1*_maskS(i,j,kp1,bi,bj)*GM_PsiY(i,j,kp1,bi,bj))
#else
     &  (maskm1*_maskS(i,j,klev,bi,bj)*eddyPsiY(i,j,klev,bi,bj)
     &  -maskp1*_maskS(i,j,kp1,bi,bj)*eddyPsiY(i,j,kp1,bi,bj))
#endif
     &  *recip_drF(klev)*_recip_hFacS(i,j,klev,bi,bj)
     &  *0.5*(_fCori(i,j,bi,bj)+_fCori(i,j-1,bi,bj))
       ENDDO
      ENDDO
#ifdef ALLOW_GMREDI
      endif
#endif

#endif

      RETURN
      END
