C $Header: /u/gcmpack/MITgcm/model/src/update_r_star.F,v 1.7 2008/08/12 22:42:33 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_R_STAR
C     !INTERFACE:
      SUBROUTINE UPDATE_R_STAR( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_R_STAR                                 
C     | o Update the thickness fractions (hFacC,W,S) 
C     |   according to the surface r-position = Non-Linear FrSurf 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "DYNVARS.h"
#include "GRID.h"
#include "SURFACE.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid - Thread number for this instance of the routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     Local variables
C     i,j,k,bi,bj - loop counter
      INTEGER i,j,k,bi,bj
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)  

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          idynkey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_OBCS
# ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE rStarFacC(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
CADJ STORE rStarFacS(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
CADJ STORE rStarFacW(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
C-- Apply OBC to rStar_Factor_W,S before updating hFacW,S
        IF (useOBCS) THEN
         CALL OBCS_APPLY_R_STAR(
     I                    bi, bj,
     U                    rStarFacC, rStarFacW, rStarFacS,
     I                    myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
# ifndef DISABLE_RSTAR_CODE
C-- Update the fractional thickness hFacC , hFacW & hFacS (& "recip_hFac") :
            hFacC(i,j,k,bi,bj) = h0FacC(i,j,k,bi,bj)
     &                          *rStarFacC(i,j,bi,bj)
            hFacW(i,j,k,bi,bj) = h0FacW(i,j,k,bi,bj)
     &                          *rStarFacW(i,j,bi,bj)
            hFacS(i,j,k,bi,bj) = h0FacS(i,j,k,bi,bj)
     &                          *rStarFacS(i,j,bi,bj)
#endif
C
#ifdef USE_MASK_AND_NO_IF
            recip_hFacC(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
     &        / ( _hFacC(i,j,k,bi,bj) + (1.-maskC(i,j,k,bi,bj)) )
            recip_hFacW(i,j,k,bi,bj) = maskW(i,j,k,bi,bj)
     &        / ( _hFacW(i,j,k,bi,bj) + (1.-maskW(i,j,k,bi,bj)) )
            recip_hFacS(i,j,k,bi,bj) = maskS(i,j,k,bi,bj)
     &        / ( _hFacS(i,j,k,bi,bj) + (1.-maskS(i,j,k,bi,bj)) )
#else
           IF (maskC(i,j,k,bi,bj).NE.0.)
     &      recip_hFacC(i,j,k,bi,bj) = 1. _d 0 / _hFacC(i,j,k,bi,bj)
           IF (maskW(i,j,k,bi,bj).NE.0.)
     &      recip_hFacW(i,j,k,bi,bj) = 1. _d 0 / _hFacW(i,j,k,bi,bj)
           IF (maskS(i,j,k,bi,bj).NE.0.)
     &      recip_hFacS(i,j,k,bi,bj) = 1. _d 0 / _hFacS(i,j,k,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C- end bi,bj loop
       ENDDO
      ENDDO

c     _EXCH_XYZ_RS( hFacC, myThid )
c     _EXCH_XYZ_RS( recip_hFacC, myThid )
c     CALL EXCH_UV_XYZ_RS(hFacW,hFacS,.FALSE.,myThid)
c     CALL EXCH_UV_XYZ_RS(recip_hFacW,recip_hFacS,.FALSE.,myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
