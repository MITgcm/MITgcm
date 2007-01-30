C $Header: /u/gcmpack/MITgcm/model/src/update_surf_dr.F,v 1.6 2007/01/30 03:18:13 heimbach Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_SURF_DR
C     !INTERFACE:
      SUBROUTINE UPDATE_SURF_DR( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE UPDATE_SURF_DR                                 
C     | o Update the surface-level thickness fraction (hFacC,W,S) 
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
C     i,j,bi,bj - loop counter
      INTEGER i,j,bi,bj
      INTEGER ks
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
CADJ STORE hfac_surfc(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
CADJ STORE hfac_surfs(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
CADJ STORE hfac_surfw(:,:,bi,bj) = 
CADJ &     comlev1_bibj, key = idynkey, byte = isbyte
# endif /* ALLOW_AUTODIFF_TAMC */
C-- Apply OBC to hFac_surfW,S before updating hFacW,S
        IF (useOBCS) THEN
         CALL OBCS_APPLY_SURF_DR(
     I                    bi, bj, ksurfC, ksurfW, ksurfS,
     U                    hFac_surfC, hFac_surfW, hFac_surfS,
     I                    myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C-- Update the fractional thickness "hFacC" of the surface level ksurfC :
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx 
          ks = ksurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacC(i,j,ks,bi,bj) = hFac_surfC(i,j,bi,bj)
           recip_hFacC(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfC(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Update fractional thickness "hFacW" & "hFacS" (at U and V points) 

        DO j=1-Oly,sNy+Oly
         DO i=2-Olx,sNx+Olx 
          ks = ksurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacW(i,j,ks,bi,bj) = hFac_surfW(i,j,bi,bj)
           recip_hFacW(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfW(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO
        DO j=2-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx 
          ks = ksurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           hFacS(i,j,ks,bi,bj) = hFac_surfS(i,j,bi,bj)
           recip_hFacS(i,j,ks,bi,bj)= 1. _d 0 / hFac_surfS(i,j,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C- end bi,bj loop
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
