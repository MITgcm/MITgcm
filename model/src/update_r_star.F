#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: UPDATE_R_STAR
C     !INTERFACE:
      SUBROUTINE UPDATE_R_STAR( useLatest, myTime, myIter, myThid )

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
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     useLatest :: if true use rStarFacC, else use rStarFacNm1C
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: Thread number for this instance of the routine.
      LOGICAL useLatest
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
#ifdef NONLIN_FRSURF
C     Local variables
C     i,j,k,bi,bj :: loop counter
      INTEGER i,j,k,bi,bj
CEOP

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       IF (useLatest) THEN

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
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

       ELSE

        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
# ifndef DISABLE_RSTAR_CODE
C-- Update the fractional thickness hFacC , hFacW & hFacS (&
C"recip_hFac") :
            hFacC(i,j,k,bi,bj) = h0FacC(i,j,k,bi,bj)
     &                          *rStarFacNm1C(i,j,bi,bj)
            hFacW(i,j,k,bi,bj) = h0FacW(i,j,k,bi,bj)
     &                          *rStarFacNm1W(i,j,bi,bj)
            hFacS(i,j,k,bi,bj) = h0FacS(i,j,k,bi,bj)
     &                          *rStarFacNm1S(i,j,bi,bj)
#endif
C
#ifdef USE_MASK_AND_NO_IF
            recip_hFacC(i,j,k,bi,bj) = maskC(i,j,k,bi,bj) /
     &        ( _hFacC(i,j,k,bi,bj) + (oneRS - maskC(i,j,k,bi,bj)) )
            recip_hFacW(i,j,k,bi,bj) = maskW(i,j,k,bi,bj) /
     &        ( _hFacW(i,j,k,bi,bj) + (oneRS - maskW(i,j,k,bi,bj)) )
            recip_hFacS(i,j,k,bi,bj) = maskS(i,j,k,bi,bj) /
     &        ( _hFacS(i,j,k,bi,bj) + (oneRS - maskS(i,j,k,bi,bj)) )
#else
            IF ( maskC(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacC(i,j,k,bi,bj) = oneRS / _hFacC(i,j,k,bi,bj)
            IF ( maskW(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacW(i,j,k,bi,bj) = oneRS / _hFacW(i,j,k,bi,bj)
            IF ( maskS(i,j,k,bi,bj).NE.zeroRS )
     &        recip_hFacS(i,j,k,bi,bj) = oneRS / _hFacS(i,j,k,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO

       ENDIF

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
