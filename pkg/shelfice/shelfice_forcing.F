#include "SHELFICE_OPTIONS.h"

C--  File shelfice_forcing.F:
C--   Contents
C--   o SHELFICE_FORCING_T
C--   o SHELFICE_FORCING_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_FORCING_T
C     !INTERFACE:
      SUBROUTINE SHELFICE_FORCING_T(
     U                    gT_arr,
     I                    iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHELFICE_FORCING_T
C     | o Contains problem specific forcing for temperature.
C     *==========================================================*
C     | Adds terms to gT for forcing by shelfice sources
C     | e.g. heat flux
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     kLev      :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j   :: Loop counters
C     kp1,km1 :: index of next/previous level
C     gTloc   :: local tendency in boundary layer
C     drLoc   :: fractional cell width of boundary layer in (k+/-1)th layer
      INTEGER i, j
      INTEGER Kp1, Km1
      _RS     drLoc
      _RL     gTloc
CEOP

C--   Forcing term
      IF ( SHELFICEboundaryLayer ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( kLev .LT. Nr .AND. kLev .EQ. kTopC(I,J,bi,bj) ) THEN
          kp1 = MIN(kLev+1,Nr)
          drLoc = drF(kLev)*( 1. _d 0 - _hFacC(I,J,kLev,bi,bj) )
          drLoc = MIN( drLoc, drF(Kp1) * _hFacC(I,J,Kp1,bi,bj) )
          drLoc = MAX( drLoc, 0. _d 0)
          gTloc = shelficeForcingT(i,j,bi,bj)
     &         /( drF(kLev)*_hFacC(I,J,kLev,bi,bj)+drLoc )
          gT_arr(i,j) = gT_arr(i,j) + gTloc
         ELSEIF ( kLev .GT. 1 .AND. kLev-1 .EQ. kTopC(I,J,bi,bj) ) THEN
          km1 = MAX(kLev-1,1)
          drLoc = drF(km1)*( 1. _d 0 - _hFacC(I,J,km1,bi,bj) )
          drLoc = MIN( drLoc, drF(kLev) * _hFacC(I,J,kLev,bi,bj) )
          drLoc = MAX( drLoc, 0. _d 0)
          gTloc = shelficeForcingT(i,j,bi,bj)
     &         /( drF(km1)*_hFacC(I,J,km1,bi,bj)+drLoc )
C     The following is shorthand for the averaged tendency:
C     gT(k+1) = gT(k+1) + { gTloc * [drF(k)*(1-hFacC(k))]
C                       +   0     * [drF(k+1) - drF(k)*(1-hFacC(k))]
C                         }/[drF(k+1)*hFacC(k+1)]
          gT_arr(i,j) = gT_arr(i,j) + gTloc
     &         * drLoc*recip_drF(kLev)* _recip_hFacC(i,j,kLev,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_SHELFICE */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_FORCING_S
C     !INTERFACE:
      SUBROUTINE SHELFICE_FORCING_S(
     U                    gS_arr,
     I                    iMin,iMax,jMin,jMax, kLev, bi, bj,
     I                    myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R SHELFICE_FORCING_S
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gS for forcing by shelfice sources
C     | e.g. fresh-water flux (virtual salt flux).
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
c#include "DYNVARS.h"
c#include "FFIELDS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     kLev      :: Current vertical level index
C     bi,bj     :: Current tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number
C     myThid    :: my Thread Id number
      _RL     gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER kLev, bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     i,j   :: Loop counters
C     kp/m1 :: index of next/previous level
C     gTloc :: local tendency in boundary layer
C     drLoc :: fractional cell width of boundary layer
      INTEGER i, j
      INTEGER Kp1, Km1
      _RS     drLoc
      _RL     gSloc
CEOP

C--   Forcing term
      IF ( SHELFICEboundaryLayer ) THEN
       DO j=1,sNy
        DO i=1,sNx
         IF ( kLev .LT. Nr .AND. kLev .EQ. kTopC(I,J,bi,bj) ) THEN
          kp1 = MIN(kLev+1,Nr)
          drLoc = drF(kLev)*( 1. _d 0 - _hFacC(I,J,kLev,bi,bj) )
          drLoc = MIN( drLoc, drF(Kp1) * _hFacC(I,J,Kp1,bi,bj) )
          drLoc = MAX( drLoc, 0. _d 0)
          gSloc = shelficeForcingS(i,j,bi,bj)
     &         /( drF(kLev)*_hFacC(I,J,kLev,bi,bj)+drLoc )
          gS_arr(i,j) = gS_arr(i,j) + gSloc
         ELSEIF ( kLev .GT. 1 .AND. kLev-1 .EQ. kTopC(I,J,bi,bj) ) THEN
          km1 = MAX(kLev-1,1)
          drLoc = drF(km1)*( 1. _d 0 - _hFacC(I,J,km1,bi,bj) )
          drLoc = MIN( drLoc, drF(kLev) * _hFacC(I,J,kLev,bi,bj) )
          drLoc = MAX( drLoc, 0. _d 0)
          gSloc = shelficeForcingS(i,j,bi,bj)
     &         /( drF(km1)*_hFacC(I,J,km1,bi,bj)+drLoc )
C     The following is shorthand for the averaged tendency:
C     gS(k+1) = gS(k+1) + { gSloc * [drF(k)*(1-hFacC(k))]
C                       +   0     * [drF(k+1) - drF(k)*(1-hFacC(k))]
C                         }/[drF(k+1)*hFacC(k+1)]
          gS_arr(i,j) = gS_arr(i,j) + gSloc
     &         * drLoc*recip_drF(kLev)* _recip_hFacC(i,j,kLev,bi,bj)
         ENDIF
        ENDDO
       ENDDO
      ENDIF

#endif /* ALLOW_SHELFICE */
      RETURN
      END
