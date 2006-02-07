C $Header: /u/gcmpack/MITgcm/pkg/shelfice/shelfice_forcing.F,v 1.1 2006/02/07 11:45:21 mlosch Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_FORCING_T
C     !INTERFACE:
      SUBROUTINE SHELFICE_FORCING_T(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
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
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
C     kSurface  :: index of surface layer
      INTEGER i, j
      INTEGER kSurface
CEOP

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
      DO j=1,sNy
       DO i=1,sNx
        IF ( kLev .EQ. kTopC(I,J,bi,bj) ) THEN
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &        +shelficeForcingT(i,j,bi,bj)
     &        *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_SHELFICE */
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: SHELFICE_FORCING_S
C     !INTERFACE:
      SUBROUTINE SHELFICE_FORCING_S(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )

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
#include "DYNVARS.h"
C#include "FFIELDS.h"
#include "SHELFICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

#ifdef ALLOW_SHELFICE
C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
C     kSurface  :: index of surface layer
      INTEGER i, j
      INTEGER kSurface
CEOP

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
      DO j=1,sNy
       DO i=1,sNx
        IF ( kLev .EQ. kTopC(I,J,bi,bj) ) THEN
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &        +shelficeForcingS(i,j,bi,bj)
     &        *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDIF
       ENDDO
      ENDDO

#endif /* ALLOW_SHELFICE */
      RETURN
      END
