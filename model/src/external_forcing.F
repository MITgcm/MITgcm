C $Header: /u/gcmpack/MITgcm/model/src/external_forcing.F,v 1.7 2000/09/11 20:45:57 heimbach Exp $

#include "CPP_OPTIONS.h"

CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_U                                   |
C     | o Contains problem specific forcing for zonal velocity.  |
C     |==========================================================|
C     | Adds terms to gU for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. 1 ) THEN
       CALL EXTERNAL_FORCING_SURF_U( 
     &           iMin, iMax, jMin, jMax,bi,bj,myThid )
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceTendencyU(i,j,bi,bj)
     &   *_maskW(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_V                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gV for forcing by external sources         |
C     | e.g. wind stress, bottom friction etc..................  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"


C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface
C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. 1 ) THEN
       CALL EXTERNAL_FORCING_SURF_V( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceTendencyV(i,j,bi,bj)
     &   *_maskS(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_T                                   |
C     | o Contains problem specific forcing for temperature.     |
C     |==========================================================|
C     | Adds terms to gT for forcing by external sources         |
C     | e.g. heat flux, climatalogical relaxation..............  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#ifdef SHORTWAVE_HEATING
      integer    two, k
      _RS one
      parameter (two=2,one=1.)
       _RS ztmp(two), swfracb(two)
#endif


C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
C     Add heat in top-layer
      IF ( kLev .EQ. 1 ) THEN
       CALL EXTERNAL_FORCING_SURF_T( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
       DO j=jMin,jMax
        DO i=iMin,iMax
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &     +maskC(i,j)*surfaceTendencyT(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef SHORTWAVE_HEATING
C Penetrating SW radiation
      ztmp(1)=0.0
      do k=1,klev-1
         ztmp(1)=ztmp(1)-delZ(k)
      enddo
      ztmp(2)=ztmp(1)-delZ(klev)
      call SWFRAC(two,one,ztmp, swfracb)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gT(i,j,klev,bi,bj) = gT(i,j,klev,bi,bj)
     &    +maskC(i,j)*Qsw(i,j,bi,bj)*(swfracb(1)-swfracb(2))
       ENDDO
      ENDDO
#endif
      RETURN
      END
CStartOfInterface
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           maskC,
     I           myCurrentTime,myThid)
C     /==========================================================\
C     | S/R EXTERNAL_FORCING_S                                   |
C     | o Contains problem specific forcing for merid velocity.  |
C     |==========================================================|
C     | Adds terms to gS for forcing by external sources         |
C     | e.g. fresh-water flux, climatalogical relaxation.......  |
C     \==========================================================/
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"

C     == Routine arguments ==
C     iMin - Working range of tile for applying forcing.
C     iMax
C     jMin
C     jMax
C     kLev
      _RS maskC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myCurrentTime
      INTEGER myThid
CEndOfInterface

C     == Local variables ==
C     Loop counters
      INTEGER I, J

C--   Forcing term
C     Add fresh-water in top-layer
      IF ( kLev .EQ. 1 ) THEN
       CALL EXTERNAL_FORCING_SURF_S( 
     I           iMin, iMax, jMin, jMax,bi,bj,myThid )
       DO j=jMin,jMax
        DO i=iMin,iMax
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &   +maskC(i,j)*surfaceTendencyS(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
