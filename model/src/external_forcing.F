C $Header: /u/gcmpack/MITgcm/model/src/external_forcing.F,v 1.13 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: EXTERNAL_FORCING_U
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_U                                    
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
CEOP

C--   Forcing term
C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. 1 ) THEN
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
CBOP
C     !ROUTINE: EXTERNAL_FORCING_V
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_V                                    
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
CEOP

C--   Forcing term
C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. 1 ) THEN
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
CBOP
C     !ROUTINE: EXTERNAL_FORCING_T
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_T                                    
C     | o Contains problem specific forcing for temperature.      
C     *==========================================================*
C     | Adds terms to gT for forcing by external sources          
C     | e.g. heat flux, climatalogical relaxation..............   
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
#ifdef SHORTWAVE_HEATING
      integer two
      _RL minusone
      parameter (two=2,minusone=-1.)
      _RL swfracb(two)
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
CEndOfInterface

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
CEOP

C--   Forcing term
C     Add heat in top-layer
      IF ( kLev .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &     +maskC(i,j,kLev,bi,bj)*surfaceTendencyT(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef SHORTWAVE_HEATING
C Penetrating SW radiation
      swfracb(1)=abs(rF(klev))
      swfracb(2)=abs(rF(klev+1))
      call SWFRAC(
     I     two,minusone,
     I     myCurrentTime,myThid,
     O     swfracb)
      DO j=jMin,jMax
       DO i=iMin,iMax
        gT(i,j,klev,bi,bj) = gT(i,j,klev,bi,bj) 
     &   -maskC(i,j,klev,bi,bj)*Qsw(i,j,bi,bj)*(swfracb(1)-swfracb(2))
     &    *recip_Cp*recip_rhoNil*recip_drF(klev)
       ENDDO
      ENDDO
#endif
      RETURN
      END
CBOP
C     !ROUTINE: EXTERNAL_FORCING_S
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_S                                    
C     | o Contains problem specific forcing for merid velocity.   
C     *==========================================================*
C     | Adds terms to gS for forcing by external sources          
C     | e.g. fresh-water flux, climatalogical relaxation.......   
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

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Loop counters
      INTEGER I, J
CEOP

C--   Forcing term
C     Add fresh-water in top-layer
      IF ( kLev .EQ. 1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &   +maskC(i,j,kLev,bi,bj)*surfaceTendencyS(i,j,bi,bj)
        ENDDO
       ENDDO
      ENDIF

      RETURN
      END
