C $Header: /u/gcmpack/MITgcm/model/src/external_forcing.F,v 1.28 2004/10/19 02:39:58 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif

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
C     number of surface interface layer
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
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_U(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_AIM */
C AMM
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_U(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_FIZHI */
C AMM

C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceForcingU(i,j,bi,bj)
     &   *recip_drF(kLev)*recip_hFacW(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_SPONGE))
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_U(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
      ENDIF
#endif

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
C     number of surface interface layer
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
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_V(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_AIM */

C AMM
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_V(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_FIZHI */
C AMM
C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceForcingV(i,j,bi,bj)
     &   *recip_drF(kLev)*recip_hFacS(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_SPONGE))
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_V(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
      ENDIF
#endif

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
C     number of surface interface layer
      INTEGER kSurface
#ifdef SHORTWAVE_HEATING
      integer two
      _RL minusone
      parameter (two=2,minusone=-1.)
      _RL swfracb(two)
      INTEGER kp1
#endif
CEOP

      IF ( fluidIsAir ) THEN
       kSurface = 0
      ELSEIF ( usingPCoords ) THEN
       kSurface = Nr
      ELSE
       kSurface = 1
      ENDIF

C--   Forcing term
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_T(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_AIM */

C AMM
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_T(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_FIZHI */
C AMM

C     Add heat in top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &     +surfaceForcingT(i,j,bi,bj)
     &     *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef SHORTWAVE_HEATING
C Penetrating SW radiation
      kp1 = klev+1
      swfracb(1)=abs(rF(klev))
      swfracb(2)=abs(rF(klev+1))
      CALL SWFRAC(
     I     two,minusone,
     I     myCurrentTime,myThid,
     U     swfracb)
      IF (klev.EQ.Nr) THEN
        kp1 = klev
        swfracb(2)=0. _d 0
      ENDIF
      DO j=jMin,jMax
       DO i=iMin,iMax
        gT(i,j,klev,bi,bj) = gT(i,j,klev,bi,bj) 
     &   -Qsw(i,j,bi,bj)*(swfracb(1)*maskC(i,j,klev,bi,bj)
     &                   -swfracb(2)*maskC(i,j,kp1, bi,bj))
     &    *recip_Cp*recip_rhoConst
     &    *recip_drF(klev)*recip_hFacC(i,j,kLev,bi,bj)
       ENDDO
      ENDDO
#endif

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_SPONGE))
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_T(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
      ENDIF
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
C     number of surface interface layer
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
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_S(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_AIM */

C AMM
#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_S(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myCurrentTime, myThid )
#endif /* ALLOW_FIZHI */
C AMM

C     Add fresh-water in top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &     +surfaceForcingS(i,j,bi,bj)
     &     *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#if (defined (ALLOW_OBCS) && defined (ALLOW_OBCS_SPONGE))
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_S(
     I           iMin, iMax, jMin, jMax,bi,bj,kLev,
     I           myCurrentTime,myThid)
      ENDIF
#endif

      RETURN
      END
