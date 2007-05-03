C $Header: /u/gcmpack/MITgcm/verification/tidal_basin_2d/code/external_forcing.F,v 1.6 2007/05/03 21:42:15 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: EXTERNAL_FORCING_U
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_U(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_U
C     | o Contains problem specific forcing for zonal velocity.
C     *==========================================================*
C     | Adds terms to gU for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
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
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
C     kSurface  :: index of surface layer
      INTEGER i, j
      INTEGER kSurface
      _RL tidal_freq,tidal_Hscale
      _RL Coord2longitude,longitud1,longitud2
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
     &                      myTime, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_U(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_FIZHI */

C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. kSurface ) THEN
c      DO j=1,sNy
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNy+1]
       DO j=0,sNy+1
        DO i=1,sNx+1
         gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceForcingU(i,j,bi,bj)
     &   *recip_drF(kLev)*recip_hFacW(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF


C--   Tidal body force: written as gradient of geopotential
C True M2 frequency is
c     tidal_freq=2.*pi/(43200.+25.*60.)
C But for convenience we are using 12 hour period
      tidal_freq=2.*pi/(43200.)
C Make the tide relatively strong (about 1 m)
      tidal_Hscale=10.
      IF ( usingCartesianGrid ) THEN
       Coord2longitude=1./rSphere
      ELSEIF ( usingSphericalPolarGrid ) THEN
       Coord2longitude=pi/180.
      ELSE
       STOP 'Be careful about 2D!'
      ENDIF
      DO j=0,sNy+1
       DO i=1,sNx+1
         longitud1=XC(i-1,j,bi,bj)*Coord2longitude
         longitud2=XC(i,j,bi,bj)*Coord2longitude
         gU(i,j,kLev,bi,bj) = gU(i,j,kLev,bi,bj)
     &   +gravity*tidal_Hscale*
     &    ( SIN( tidal_freq*myTime + 2.*longitud2 )
     &     -SIN( tidal_freq*myTime + 2.*longitud1 )
     &    )*recip_DXC(i,j,bi,bj)
     &   *_maskW(i,j,kLev,bi,bj)
c    &   *min( myTime/86400. , 1.)
       ENDDO
      ENDDO

#if (defined (ALLOW_TAU_EDDY))
       CALL TAUEDDY_EXTERNAL_FORCING_U(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_U(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
      ENDIF
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_V
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_V(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_V
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gV for forcing by external sources
C     | e.g. wind stress, bottom friction etc ...
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
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

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
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_V(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_V(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_FIZHI */

C     Add windstress momentum impulse into the top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=1,sNy+1
c       DO i=1,sNx
C-jmc: Without CD-scheme, this is OK ; but with CD-scheme, needs to cover [0:sNx+1]
        DO i=0,sNx+1
         gV(i,j,kLev,bi,bj) = gV(i,j,kLev,bi,bj)
     &   +foFacMom*surfaceForcingV(i,j,bi,bj)
     &   *recip_drF(kLev)*recip_hFacS(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#if (defined (ALLOW_TAU_EDDY))
       CALL TAUEDDY_EXTERNAL_FORCING_V(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_V(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
      ENDIF
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_T
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_T(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_T
C     | o Contains problem specific forcing for temperature.
C     *==========================================================*
C     | Adds terms to gT for forcing by external sources
C     | e.g. heat flux, climatalogical relaxation, etc ...
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
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j       :: Loop counters
C     kSurface  :: index of surface layer
      INTEGER i, j
      INTEGER kSurface
CEOP
#ifdef SHORTWAVE_HEATING
      integer two
      _RL minusone
      parameter (two=2,minusone=-1.)
      _RL swfracb(two)
      INTEGER kp1
#endif

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
     &                      myTime, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_T(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_FIZHI */

C     Add heat in top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=1,sNy
        DO i=1,sNx
         gT(i,j,kLev,bi,bj)=gT(i,j,kLev,bi,bj)
     &     +surfaceForcingT(i,j,bi,bj)
     &     *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef SHORTWAVE_HEATING
C Penetrating SW radiation
c     IF ( usePenetratingSW ) THEN
       swfracb(1)=abs(rF(klev))
       swfracb(2)=abs(rF(klev+1))
       CALL SWFRAC(
     I             two, minusone,
     U             swfracb,
     I             myTime, 1, myThid )
       kp1 = klev+1
       IF (klev.EQ.Nr) THEN
        kp1 = klev
        swfracb(2)=0. _d 0
       ENDIF
       DO j=1,sNy
        DO i=1,sNx
         gT(i,j,klev,bi,bj) = gT(i,j,klev,bi,bj)
     &   -Qsw(i,j,bi,bj)*(swfracb(1)*maskC(i,j,klev,bi,bj)
     &                   -swfracb(2)*maskC(i,j,kp1, bi,bj))
     &    *recip_Cp*recip_rhoConst
     &    *recip_drF(klev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
c     ENDIF
#endif

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_T(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
      ENDIF
#endif

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: EXTERNAL_FORCING_S
C     !INTERFACE:
      SUBROUTINE EXTERNAL_FORCING_S(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R EXTERNAL_FORCING_S
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds terms to gS for forcing by external sources
C     | e.g. fresh-water flux, climatalogical relaxation, etc ...
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
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     bi,bj     :: Current tile indices
C     kLev      :: Current vertical level index
C     myTime    :: Current time in simulation
C     myThid    :: Thread Id number
      INTEGER iMin, iMax, jMin, jMax, kLev, bi, bj
      _RL myTime
      INTEGER myThid

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
#ifdef ALLOW_AIM
      IF ( useAIM ) CALL AIM_TENDENCY_APPLY_S(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_AIM */

#ifdef ALLOW_FIZHI
      IF ( useFIZHI ) CALL FIZHI_TENDENCY_APPLY_S(
     &                      iMin,iMax, jMin,jMax, bi,bj, kLev,
     &                      myTime, myThid )
#endif /* ALLOW_FIZHI */

C     Add fresh-water in top-layer
      IF ( kLev .EQ. kSurface ) THEN
       DO j=1,sNy
        DO i=1,sNx
         gS(i,j,kLev,bi,bj)=gS(i,j,kLev,bi,bj)
     &     +surfaceForcingS(i,j,bi,bj)
     &     *recip_drF(kLev)*recip_hFacC(i,j,kLev,bi,bj)
        ENDDO
       ENDDO
      ENDIF

#ifdef ALLOW_OBCS
      IF (useOBCS) THEN
       CALL OBCS_SPONGE_S(
     I           iMin,iMax, jMin,jMax, bi,bj, kLev,
     I           myTime, myThid )
      ENDIF
#endif

      RETURN
      END
