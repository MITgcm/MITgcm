C $Header: /u/gcmpack/MITgcm/verification/rotating_tank/code/Attic/external_forcing.F,v 1.6 2007/05/03 21:42:15 jmc Exp $
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
C     iG, jG             :: Global index temps.
C     hC, hW, hE, hN, hS :: Fractional vertical distance open to fluid temps.
C     dFlux[WENS]        :: Diffusive flux normal to each cell face.
C     faceArea           :: Temp. for holding area normal to tempurature gradient.
      INTEGER iG, jG
      _RL hC, hW, hE, hN, hS
      _RL dFluxW, dFluxE, dFluxN, dFluxS
      _RL faceArea

C--   Forcing term
C     Add term which represents the diffusive flux from a circular cylinder of temperature tCylIm in the 
C     interior of the simulation domain. Result is a tendency which is determined from the finite
C     volume formulated divergence of the diffusive heat flux due to the local cylinder 
C     temperature, fluid temperature difference.
C     kDiffCyl :: Diffusion coefficient
C     tCylIn     :: Temperature of the inner boundary of the cylinder
C     tCylOut     :: Temperature of the outer boundary cylinder
C     iGSource :: Index space I (global) coordinate for source center.
C     jGSource :: Index space J (global) coordinate for source center.
C     rSource  :: Extent of the source term region. Loop will skip checking points outside
C              :: this region. Within this region the source heating will be added
C              :: to any points that are at a land - fluid boundary. rSource is in grid
C              :: points, so that points checked are ophi(iGlobal,jGlobal) such that
C              :: iGlobal=iGsource +/- rSource, jGlobal = jGsource +/- rSource.
C              :: rSource, iGSource and jGSource only need to define a quadrilateral that
C              :: includes the cylinder and no other land, they do not need to be exact.
c afe:
c  the below appears to be an attempt to make the heat flux somewhat general in regards
c  to bathymetry, but jmc pointed out some ways it could be better.  this is not
c  an issue at this point (July '04) since all experiments are being done with straight-
c  sided tank and cyclinder walls.
c  some todo items:
c  * add terms to top and bottom -- probably critical!!!
c  * make tCyl more flexible -- maybe have as separate inner and outer variables
c       or, eventually, a forcing field
c  * think about possible problems with differing heat diffusion rates in wall materials
c    (plexiglass, air, water in the compound tank case)
      _RL kDiffCyl
      _RL tCyl
      INTEGER rSource
      INTEGER iGSource
      INTEGER jGSource
      CHARACTER*(MAX_LEN_MBUF+1000) msgBuf

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

      kDiffCyl = 3. _d -7
      rSource  = 3
      iGSource = 30
      jGSource = 8
      DO j=1,sNy
       DO i=1,sNx
        dFluxW = 0.
        dFluxE = 0.
        dFluxN = 0.
        dFluxS = 0.
        jG = myYGlobalLo-1+(bj-1)*sNy+J
        iG = myXGlobalLo-1+(bi-1)*sNx+I
c      IF(jG .GE. jGSource-rSource .AND. jG .LE. jGSource+rSource) THEN
c     the following bites the big one
      IF(jG .LE. 10) THEN
         tCyl = tCylIn
      ELSE 
         tCyl = tCylOut
      ENDIF
c      IF(iG .GE. iGSource-rSource .AND. iG .LE. iGSource+rSource) THEN
          hC = hFacC(i  ,j  ,kLev,bi,bj)
          hW = hFacW(i  ,j  ,kLev,bi,bj)
          hE = hFacW(i+1,j  ,kLev,bi,bj)
          hN = hFacS(i  ,j+1,kLev,bi,bj)
          hS = hFacS(i  ,j  ,kLev,bi,bj)
          IF ( hC .NE. 0. .AND .hW .EQ. 0. ) THEN
C          Cylinder to west
           faceArea = drF(kLev)*dyG(i,j,bi,bj)
           dFluxW = 
     &      -faceArea*kDiffCyl*(theta(i,j,kLev,bi,bj) - tCyl)
     &      *recip_dxC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hE .EQ. 0. ) THEN
C          Cylinder to east
           faceArea = drF(kLev)*dyG(i+1,j,bi,bj)
           dFluxE =
     &      -faceArea*kDiffCyl*(tCyl - theta(i,j,kLev,bi,bj))
     &      *recip_dxC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hN .EQ. 0. ) THEN
C          Cylinder to north
           faceArea = drF(kLev)*dxG(i,j+1,bi,bj)
           dFluxN =
     &      -faceArea*kDiffCyl*(tCyl-theta(i,j,kLev,bi,bj))
     &      *recip_dyC(i,j,bi,bj)
          ENDIF
          IF ( hC .NE. 0. .AND .hS .EQ. 0. ) THEN
C          Cylinder to south
           faceArea = drF(kLev)*dxG(i,j,bi,bj)
           dFluxS =
     &      -faceArea*kDiffCyl*(theta(i,j,kLev,bi,bj) - tCyl)
     &      *recip_dyC(i,j,bi,bj)
          ENDIF
c      ENDIF
c      ENDIF
C       Net tendency term is minus flux divergence divided by the volume.
        gT(i,j,kLev,bi,bj) = gT(i,j,kLev,bi,bj)
     &  -_recip_hFacC(i,j,kLev,bi,bj)*recip_drF(kLev)
     &  *recip_rA(i,j,bi,bj)
     &  *(
     &    dFluxE-dFluxW
     &   +dFluxN-dFluxS
     &   )

       ENDDO
      ENDDO

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
