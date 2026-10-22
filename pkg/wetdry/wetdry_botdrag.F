#include "WETDRY_OPTIONS.h"
#include "MOM_COMMON_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: WETDRY_BOTDRAG

C !INTERFACE:
      SUBROUTINE WETDRY_BOTDRAG( myTime, myThid )

C !DESCRIPTION:
C     Depth-dependent quadratic bottom drag (wdManningN > 0): the drag
C     coefficient of each face follows Manning's law,
C        Cd = g n^2 / D^(1/3),   limited to bottomDragQuadratic .. wdDragMax,
C     with D the flow depth of the face as used by WETDRY_UPDATE_MASKS (the
C     higher of the two surfaces minus the face bed crest, at least
C     wdMinDepth). Thin water on drying flats gets the stronger drag of
C     shallow flow (n = 0.02 s m^-1/3: Cd = 0.0067 at 0.2 m, 0.0031 at
C     2 m; the constant bottomDragQuadratic takes over below the depth
C     where Manning's Cd falls under it). Written into bottomDragCoeffW/S,
C     which MOM_U/V_BOTDRAG_COEFF use in place of bottomDragQuadratic
C     with ALLOW_BOTTOMDRAG_ROUGHNESS; called with the face masks every
C     time step (MITgcm's own roughness option sets them once, from the
C     rest-state bottom cell).
C     With wdManningFile, n on a face is wdManningN plus the larger of
C     the two neighbours' extra roughness wdManEx, times a factor that is
C     1 until wdManningT1 and falls linearly to 0 at wdManningT2 (river
C     ice and ice jams that clear at breakup).

C !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "WETDRY.h"
#ifdef ALLOW_BOTTOMDRAG_ROUGHNESS
# include "MOM_VISC.h"
#endif

C !INPUT PARAMETERS:
      _RL     myTime
      INTEGER myThid
CEOP

#if (defined ALLOW_WETDRY) && (defined ALLOW_BOTTOMDRAG_ROUGHNESS)
C !LOCAL VARIABLES:
      INTEGER i, j, bi, bj
      _RL zbf, hf, cd, fac, rn

C-    time factor of the extra roughness
      fac = 1. _d 0
      IF ( wdManningT2.GT.wdManningT1 ) THEN
        fac = ( wdManningT2 - myTime )/( wdManningT2 - wdManningT1 )
        fac = MIN( 1. _d 0, MAX( 0. _d 0, fac ) )
      ENDIF
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx+1
          zbf = MAX( R_low(i-1,j,bi,bj), R_low(i,j,bi,bj) )
          hf  = MAX( Ro_surf(i-1,j,bi,bj) + etaH(i-1,j,bi,bj),
     &               Ro_surf(i,j,bi,bj)   + etaH(i,j,bi,bj) ) - zbf
          hf  = MAX( hf, wdMinDepth )
          rn  = wdManningN + fac*MAX( wdManEx(i-1,j,bi,bj),
     &                                 wdManEx(i,j,bi,bj) )
          cd  = gravity*rn*rn*hf**(-1. _d 0/3. _d 0)
          bottomDragCoeffW(i,j,bi,bj) =
     &         MIN( wdDragMax, MAX( bottomDragQuadratic, cd ) )
         ENDDO
        ENDDO
        DO j=1,sNy+1
         DO i=1,sNx
          zbf = MAX( R_low(i,j-1,bi,bj), R_low(i,j,bi,bj) )
          hf  = MAX( Ro_surf(i,j-1,bi,bj) + etaH(i,j-1,bi,bj),
     &               Ro_surf(i,j,bi,bj)   + etaH(i,j,bi,bj) ) - zbf
          hf  = MAX( hf, wdMinDepth )
          rn  = wdManningN + fac*MAX( wdManEx(i,j-1,bi,bj),
     &                                 wdManEx(i,j,bi,bj) )
          cd  = gravity*rn*rn*hf**(-1. _d 0/3. _d 0)
          bottomDragCoeffS(i,j,bi,bj) =
     &         MIN( wdDragMax, MAX( bottomDragQuadratic, cd ) )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      CALL EXCH_UV_XY_RS( bottomDragCoeffW, bottomDragCoeffS, .FALSE.,
     &                    myThid )
#endif /* ALLOW_WETDRY and ALLOW_BOTTOMDRAG_ROUGHNESS */

      RETURN
      END
