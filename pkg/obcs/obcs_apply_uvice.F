C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_apply_uvice.F,v 1.14 2011/09/02 14:19:16 jmc Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

CBOP
C     !ROUTINE: OBCS_APPLY_UVICE
C     !INTERFACE:
      SUBROUTINE OBCS_APPLY_UVICE(
     U                          uFld, vFld,
     I                          myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | S/R OBCS_APPLY_UVICE
C     *==========================================================*

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_SEAICE.h"

C     !INPUT/OUTPUT PARAMETERS:
C    myThid   :: my Thread Id number
      _RL uFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

#if (defined (ALLOW_OBCS) && defined (ALLOW_SEAICE))

C     !LOCAL VARIABLES:
C     I,J,K,bi,bj :: Loop counters
      INTEGER I,J,K,bi,bj
CEOP

      K = 1
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

C     Set model variables to OB values on North/South Boundaries
# ifdef ALLOW_OBCS_NORTH
      IF ( tileHasOBN(bi,bj) ) THEN
      DO I=1-Olx,sNx+Olx
C     Northern boundary
       IF (OB_Jn(I,bi,bj).NE.0) THEN
#  ifdef OBCS_SEAICE_COMPUTE_UVICE
        uFld(I,OB_Jn(I,bi,bj)  ,bi,bj) =
     &       _maskW(I,OB_Jn(I,bi,bj),K,bi,bj) *
     &       uFld(I,OB_Jn(I,bi,bj)-1,bi,bj)
        vFld(I,OB_Jn(I,bi,bj)  ,bi,bj) =
     &       _maskS(I,OB_Jn(I,bi,bj),K,bi,bj) *
     &       vFld(I,OB_Jn(I,bi,bj)-1,bi,bj)
#  else /* OBCS_SEAICE_COMPUTE_UVICE */
#   ifdef OBCS_SEAICE_AVOID_CONVERGENCE
        vFld(I,OB_Jn(I,bi,bj),bi,bj) =
     &       max(OBNvice(I,bi,bj),vFld(I,OB_Jn(I,bi,bj),bi,bj)) *
     &       _maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
#   else /* OBCS_SEAICE_AVOID_CONVERGENCE */
        vFld(I,OB_Jn(I,bi,bj),bi,bj) = OBNvice(I,bi,bj) *
     &       _maskS(I,OB_Jn(I,bi,bj),K,bi,bj)
#   endif /* OBCS_SEAICE_AVOID_CONVERGENCE */
        uFld(I,OB_Jn(I,bi,bj),bi,bj) = OBNuice(I,bi,bj) *
     &       _maskW(I,OB_Jn(I,bi,bj),K,bi,bj)
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PERP
        if ( _maskS(I,OB_Jn(I,bi,bj)  ,K,bi,bj) .NE. 0. .AND.
     &       _maskS(I,OB_Jn(I,bi,bj)-2,K,bi,bj) .NE. 0. )
     &       vFld(I,OB_Jn(I,bi,bj)-1,bi,bj) = 0.5 _d 0 *
     &       ( vFld(I,OB_Jn(I,bi,bj)  ,bi,bj) +
     &       vFld(I,OB_Jn(I,bi,bj)-2,bi,bj) ) *
     &       _maskS(I,OB_Jn(I,bi,bj)-1,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PERP */
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PAR
        if ( _maskW(I,OB_Jn(I,bi,bj)  ,K,bi,bj) .NE. 0. .AND.
     &       _maskW(I,OB_Jn(I,bi,bj)-2,K,bi,bj) .NE. 0. )
     &       uFld(I,OB_Jn(I,bi,bj)-1,bi,bj) = 0.5 _d 0 *
     &       ( uFld(I,OB_Jn(I,bi,bj)  ,bi,bj) +
     &       uFld(I,OB_Jn(I,bi,bj)-2,bi,bj) ) *
     &       _maskW(I,OB_Jn(I,bi,bj)-1,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PAR */
#  endif /* OBCS_SEAICE_COMPUTE_UVICE */
       ENDIF
      ENDDO
      ENDIF
# endif /* ALLOW_OBCS_NORTH */

# ifdef ALLOW_OBCS_SOUTH
      IF ( tileHasOBS(bi,bj) ) THEN
      DO I=1-Olx,sNx+Olx
C     Southern boundary
       IF (OB_Js(I,bi,bj).NE.0) THEN
#  ifdef OBCS_SEAICE_COMPUTE_UVICE
        uFld(I,OB_Js(I,bi,bj)+1,bi,bj) =
     &         _maskW(I,OB_Js(I,bi,bj)+1,K,bi,bj) *
     &         uFld(I,OB_Js(I,bi,bj)+2,bi,bj)
        vFld(I,OB_Js(I,bi,bj)+1,bi,bj) =
     &         _maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj) *
     &         vFld(I,OB_Js(I,bi,bj)+2,bi,bj)
#  else /* OBCS_SEAICE_COMPUTE_UVICE */
#   ifdef OBCS_SEAICE_AVOID_CONVERGENCE
        vFld(I,OB_Js(I,bi,bj)+1,bi,bj)=
     &       min(OBSvice(I,bi,bj),vFld(I,OB_Js(I,bi,bj)+1,bi,bj))
     &       * _maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
#   else /* OBCS_SEAICE_AVOID_CONVERGENCE */
        vFld(I,OB_Js(I,bi,bj)+1,bi,bj)=OBSvice(I,bi,bj)
     &       * _maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj)
#   endif /* OBCS_SEAICE_AVOID_CONVERGENCE */
        uFld(I,OB_Js(I,bi,bj),bi,bj)=OBSuice(I,bi,bj)
     &       * _maskW(I,OB_Js(I,bi,bj),K,bi,bj)
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PERP
        if ( _maskS(I,OB_Js(I,bi,bj)+1,K,bi,bj) .NE. 0. .AND.
     &       _maskS(I,OB_Js(I,bi,bj)+3,K,bi,bj) .NE. 0. )
     &       vFld(I,OB_Js(I,bi,bj)+2,bi,bj) = 0.5 _d 0 *
     &       ( vFld(I,OB_Js(I,bi,bj)+1,bi,bj) +
     &       vFld(I,OB_Js(I,bi,bj)+3,bi,bj) ) *
     &       _maskS(I,OB_Js(I,bi,bj)+2,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PERP */
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PAR
        if ( _maskW(I,OB_Js(I,bi,bj)  ,K,bi,bj) .NE. 0. .AND.
     &       _maskW(I,OB_Js(I,bi,bj)+2,K,bi,bj) .NE. 0. )
     &       uFld(I,OB_Js(I,bi,bj)+1,bi,bj) = 0.5 _d 0 *
     &       ( uFld(I,OB_Js(I,bi,bj)  ,bi,bj) +
     &       uFld(I,OB_Js(I,bi,bj)+2,bi,bj) ) *
     &       _maskW(I,OB_Js(I,bi,bj)+1,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PAR */
#  endif /* OBCS_SEAICE_COMPUTE_UVICE */
       ENDIF
      ENDDO
      ENDIF
# endif /* ALLOW_OBCS_SOUTH */

C     Set model variables to OB values on East/West Boundaries
# ifdef ALLOW_OBCS_EAST
      IF ( tileHasOBE(bi,bj) ) THEN
      DO J=1-Oly,sNy+Oly
C     Eastern boundary
       IF (OB_Ie(J,bi,bj).NE.0) THEN
#  ifdef OBCS_SEAICE_COMPUTE_UVICE
        uFld(OB_Ie(J,bi,bj),J,bi,bj) =
     &         _maskW(OB_Ie(J,bi,bj),J,K,bi,bj) *
     &         uFld(OB_Ie(J,bi,bj)-1,J,bi,bj)
        vFld(OB_Ie(J,bi,bj),J,bi,bj) =
     &         _maskS(OB_Ie(J,bi,bj),J,K,bi,bj) *
     &         vFld(OB_Ie(J,bi,bj)-1,J,bi,bj)
#  else /* OBCS_SEAICE_COMPUTE_UVICE */
#   ifdef OBCS_SEAICE_AVOID_CONVERGENCE
        uFld(OB_Ie(J,bi,bj),J,bi,bj)=
     &       max(OBEuice(J,bi,bj),uFld(OB_Ie(J,bi,bj),J,bi,bj))
     &       * _maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
#   else /* OBCS_SEAICE_AVOID_CONVERGENCE */
        uFld(OB_Ie(J,bi,bj),J,bi,bj)=OBEuice(J,bi,bj)
     &       * _maskW(OB_Ie(J,bi,bj),J,K,bi,bj)
#   endif /* OBCS_SEAICE_AVOID_CONVERGENCE */
        vFld(OB_Ie(J,bi,bj),J,bi,bj)=OBEvice(J,bi,bj)
     &       * _maskS(OB_Ie(J,bi,bj),J,K,bi,bj)
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PERP
        if ( _maskW(OB_Ie(J,bi,bj)  ,J,K,bi,bj) .NE. 0. .AND.
     &       _maskW(OB_Ie(J,bi,bj)-2,J,K,bi,bj) .NE. 0. )
     &       uFld(OB_Ie(J,bi,bj)-1,J,bi,bj) = 0.5 _d 0 *
     &       ( uFld(OB_Ie(J,bi,bj)  ,J,bi,bj) +
     &       uFld(OB_Ie(J,bi,bj)-2,J,bi,bj) ) *
     &       _maskW(OB_Ie(J,bi,bj)-1,J,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PERP */
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PAR
        if ( _maskS(OB_Ie(J,bi,bj)  ,J,K,bi,bj) .NE. 0. .AND.
     &       _maskS(OB_Ie(J,bi,bj)-2,J,K,bi,bj) .NE. 0. )
     &       vFld(OB_Ie(J,bi,bj)-1,J,bi,bj) = 0.5 _d 0 *
     &       ( vFld(OB_Ie(J,bi,bj)  ,J,bi,bj) +
     &       vFld(OB_Ie(J,bi,bj)-2,J,bi,bj) ) *
     &       _maskS(OB_Ie(J,bi,bj)-1,J,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PAR */
#  endif /* OBCS_SEAICE_COMPUTE_UVICE */
       ENDIF
      ENDDO
      ENDIF
# endif /* ALLOW_OBCS_EAST */

# ifdef ALLOW_OBCS_WEST
      IF ( tileHasOBW(bi,bj) ) THEN
      DO J=1-Oly,sNy+Oly
C     Western boundary
       IF (OB_Iw(J,bi,bj).NE.0) THEN
#  ifdef OBCS_SEAICE_COMPUTE_UVICE
        uFld(OB_Iw(J,bi,bj)+1,J,bi,bj)=
     &         _maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj) *
     &         uFld(OB_Iw(J,bi,bj)+2,J,bi,bj)
        vFld(OB_Iw(J,bi,bj)+1,J,bi,bj)=
     &         _maskS(OB_Iw(J,bi,bj)+1,J,K,bi,bj) *
     &         vFld(OB_Iw(J,bi,bj)+2,J,bi,bj)
#  else /* OBCS_SEAICE_COMPUTE_UVICE */
#   ifdef OBCS_SEAICE_AVOID_CONVERGENCE
        uFld(OB_Iw(J,bi,bj)+1,J,bi,bj)=
     &       min(OBWuice(J,bi,bj),uFld(OB_Iw(J,bi,bj)+1,J,bi,bj))
     &       * _maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
#   else /* OBCS_SEAICE_AVOID_CONVERGENCE */
        uFld(OB_Iw(J,bi,bj)+1,J,bi,bj)=OBWuice(J,bi,bj)
     &       * _maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
#   endif /* OBCS_SEAICE_AVOID_CONVERGENCE */
        vFld(OB_Iw(J,bi,bj),J,bi,bj)=OBWvice(J,bi,bj)
     &       * _maskS(OB_Iw(J,bi,bj),J,K,bi,bj)
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PERP
        if ( _maskW(OB_Iw(J,bi,bj)+1,J,K,bi,bj) .NE. 0. .AND.
     &       _maskW(OB_Iw(J,bi,bj)+3,J,K,bi,bj) .NE. 0. )
     &       uFld(OB_Iw(J,bi,bj)+2,J,bi,bj) = 0.5 _d 0 *
     &       ( uFld(OB_Iw(J,bi,bj)+1,J,bi,bj) +
     &       uFld(OB_Iw(J,bi,bj)+3,J,bi,bj) ) *
     &       _maskW(OB_Iw(J,bi,bj)+2,J,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PERP */
#   ifdef OBCS_SEAICE_SMOOTH_UVICE_PAR
        if ( _maskS(OB_Iw(J,bi,bj)  ,J,K,bi,bj) .NE. 0. .AND.
     &       _maskS(OB_Iw(J,bi,bj)+2,J,K,bi,bj) .NE. 0. )
     &       vFld(OB_Iw(J,bi,bj)+1,J,bi,bj) = 0.5 _d 0 *
     &       ( vFld(OB_Iw(J,bi,bj)  ,J,bi,bj) +
     &       vFld(OB_Iw(J,bi,bj)+2,J,bi,bj) ) *
     &       _maskS(OB_Iw(J,bi,bj)+1,J,K,bi,bj)
#   endif /* OBCS_SEAICE_SMOOTH_UVICE_PAR */
#  endif /* OBCS_SEAICE_COMPUTE_UVICE */
       ENDIF
      ENDDO
      ENDIF
# endif /* ALLOW_OBCS_WEST */

       ENDDO
      ENDDO

      CALL EXCH_UV_XY_RL( uFld, vFld, .TRUE., myThid )

#endif /* defined (ALLOW_OBCS) && defined (ALLOW_SEAICE) */

      RETURN
      END
