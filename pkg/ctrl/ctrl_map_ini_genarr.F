C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_genarr.F,v 1.9 2014/09/30 18:41:45 gforget Exp $
C $Name:  $

#include "CTRL_OPTIONS.h"

CBOP
C     !ROUTINE: CTRL_MAP_INI_GENARR
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_INI_GENARR( myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_INI_GENARR
C     | Add the generic arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_FIELDS.h"
#include "CTRL_GENARR.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == local variables ==
      integer iarr
      integer igen_etan0,igen_bdrag,igen_geoth
      integer igen_theta0, igen_salt0
      integer igen_kapgm, igen_kapredi, igen_diffkr
CEOP

#ifdef ALLOW_GENARR2D_CONTROL

C--   generic 2D control variables

      igen_etan0=0
      igen_bdrag=0
      igen_geoth=0
      DO iarr = 1, maxCtrlArr2D
      if (xx_genarr2d_file(iarr).EQ.'xxg_etan0') igen_etan0=iarr
      if (xx_genarr2d_file(iarr).EQ.'xxg_bottomdrag') igen_bdrag=iarr
      if (xx_genarr2d_file(iarr).EQ.'xxg_geothermal') igen_geoth=iarr
      ENDDO

      call ctrl_map_genarr2d(etaN,igen_etan0,myThid)
#ifdef ALLOW_BOTTOMDRAG_CONTROL
      call ctrl_map_genarr2d(bottomDragFld,igen_bdrag,myThid)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
      call ctrl_map_genarr2d(geothermalFlux,igen_geoth,myThid)
#endif

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL

C--   generic 3D control variables

      igen_theta0=0
      igen_salt0=0
      igen_kapgm=0
      igen_kapredi=0
      igen_diffkr=0
      DO iarr = 1, maxCtrlArr3D
      if (xx_genarr3d_file(iarr).EQ.'xx_theta') igen_theta0=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_salt') igen_salt0=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_kapgm') igen_kapgm=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_kapredi') igen_kapredi=iarr
      if (xx_genarr3d_file(iarr).EQ.'xx_diffkr') igen_diffkr=iarr
      ENDDO

      call ctrl_map_genarr3d(theta,igen_theta0,myThid)
      call ctrl_map_genarr3d(salt,igen_salt0,myThid)
#ifdef ALLOW_KAPGM_CONTROL
      call ctrl_map_genarr3d(kapgm,igen_kapgm,myThid)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
      call ctrl_map_genarr3d(kapredi,igen_kapredi,myThid)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
      call ctrl_map_genarr3d(diffkr,igen_diffkr,myThid)
#endif

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END

C---------------------------

C     !ROUTINE: CTRL_MAP_GENARR2D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR2D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR2D
C     | Add the generic arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_GENARR2D_CONTROL

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j
      integer jmin,jmax
      integer imin,imax
      _RL fldTmp  (1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = jmin,jmax
            DO i = imin,imax
                fldTmp(i,j,bi,bj) = fld(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO

       if ( (xx_genarr2d_weight(iarr).NE.' ').AND.(iarr.GT.0) )
     &  call ctrl_map_ini_gen2D(xx_genarr2d_file(iarr),
     &       xx_genarr2d_weight(iarr), xx_genarr2d_dummy(iarr),
     &       xx_genarr2d_bounds(1,iarr), fldTmp, maskc,
     &       xx_genarr2d_numsmooth(iarr), mythid )

       _EXCH_XY_RL( fldTmp, mythid)

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNy+Olx
                fld(i,j,bi,bj) = fldTmp(i,j,bi,bj)
            ENDDO
          ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_GENARR2D_CONTROL */

      RETURN
      END

C---------------------------

C     !ROUTINE: CTRL_MAP_GENARR3D
C     !INTERFACE:
      SUBROUTINE CTRL_MAP_GENARR3D( fld, iarr, myThid )

C     !DESCRIPTION: \bv
C     *=================================================================
C     | SUBROUTINE CTRL_MAP_GENARR3D
C     | Add the generic arrays of the
C     | control vector to the model state and update the tile halos.
C     | The control vector is defined in the header file "ctrl.h".
C     *=================================================================
C     \ev

C     !USES:
      IMPLICIT NONE

C     == global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"

#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "CTRL_GENARR.h"
#include "ctrl_dummy.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      _RL fld   (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
      INTEGER iarr
      INTEGER myThid

#ifdef ALLOW_GENARR3D_CONTROL

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j,k
      integer jmin,jmax
      integer imin,imax
      _RL fldTmp  (1-olx:snx+olx,1-oly:sny+oly,nr,nsx,nsy)
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = jmin,jmax
            DO i = imin,imax
              DO k = 1,nr
                fldTmp(i,j,k,bi,bj) = fld(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO

       if ( (xx_genarr3d_weight(iarr).NE.' ').AND.(iarr.GT.0) )
     &  call ctrl_map_ini_gen3D(xx_genarr3d_file(iarr),
     &       xx_genarr3d_weight(iarr), xx_genarr3d_dummy(iarr),
     &       xx_genarr3d_bounds(1,iarr), fldTmp, maskc,
     &       xx_genarr3d_numsmooth(iarr), mythid )

       _EXCH_XYZ_RL( fldTmp, mythid)

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          DO j = 1-Oly,sNy+Oly
            DO i = 1-Olx,sNy+Olx
              DO k = 1,nr
                fld(i,j,k,bi,bj) = fldTmp(i,j,k,bi,bj)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
       ENDDO

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END


