C $Header: /u/gcmpack/MITgcm/pkg/ctrl/ctrl_map_ini_genarr.F,v 1.7 2014/09/19 17:47:59 gforget Exp $
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
#include "ctrl_dummy.h"
#include "optim.h"
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == routine arguments ==
      INTEGER myThid

C     !FUNCTIONS:
      INTEGER  ILNBLNk
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
C     == local variables ==
      integer bi,bj
      integer i,j,k
      integer jmin,jmax
      integer imin,imax
      integer il
      integer iarr

      logical doglobalread
      logical ladinit
      character*( 80)   fnamegeneric
      _RL     fac
CEOP

      jmin = 1
      jmax = sNy
      imin = 1
      imax = sNx

      doglobalread = .false.
      ladinit      = .false.
      fac = 1. _d 0

#ifdef ALLOW_GENARR2D_CONTROL
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr2D

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do j = jmin,jmax
            do i = imin,imax
              tmpfld2d(i,j,bi,bj) = 0. _d 0
              if (xx_genarr2d_file(iarr).EQ.'xxg_etan0') then
                 tmpfld2d(i,j,bi,bj) = etaN(i,j,bi,bj)
#ifdef ALLOW_BOTTOMDRAG_CONTROL
              elseif (xx_genarr2d_file(iarr).EQ.'xxg_bottomdrag') then
                 tmpfld2d(i,j,bi,bj) = bottomDragFld(i,j,bi,bj)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
              elseif (xx_genarr2d_file(iarr).EQ.'xxg_geothermal') then
                 tmpfld2d(i,j,bi,bj) = geothermalFlux(i,j,bi,bj)
#endif
              endif
            enddo
          enddo
        ENDDO
       ENDDO

       if (xx_genarr2d_weight(iarr).NE.' ') 
     &  call ctrl_map_ini_gen2D(xx_genarr2d_file(iarr),
     &  xx_genarr2d_weight(iarr), xx_genarr2d_dummy(iarr),
     &  xx_genarr2d_bounds(1,iarr), tmpfld2d, maskc,
     &  xx_genarr2d_numsmooth(iarr), mythid )

       _EXCH_XY_RL( tmpfld2d, mythid)

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do j = 1-Oly,sNy+Oly
            do i = 1-Olx,sNy+Olx
              if (xx_genarr2d_file(iarr).EQ.'xxg_etan0') then
                 tmpfld2d(i,j,bi,bj) = etaN(i,j,bi,bj)
#ifdef ALLOW_BOTTOMDRAG_CONTROL
              elseif (xx_genarr2d_file(iarr).EQ.'xxg_bottomdrag') then
                 bottomDragFld(i,j,bi,bj) = tmpfld2d(i,j,bi,bj)
#endif
#ifdef ALLOW_GEOTHERMAL_FLUX
              elseif (xx_genarr2d_file(iarr).EQ.'xxg_geothermal') then
                 geothermalFlux(i,j,bi,bj) = tmpfld2d(i,j,bi,bj)
#endif
              endif
            enddo
          enddo
        ENDDO
       ENDDO

C--   end iarr loop
      ENDDO

#endif /* ALLOW_GENARR2D_CONTROL */

#ifdef ALLOW_GENARR3D_CONTROL
C--   generic - user-defined control vars
      DO iarr = 1, maxCtrlArr3D

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do k = 1,Nr
           do j = jmin,jmax
            do i = imin,imax
              tmpfld3d(i,j,k,bi,bj) = 0. _d 0
              if (xx_genarr3d_file(iarr).EQ.'xxg_theta') then
                tmpfld3d(i,j,k,bi,bj) = theta(i,j,k,bi,bj)
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_salt') then
                tmpfld3d(i,j,k,bi,bj) = salt(i,j,k,bi,bj)
#ifdef ALLOW_KAPGM_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_kapgm') then
                tmpfld3d(i,j,k,bi,bj) = kapgm(i,j,k,bi,bj)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_kapredi') then
                tmpfld3d(i,j,k,bi,bj) = kapredi(i,j,k,bi,bj)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_diffkr') then
                tmpfld3d(i,j,k,bi,bj) = diffKr(i,j,k,bi,bj)
#endif
              endif
            enddo
           enddo
          enddo
        ENDDO
       ENDDO

       if (xx_genarr3d_weight(iarr).NE.' ')
     &  call ctrl_map_ini_gen3D(xx_genarr3d_file(iarr),
     &  xx_genarr3d_weight(iarr), xx_genarr3d_dummy(iarr),
     &  xx_genarr3d_bounds(1,iarr), tmpfld3d, maskc,
     &  xx_genarr3d_numsmooth(iarr), mythid )

       _EXCH_XYZ_RL( tmpfld3d, myThid )

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)
          do k = 1,Nr
           do j = 1-Oly,sNy+Oly
            do i = 1-Olx,sNy+Olx
              if (xx_genarr3d_file(iarr).EQ.'xxg_theta') then
                theta(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_salt') then
                salt(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
#ifdef ALLOW_KAPGM_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_kapgm') then
                kapgm(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
#endif
#ifdef ALLOW_KAPREDI_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_kapredi') then
                kapredi(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
#endif
#ifdef ALLOW_DIFFKR_CONTROL
              elseif (xx_genarr3d_file(iarr).EQ.'xxg_diffkr') then
                diffKr(i,j,k,bi,bj) = tmpfld3d(i,j,k,bi,bj)
#endif
              endif
            enddo
           enddo
          enddo
        ENDDO
       ENDDO

C--   end iarr loop
      ENDDO

#endif /* ALLOW_GENARR3D_CONTROL */

      RETURN
      END
