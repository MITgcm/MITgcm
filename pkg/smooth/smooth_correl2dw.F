#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      subroutine smooth_correl2Dw (
     U     fld_in,mask_in,xx_gen_file,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_correl2Dw
C     | o Routine that maps a 2D control field to physical units
C     |   by mutliplying it with 1/sqrt(weight)
C     |   after smooth_correl2D has been applied
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "SMOOTH.h"
#if (defined (ALLOW_CTRL) && defined (ECCO_CTRL_DEPRECATED))
# include "ctrl.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
#endif
#if (defined (ALLOW_ECCO) && defined (ECCO_CTRL_DEPRECATED))
# include "ecco_cost.h"
#endif

      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld_in (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      character*(MAX_LEN_FNAM) xx_gen_file
      integer myThid

#ifdef ECCO_CTRL_DEPRECATED

# if (defined ALLOW_CTRL) || (defined ALLOW_ECCO)

      integer i,j,bi,bj
      integer itlo,ithi
      integer jtlo,jthi
      _RL tmpW
      LOGICAL weightWasFound
#if (defined (ALLOW_GENARR2D_CONTROL) || defined (ALLOW_GENARR3D_CONTROL) || defined (ALLOW_GENTIM2D_CONTROL))
      INTEGER iarr
#endif

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)


      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO j = 1,sNy
         DO i = 1,sNx

        weightWasFound=.TRUE.

        if ( xx_gen_file .EQ. xx_hflux_file ) then
        tmpW=whflux(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_sflux_file ) then
        tmpW=wsflux(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_tauu_file ) then
        tmpW=wtauu(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_tauv_file ) then
        tmpW=wtauv(i,j,bi,bj)

        elseif ( xx_gen_file .EQ. xx_atemp_file ) then
        tmpW=watemp(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_aqh_file ) then
        tmpW=waqh(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_precip_file ) then
        tmpW=wprecip(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_snowprecip_file ) then
        tmpW=wsnowprecip(i,j,bi,bj)

        elseif ( xx_gen_file .EQ. xx_swflux_file ) then
        tmpW=wswflux(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_swdown_file ) then
        tmpW=wswdown(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_lwflux_file ) then
        tmpW=wlwflux(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_lwdown_file ) then
        tmpW=wlwdown(i,j,bi,bj)

        elseif ( xx_gen_file .EQ. xx_evap_file ) then
        tmpW=wevap(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_apressure_file ) then
        tmpW=wapressure(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_uwind_file ) then
        tmpW=wuwind(i,j,bi,bj)
        elseif ( xx_gen_file .EQ. xx_vwind_file ) then
        tmpW=wvwind(i,j,bi,bj)

        else
          tmpW=0.
          weightWasFound=.FALSE.
        endif

#ifdef ALLOW_CTRL
#ifdef ALLOW_GENTIM2D_CONTROL
      do iarr = 1, maxCtrlTim2D
        if ( xx_gen_file .EQ. xx_gentim2d_file(iarr) ) then
          tmpW=wgentim2d(i,j,bi,bj,iarr)
          weightWasFound=.TRUE.
        endif
      enddo
#endif
#endif

      if ((mask_in(i,j,bi,bj).NE.0.).AND.(tmpW.NE.0.)) then
      fld_in(i,j,bi,bj)=fld_in(i,j,bi,bj)/sqrt(tmpW)
      else
      fld_in(i,j,bi,bj)=fld_in(i,j,bi,bj)*0.
      endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL ( fld_in , myThid )

      if (.NOT.weightWasFound) WRITE(errorMessageUnit,'(2A)' )
     &       'WARNING: no weights found for ',xx_gen_file

#endif /* ALLOW_ECCO or ALLOW_CTRL */

#endif /* ECCO_CTRL_DEPRECATED */

      end
