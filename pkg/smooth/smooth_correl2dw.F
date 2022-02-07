#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

      SUBROUTINE SMOOTH_CORREL2DW(
     &           fld_in, mask_in, xx_gen_file, myThid )

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

      _RL fld_in (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS mask_in(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      CHARACTER*(MAX_LEN_FNAM) xx_gen_file
      INTEGER myThid

#ifdef ECCO_CTRL_DEPRECATED

# if (defined ALLOW_CTRL) || (defined ALLOW_ECCO)

      INTEGER i,j,bi,bj
      _RL tmpW
      LOGICAL weightWasFound
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
#endif

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j = 1,sNy
         DO i = 1,sNx

          weightWasFound = .TRUE.

          IF ( xx_gen_file .EQ. xx_hflux_file ) THEN
           tmpW = whflux(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_sflux_file ) THEN
           tmpW = wsflux(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_tauu_file ) THEN
           tmpW = wtauu(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_tauv_file ) THEN
           tmpW = wtauv(i,j,bi,bj)

          ELSEIF ( xx_gen_file .EQ. xx_atemp_file ) THEN
           tmpW = watemp(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_aqh_file ) THEN
           tmpW = waqh(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_precip_file ) THEN
           tmpW = wprecip(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_snowprecip_file ) THEN
           tmpW = wsnowprecip(i,j,bi,bj)

          ELSEIF ( xx_gen_file .EQ. xx_swflux_file ) THEN
           tmpW = wswflux(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_swdown_file ) THEN
           tmpW = wswdown(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_lwflux_file ) THEN
           tmpW = wlwflux(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_lwdown_file ) THEN
           tmpW = wlwdown(i,j,bi,bj)

          ELSEIF ( xx_gen_file .EQ. xx_evap_file ) THEN
           tmpW = wevap(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_apressure_file ) THEN
           tmpW = wapressure(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_uwind_file ) THEN
           tmpW = wuwind(i,j,bi,bj)
          ELSEIF ( xx_gen_file .EQ. xx_vwind_file ) THEN
           tmpW = wvwind(i,j,bi,bj)

          ELSE
            tmpW = 0.
            weightWasFound=.FALSE.
          ENDIF

#ifdef ALLOW_CTRL
#ifdef ALLOW_GENTIM2D_CONTROL
          DO iarr = 1, maxCtrlTim2D
           IF ( xx_gen_file .EQ. xx_gentim2d_file(iarr) ) THEN
             tmpW = wgentim2d(i,j,bi,bj,iarr)
             weightWasFound = .TRUE.
           ENDIF
          ENDDO
#endif
#endif

          IF ( (mask_in(i,j,bi,bj).NE.0.) .AND. (tmpW.NE.0.) ) THEN
           fld_in(i,j,bi,bj) = fld_in(i,j,bi,bj)/SQRT(tmpW)
          ELSE
           fld_in(i,j,bi,bj) = fld_in(i,j,bi,bj)*0.
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL ( fld_in , myThid )

      IF (.NOT.weightWasFound) WRITE(errorMessageUnit,'(2A)' )
     &       'WARNING: no weights found for ', xx_gen_file

#endif /* ALLOW_ECCO or ALLOW_CTRL */

#endif /* ECCO_CTRL_DEPRECATED */

      RETURN
      END
