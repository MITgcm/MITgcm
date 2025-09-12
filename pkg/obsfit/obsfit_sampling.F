#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OBSFIT_SAMPLING

C     !INTERFACE: 
      SUBROUTINE OBSFIT_SAMPLING(
     O                            modval_cur_out,
     I                            i_cur,
     I                            j_cur,
     I                            k_cur,
     I                            weights_cur,
     I                            var_cur,
     I                            file_cur,
     I                            myTime,
     I                            bi,
     I                            bj,
     I                            myThid )

C     !DESCRIPTION:
C     ==================================================================
C     | 3D interpolation of model counterparts for ObsFit data   
C     ==================================================================

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "PARAMS.h"
#ifdef ALLOW_CAL
# include "cal.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_OBSFIT
# include "OBSFIT_SIZE.h"
# include "OBSFIT.h"
#endif
#ifdef ALLOW_PTRACERS
#include "PTRACERS_SIZE.h"
#include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_BLING
#include "BLING_VARS.h"
#endif

C     !INPUT PARAMETERS: 
C     i/j/k_cur   :: i/j/k - index arrays of model grid points
C     weights_cur :: weights array for profile data
C     var_cur     :: current variable name
C     file_cur    :: current filenumber
C     myTime      :: time counter for this thread
C     bi,bj       :: tile indices
C     myThid      :: my thread ID number
#ifdef ALLOW_OBSFIT
      INTEGER  i_cur(NUM_INTERP_PTS_OBS)
      INTEGER  j_cur(NUM_INTERP_PTS_OBS)
      INTEGER  k_cur(NUM_INTERP_PTS_OBS)
      _RL      weights_cur(NUM_INTERP_PTS_OBS)
#else
      INTEGER  i_cur, j_cur, k_cur
#endif
      _RL      myTime
      INTEGER  bi, bj
      INTEGER  myThid
      INTEGER  file_cur, var_cur

C     !OUTPUT PARAMETERS:
C     modval_cur_out :: interpolated model data
      _RL modval_cur_out
CEOP

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_OBSFIT
      INTEGER  iq, kcur
      _RL      tab_coeffs1(NUM_INTERP_PTS_OBS)
      _RL      tab_coeffs3(NUM_INTERP_PTS_OBS)
      _RL      weighing_pts(NUM_INTERP_PTS_OBS),weighingSUM
      _RL      modval_cur, mask_cur
      _RL      tmp_coeff

C This is where sample types are assigned to model variables
      weighingSUM = 0. _d 0
      DO iq = 1, NUM_INTERP_PTS_OBS
        IF ( var_cur.EQ.1 ) THEN
         tab_coeffs1(iq) = theta(i_cur(iq),j_cur(iq),k_cur(iq),bi,bj)
        ELSEIF ( var_cur.EQ.2 ) THEN
         tab_coeffs1(iq) = salt(i_cur(iq),j_cur(iq),k_cur(iq),bi,bj)
        ELSEIF ( var_cur.EQ.3 ) THEN
         tab_coeffs1(iq) = uvel(i_cur(iq),j_cur(iq),k_cur(iq),bi,bj)
        ELSEIF ( var_cur.EQ.4 ) THEN
         tab_coeffs1(iq) = vvel(i_cur(iq),j_cur(iq),k_cur(iq),bi,bj)
        ELSEIF ( var_cur.EQ.5 ) THEN
         tab_coeffs1(iq) = etan(i_cur(iq),j_cur(iq),bi,bj)
        ELSE
         tab_coeffs1(iq) = 0. _d 0
        ENDIF
        tab_coeffs3(iq) = maskC(i_cur(iq),j_cur(iq),k_cur(iq),bi,bj)
        weighing_pts(iq) = tab_coeffs3(iq)*weights_cur(iq)
        weighingSUM = weighingSUM+weighing_pts(iq)
      ENDDO

      IF ( weighingSUM.GT.0 ) THEN
        modval_cur = 0. _d 0
        mask_cur = 1. _d 0
        DO iq = 1, NUM_INTERP_PTS_OBS
          modval_cur = modval_cur
     &     +tab_coeffs1(iq)*weighing_pts(iq)/weighingSUM
        ENDDO
      ELSE
        modval_cur = 0. _d 0
        mask_cur = 0. _d 0
      ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
C     This is necessary, because TAF does not recognise sample_modmask
C     as active and does not include it in the TL-version of this
C     routine leading to sample_modmask=0 and also to
C     objf_obsfit_tl=0.
C$TAF INCOMPLETE sample_modmask
#endif

      modval_cur_out = modval_cur
      sample_modmask(bi,bj) = mask_cur

#endif /* ALLOW_OBSFIT */

      RETURN
      END

