#include "OBSFIT_OPTIONS.h"
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: OBSFIT_INTERP

C     !INTERFACE: 
      SUBROUTINE OBSFIT_INTERP(
     O                          modval_cur_out,
     I                          i_cur,
     I                          j_cur,
     I                          k_cur,
     I                          weights_cur,
     I                          var_cur,
     I                          file_cur,
     I                          mytime,
     I                          bi,
     I                          bj,
     I                          myThid )

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
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      _RL      mytime
      INTEGER  myThid
      INTEGER  file_cur, var_cur
#ifndef ALLOW_OBSFIT
      _RL      modval_cur_out, weights_cur
      INTEGER  i_cur, j_cur, k_cur
#else
      _RL      modval_cur_out
      _RL      weights_cur(NUM_INTERP_PTS_OBS)
      INTEGER  i_cur(NUM_INTERP_PTS_OBS)
      INTEGER  j_cur(NUM_INTERP_PTS_OBS)
      INTEGER  k_cur(NUM_INTERP_PTS_OBS)
#endif
CEOP

#ifdef ALLOW_OBSFIT

C     !LOCAL VARIABLES:
      _RL      tab_coeffs1(NUM_INTERP_PTS_OBS)
      _RL      tab_coeffs3(NUM_INTERP_PTS_OBS)
      _RL      weighing_pts(NUM_INTERP_PTS_OBS),weighingSUM
      INTEGER  q,k,kk,kcur,bi,bj
      _RL      modval_cur,mask_cur
      _RL      tmp_coeff
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C     !FUNCTIONS:
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C Horizontal interpolation:
      weighingSUM=0. _d 0
       DO q=1,NUM_INTERP_PTS_OBS
        IF (var_cur.EQ.1) THEN
         tab_coeffs1(q)=theta(i_cur(q),j_cur(q),k_cur(q),bi,bj)
        ELSEIF (var_cur.EQ.2) THEN
         tab_coeffs1(q)=salt(i_cur(q),j_cur(q),k_cur(q),bi,bj)
        ELSEIF (var_cur.EQ.3) THEN
         tab_coeffs1(q)=uvel(i_cur(q),j_cur(q),k_cur(q),bi,bj)
        ELSEIF (var_cur.EQ.4) THEN
         tab_coeffs1(q)=vvel(i_cur(q),j_cur(q),k_cur(q),bi,bj)
        ELSEIF (var_cur.EQ.5) THEN
         tab_coeffs1(q)=etan(i_cur(q),j_cur(q),bi,bj)
        ELSE
         tab_coeffs1(q)=0. _d 0
        ENDIF
        tab_coeffs3(q)=maskC(i_cur(q),j_cur(q),k_cur(q),bi,bj)
        weighing_pts(q)=tab_coeffs3(q)*weights_cur(q)
        weighingSUM=weighingSUM+weighing_pts(q)
       ENDDO

       IF (weighingSUM.GT.0) THEN
        modval_cur=0. _d 0
        mask_cur=1. _d 0
       DO q=1,NUM_INTERP_PTS_OBS
        modval_cur=modval_cur
     &   +tab_coeffs1(q)*weighing_pts(q)/weighingSUM
       ENDDO
      ELSE
       modval_cur=0. _d 0
       mask_cur=0. _d 0
      ENDIF

      modval_cur_out = modval_cur
      sample_modmask(bi,bj) = mask_cur

#endif /* ALLOW_OBSFIT */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|