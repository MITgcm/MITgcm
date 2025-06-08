#include "PROFILES_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif
#ifdef ALLOW_ECCO
# include "ECCO_OPTIONS.h"
#endif
#ifdef ALLOW_BLING
# include "BLING_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C !ROUTINE: PROFILES_INTERP

C !INTERFACE:
      SUBROUTINE PROFILES_INTERP(
     O                            traj_cur_out,
     I                            i_cur, j_cur,
     I                            weights_cur,
     I                            var_cur,
     I                            itr_cur,
     I                            file_cur,
     I                            myTime,
     I                            bi, bj,
     I                            myThid )

C     !DESCRIPTION:
C     3D interpolation of model counterparts for netcdf profiles data

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_ECCO
# include "ECCO_SIZE.h"
# include "ECCO.h"
#endif
#ifdef ALLOW_PROFILES
# include "PROFILES_SIZE.h"
# include "profiles.h"
#endif
#ifdef ALLOW_PTRACERS
# include "PTRACERS_SIZE.h"
# include "PTRACERS_FIELDS.h"
#endif
#ifdef ALLOW_BLING
# include "BLING_VARS.h"
#endif

C     !INPUT PARAMETERS:
C     i/j_cur     :: i/j - index arrays of model grid points
C     weights_cur :: weights array for profile data
C     var_cur     :: current variable name
C     itr_cur     :: index of current variable
C     file_cur    :: current filenumber
C     myTime      :: time counter for this thread
C     bi,bj       :: Tile indices
C     myThid      :: my thread ID number
#ifdef ALLOW_PROFILES
      INTEGER i_cur(NUM_INTERP_POINTS)
      INTEGER j_cur(NUM_INTERP_POINTS)
      _RL weights_cur(NUM_INTERP_POINTS)
#else
      INTEGER i_cur, j_cur
      _RL weights_cur
#endif
      CHARACTER*(8) var_cur
      INTEGER itr_cur
      INTEGER file_cur
      _RL  myTime
      INTEGER bi, bj
      INTEGER myThid

C     !OUTPUT PARAMETERS:
C     traj_cur_out :: profile of interpolated model data
#ifdef ALLOW_PROFILES
      _RL traj_cur_out(NLEVELMAX)
#else
      _RL traj_cur_out
#endif
CEOP

C     !FUNCTIONS:
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK

C     !LOCAL VARIABLES:
#ifdef ALLOW_PROFILES
      INTEGER kProf, kLev
      INTEGER q, k, kcur, ierr
      _RL tab_coeffs1(NUM_INTERP_POINTS)
      _RL tab_coeffs3(NUM_INTERP_POINTS)
      _RL ponderations(NUM_INTERP_POINTS), pondsSUM
      _RL traj_cur(Nr), mask_cur(Nr)
      _RL tmp_coeff

      ierr = 0
C Horizontal interpolation:
      DO k = 1, Nr
        pondsSUM = 0. _d 0
        DO q = 1, NUM_INTERP_POINTS
          IF (var_cur.EQ.'theta') THEN
            tab_coeffs1(q) = theta(i_cur(q),j_cur(q),k,bi,bj)
          ELSEIF (var_cur.EQ.'salt') THEN
            tab_coeffs1(q) = salt(i_cur(q),j_cur(q),k,bi,bj)
          ELSEIF (var_cur.EQ.'pTracer') THEN
# ifdef ALLOW_PTRACERS
            tab_coeffs1(q) =
     &       pTracer(i_cur(q),j_cur(q),k,bi,bj,itr_cur)
# else
            tab_coeffs1(q) = 0. _d 0
# endif
# ifdef ALLOW_BLING
          ELSEIF (var_cur.EQ.'PCO') THEN
            tab_coeffs1(q) = pCO2(i_cur(q),j_cur(q),bi,bj)
          ELSEIF (var_cur.EQ.'PH') THEN
            tab_coeffs1(q) = pH(i_cur(q),j_cur(q),k,bi,bj)
          ELSEIF (var_cur.EQ.'CHL') THEN
            tab_coeffs1(q) = CHL(i_cur(q),j_cur(q),k,bi,bj)
          ELSEIF (var_cur.EQ.'POC') THEN
            tab_coeffs1(q) = POC(i_cur(q),j_cur(q),k,bi,bj)
# endif
# ifdef ALLOW_ECCO
          ELSEIF (var_cur.EQ.'eta') THEN
            tab_coeffs1(q) = m_eta(i_cur(q),j_cur(q),bi,bj)
# endif
          ELSEIF (var_cur.EQ.'UE') THEN
            tab_coeffs1(q) = m_UE(i_cur(q),j_cur(q),k,bi,bj)
          ELSEIF (var_cur.EQ.'VN') THEN
            tab_coeffs1(q) = m_VN(i_cur(q),j_cur(q),k,bi,bj)
          ELSE
            tab_coeffs1(q) = 0. _d 0
          ENDIF !IF (var_cur.EQ.

          tab_coeffs3(q) = maskC(i_cur(q),j_cur(q),k,bi,bj)

          ponderations(q) = tab_coeffs3(q)*weights_cur(q)
          pondsSUM = pondsSUM+ponderations(q)

        ENDDO !DO q

        IF (pondsSUM.GT.zeroRL) THEN
          traj_cur(k) = 0. _d 0
          mask_cur(k) = 1. _d 0
          pondsSUM = 1. _d 0 / pondsSUM

          DO q = 1, NUM_INTERP_POINTS
            traj_cur(k)=traj_cur(k)
     &       +tab_coeffs1(q)*ponderations(q)*pondsSUM
          ENDDO

        ELSE
          traj_cur(k) = 0. _d 0
          mask_cur(k) = 0. _d 0

        ENDIF !IF (pondsSUM.GT.zeroRL)

      ENDDO !DO k

# ifdef ALLOW_AUTODIFF_TAMC
C This is necessary, because TAF does not recognise prof_mask1D_cur
C as active and does not include it in the TL-version of this
C routine leading to prof_mask1D_cur=0 and also to
C objf_profiles_tl=0.
C$TAF INCOMPLETE prof_mask1D_cur

# endif
C Vertical interpolation:
      DO kLev = 1, NLEVELMAX
        traj_cur_out(kLev) = 0. _d 0
        prof_mask1D_cur(kLev,bi,bj) = 0. _d 0
      ENDDO

      DO kProf = 1, ProfDepthNo(file_cur,bi,bj)
C Case 1: above first grid center => first grid center value
        IF (prof_depth(file_cur,kProf,bi,bj).LT.-rC(1)) THEN
          traj_cur_out(kProf) = traj_cur(1)
          prof_mask1D_cur(kProf,bi,bj) = mask_cur(1)

C Case 2: just below last grid center => last cell value
        ELSEIF (prof_depth(file_cur,kProf,bi,bj).GE.-rC(Nr)) THEN
          IF ( prof_depth(file_cur,kProf,bi,bj).LT.
     &         (-rC(Nr)+drC(Nr)*halfRS) ) THEN
            traj_cur_out(kProf) = traj_cur(Nr)
            prof_mask1D_cur(kProf,bi,bj) = mask_cur(Nr)

          ENDIF

C Case 3: between two grid centers
        ELSE
          kcur = 0
          DO k = 1, Nr-1
            IF ( (prof_depth(file_cur,kProf,bi,bj).GE.-rC(k)).AND.
     &           (prof_depth(file_cur,kProf,bi,bj).LT.-rC(k+1))) THEN
              kcur = k
            ENDIF
          ENDDO

          IF (kcur.EQ.0) THEN
            WRITE(errorMessageUnit,'(A)')
     &       'ERROR in PROFILES_INTERP: unexpected case 1'

            ierr = ierr + 1

          ENDIF

          IF (mask_cur(kcur+1).EQ.oneRL) THEN
C Subcase 1: 2 wet points => linear interpolation
            tmp_coeff = (prof_depth(file_cur,kProf,bi,bj)+rC(kcur))/
     &       (-rC(kcur+1)+rC(kcur))

            traj_cur_out(kProf) = (1-tmp_coeff)*traj_cur(kcur)
     &       +tmp_coeff*traj_cur(kcur+1)

            prof_mask1D_cur(kProf,bi,bj) = 1. _d 0

            IF (mask_cur(kcur).EQ.zeroRL) THEN
              WRITE(errorMessageUnit,'(A)')
     &         'ERROR in PROFILES_INTERP: unexpected case 2'

              ierr = ierr + 1

            ENDIF

          ELSEIF (prof_depth(file_cur,kProf,bi,bj).LT.-rF(kcur+1))
     &    THEN
C Subcase 2: only 1 wet point just above => upper cell value
            traj_cur_out(kProf) = traj_cur(kcur)
            prof_mask1D_cur(kProf,bi,bj) = mask_cur(kcur)

          ENDIF !IF (mask_cur(kcur+1).EQ.oneRL)

        ENDIF !IF (prof_depth(file_cur,kProf,bi,bj).LT.-rC(1))

      ENDDO !DO kProf

      IF (ierr.GT.0) STOP 'ABNORMAL END: S/R PROFILES_INTERP'

#endif /* ALLOW_PROFILES */

      RETURN
      END
