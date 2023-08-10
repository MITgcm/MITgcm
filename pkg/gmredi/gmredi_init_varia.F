#include "GMREDI_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: GMREDI_INIT_VARIA
C     !INTERFACE:
      SUBROUTINE GMREDI_INIT_VARIA( myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_INIT_VARIA
C     | o Routine to initialize GM/Redi variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GMREDI.h"
#include "GMREDI_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     myThid ::  my Thread Id number
      INTEGER myThid
CEOP

#ifdef ALLOW_GMREDI
C     !LOCAL VARIABLES:
      INTEGER i,j,k,bi,bj

C--   Initialise again 3-D control variables (AD-compiler need to see this)
#if (defined ALLOW_KAPREDI_CONTROL && defined GM_READ_K3D_REDI )
C--   Initialize 3-D Isopycnal diffusivity in common block
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GM_inpK3dRedi(i,j,k,bi,bj) = GM_isopycK
     &                  *GM_isoFac1d(k)*GM_isoFac2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_KAPREDI_CONTROL and GM_READ_K3D_REDI */
#if (defined ALLOW_KAPGM_CONTROL && defined GM_READ_K3D_GM )
C--   Initialize 3-D Thickness diffusivity in common block
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           GM_inpK3dGM(i,j,k,bi,bj) = GM_background_K
     &                  *GM_bolFac1d(k)*GM_bolFac2d(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_KAPGM_CONTROL and GM_READ_K3D_GM */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#ifdef ALLOW_AUTODIFF
C- Note: To simplify dependency & avoid recomputations, when compiling
C        pkg/autodiff, we always call GMREDI_INIT_VARIA to initialise control
C        variables (as done above) without condition on useGMRedi.
C        Therefore, in this case, the "If useGMRedi" is added back here:
      IF ( useGMRedi ) THEN
#endif

#if (defined ALLOW_KAPREDI_CONTROL && defined GM_READ_K3D_REDI )
C--   Read (again) 3-D Isopycnal diffusivity from file
      IF ( GM_K3dRediFile .NE. ' ' ) THEN
       CALL READ_FLD_XYZ_RL( GM_K3dRediFile, ' ',
     &                       GM_inpK3dRedi, 0, myThid )
       CALL EXCH_XYZ_RL( GM_inpK3dRedi, myThid )
      ENDIF
#endif /* ALLOW_KAPREDI_CONTROL and GM_READ_K3D_REDI */
#if (defined ALLOW_KAPGM_CONTROL && defined GM_READ_K3D_GM )
C--   Read (again) 3-D Thickness diffusivity from file
      IF ( GM_K3dGMFile .NE. ' ' ) THEN
       CALL READ_FLD_XYZ_RL( GM_K3dGMFile, ' ',
     &                       GM_inpK3dGM, 0, myThid )
       CALL EXCH_XYZ_RL( GM_inpK3dGM, myThid )
      ENDIF
#endif /* ALLOW_KAPGM_CONTROL and GM_READ_K3D_GM */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C     Initialize arrays in common blocks :
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           Kwx(i,j,k,bi,bj) = 0. _d 0
           Kwy(i,j,k,bi,bj) = 0. _d 0
           Kwz(i,j,k,bi,bj) = 0. _d 0
           Kux(i,j,k,bi,bj) = 0. _d 0
           Kvy(i,j,k,bi,bj) = 0. _d 0
#ifdef GM_EXTRA_DIAGONAL
           Kuz(i,j,k,bi,bj) = 0. _d 0
           Kvz(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_BOLUS_ADVEC
           GM_PsiX(i,j,k,bi,bj) = 0. _d 0
           GM_PsiY(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_VISBECK_VARIABLE_K
           VisbeckK(i,j,bi,bj) = 0. _d 0
#endif
#ifdef ALLOW_GM_LEITH_QG
           GM_LeithQG_K(i,j,k,bi,bj) = 0. _d 0
#endif
#ifdef GM_BATES_K3D
           GM_BatesK3d(i,j,k,bi,bj) = 0. _d 0
#endif
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_TIMEAVE
C     Initialize averages to zero
        CALL TIMEAVE_RESET(GM_Kwx_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwy_T,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_Kwz_T,Nr, bi,bj,myThid)
        GM_timeAve(bi,bj) = 0. _d 0
#ifdef GM_VISBECK_VARIABLE_K
        CALL TIMEAVE_RESET(Visbeck_K_T, 1, bi,bj,myThid)
#endif
#ifdef GM_BOLUS_ADVEC
        CALL TIMEAVE_RESET(GM_PsiXtave,Nr, bi,bj,myThid)
        CALL TIMEAVE_RESET(GM_PsiYtave,Nr, bi,bj,myThid)
#endif
#endif /* ALLOW_TIMEAVE */

C- end bi,bj loops
       ENDDO
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C--   write GM scaling factors to file:
      IF ( GM_iso1dFile .NE. ' ' ) THEN
        CALL WRITE_GLVEC_RS( 'GM_isoFac1d', ' ', GM_isoFac1d,
     I                        Nr, -1, myThid )
      ENDIF
      IF ( GM_bol1dFile .NE. ' ' ) THEN
        CALL WRITE_GLVEC_RS( 'GM_bolFac1d', ' ', GM_bolFac1d,
     I                        Nr, -1, myThid )
      ENDIF
      IF ( GM_iso2dFile .NE. ' ' ) THEN
        CALL WRITE_FLD_XY_RS( 'GM_isoFac2d',' ',GM_isoFac2d,-1,myThid )
      ENDIF
      IF ( GM_bol2dFile .NE. ' ' ) THEN
        CALL WRITE_FLD_XY_RS( 'GM_bolFac2d',' ',GM_bolFac2d,-1,myThid )
      ENDIF

#ifdef GM_BATES_K3D
      IF (.NOT.( startTime.EQ.baseTime .AND. nIter0.EQ.0
     &     .AND. pickupSuff.EQ.' ' )) THEN
        IF (GM_useBatesK3d) CALL GMREDI_READ_PICKUP( nIter0, myThid )
      ENDIF
#endif

#ifdef GM_BATES_K3D
C This is put here, but really should be in gmredi_init_fixed.F. The problem is that
C fCori, fCoriCos, etc are not initialized when gmredi_init_fixed.F is called. To be fixed.
C     Computing beta = df/dy
      IF ( selectCoriMap.EQ.1 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gradf(i,j,bi,bj) =  beta
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSEIF ( selectCoriMap.EQ.2 ) THEN
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           gradf(i,j,bi,bj) = recip_rSphere*fCoriCos(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ELSE
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-OLy+1,sNy+OLy-1
          DO i=1-OLx+1,sNx+OLx-1
           gradf(i,j,bi,bj)  =  .5 _d 0*angleSinC(i,j,bi,bj)*(
     &    (fCori(i+1,j,bi,bj)-fCori(i  ,j,bi,bj))*recip_dxC(i+1,j,bi,bj)
     &   +(fCori(i  ,j,bi,bj)-fCori(i-1,j,bi,bj))*recip_dxC(i,j,bi,bj) )
     &                       +  .5 _d 0*angleCosC(i,j,bi,bj)*(
     &    (fCori(i,j+1,bi,bj)-fCori(i,j  ,bi,bj))*recip_dyC(i,j+1,bi,bj)
     &   +(fCori(i,j  ,bi,bj)-fCori(i,j-1,bi,bj))*recip_dyC(i,j,bi,bj) )
           gradf(i,j,bi,bj)=max(1. _d -18, gradf(i,j,bi,bj) )
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDIF
      CALL EXCH_XY_RL( gradf, myThid)
#endif /* GM_BATES_K3D */

#ifdef ALLOW_AUTODIFF
      ENDIF
#endif
#endif /* ALLOW_GMREDI */

      RETURN
      END
