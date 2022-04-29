#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: ADVECT
C     !INTERFACE:
      SUBROUTINE ADVECT(
     I                   UI, VI,
     U                   fld,
     O                   fldNm1,
     I                   iceMsk, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R ADVECT
C     | o Calculate advection and diffusion
C     |   and update the input ice-field
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     UI, VI     :: ice velocity components
C     fld        :: input and updated ice-field
C     fldNm1     :: copy of the input ice-field
C     iceMsk     :: Ocean/Land mask
C     myThid     :: my Thread Id. number
      _RL UI     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL VI     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fldNm1 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL iceMsk (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     i,j,k,bi,bj :: Loop counters
      INTEGER i, j, bi, bj
      INTEGER k
      _RL DELTT
      _RL DIFFA  (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tmpFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL afx    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)

      DELTT=SEAICE_deltaTtherm
C     save fld from previous time step
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          fldNm1(i,j,bi,bj) = fld(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#ifdef ALLOW_AUTODIFF_TAMC
CADJ INIT loctape_advect = COMMON, 2
#endif
      DO k=1,2
#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE fld = loctape_advect, key = k
#endif
C     Backward Euler correction step
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            tmpFld(i,j,bi,bj)=HALF*(fld(i,j,bi,bj)
     &           +fldNm1(i,j,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO

#ifdef ALLOW_AUTODIFF
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         afx(i,j) = 0. _d 0
         afy(i,j) = 0. _d 0
        ENDDO
       ENDDO
#endif /* ALLOW_AUTODIFF */

C NOW GO THROUGH STANDARD CONSERVATIVE ADVECTION
       IF ( .NOT. SEAICEuseFluxForm ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=0,sNy+1
           DO i=0,sNx+1
CML   This formulation gives the same result as the original code on a
CML   lat-lon-grid, but may not be accurate on irregular grids
            fld(i,j,bi,bj)=fldNm1(i,j,bi,bj)
     &           -DELTT*(
     &           ( tmpFld(i  ,j  ,bi,bj)+tmpFld(i+1,j  ,bi,bj))
     &           *   UI(i+1,j,  bi,bj) -
     &           ( tmpFld(i  ,j  ,bi,bj)+tmpFld(i-1,j  ,bi,bj))
     &           *   UI(i  ,j,  bi,bj) )*maskInC(i,j,bi,bj)
     &           *(HALF * _recip_dxF(i,j,bi,bj))
     &           -DELTT*(
     &           ( tmpFld(i  ,j  ,bi,bj)+tmpFld(i  ,j+1,bi,bj))
     &           *   VI(i  ,j+1,  bi,bj)
     &           * _dxG(i  ,j+1,bi,bj) -
     &           ( tmpFld(i  ,j  ,bi,bj)+tmpFld(i  ,j-1,bi,bj))
     &           *   VI(i  ,j  ,  bi,bj)
     &           * _dxG(i,j,bi,bj))*maskInC(i,j,bi,bj)
     &           *(HALF * _recip_dyF(i,j,bi,bj) * _recip_dxF(i,j,bi,bj))
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ELSE
C--   Use flux form for MITgcm compliance, unfortunately changes results
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
C--   first compute fluxes across cell faces
          DO j=1,sNy+1
           DO i=1,sNx+1
            afx(i,j) = _dyG(i,j,bi,bj) * UI(i,j,bi,bj)
     &           * 0.5 _d 0 * (tmpFld(i,j,bi,bj)+tmpFld(i-1,j,bi,bj))
            afy(i,j) = _dxG(i,j,bi,bj) * VI(i,j,bi,bj)
     &           * 0.5 _d 0 * (tmpFld(i,j,bi,bj)+tmpFld(i,j-1,bi,bj))
           ENDDO
          ENDDO
          DO j=1,sNy
           DO i=1,sNx
            fld(i,j,bi,bj)=fldNm1(i,j,bi,bj)
     &           -DELTT * (
     &             afx(i+1,j) - afx(i,j)
     &           + afy(i,j+1) - afy(i,j)
     &           )*recip_rA(i,j,bi,bj)*maskInC(i,j,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDIF

       CALL EXCH_XY_RL( fld, myThid )

      ENDDO

      IF ( DIFF1.GT.0. _d 0 ) THEN
C NOW DO DIFFUSION

C     make a working copy of field from last time step
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           tmpFld(i,j,bi,bj) = fldNm1(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

C NOW CALCULATE DIFFUSION COEF ROUGHLY
C  1rst pass: compute changes due to harmonic diffusion and add it to ice-field
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            DIFFA(i,j,bi,bj) = MIN( _dxF(i,j,bi,bj), _dyF(i,j,bi,bj) )
           ENDDO
          ENDDO
        ENDDO
       ENDDO
C-     Compute laplacian of ice-field; return result in same array
       CALL DIFFUS( tmpFld, DIFFA, iceMsk, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fld(i,j,bi,bj) = ( fld(i,j,bi,bj)
     &                       +tmpFld(i,j,bi,bj)*DIFF1*DELTT
     &                      )*iceMsk(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

c     IF ( useBiHarmonic ) THEN
C  2nd  pass: compute changes due to biharmonic diffusion and add it to ice-field
       _EXCH_XY_RL( tmpFld, myThid )
C     use some strange quadratic form for the second time around
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
#ifdef ALLOW_AUTODIFF_TAMC
C      to avoid recomputations when there was a k loop; not needed anymore
c           DIFFA(i,j,bi,bj) = MIN( _dxF(i,j,bi,bj), _dyF(i,j,bi,bj) )
#endif
            DIFFA(i,j,bi,bj) = - DIFFA(i,j,bi,bj)*DIFFA(i,j,bi,bj)
           ENDDO
          ENDDO
        ENDDO
       ENDDO
C-     Compute bilaplacian (laplacian of laplacian); return result in same array
       CALL DIFFUS( tmpFld, DIFFA, iceMsk, myThid )

       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           fld(i,j,bi,bj) = ( fld(i,j,bi,bj)
     &                       +tmpFld(i,j,bi,bj)*DIFF1*DELTT
     &                      )*iceMsk(i,j,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
C--   end biharmonic block
c     ENDIF

C--   end DIFF1 > 0 block
      ENDIF

      RETURN
      END
