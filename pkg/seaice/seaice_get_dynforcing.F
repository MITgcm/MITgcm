#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_EXF
# include "EXF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_GET_DYNFORCING
C     !INTERFACE:
      SUBROUTINE SEAICE_GET_DYNFORCING(
     I     uIce, vIce, icFrac, SIMaskU, SIMaskV,
     O     taux, tauy,
     I     myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_GET_DYNFORCING
C     |   compute surface stress from atmopheric forcing fields
C     *==========================================================*
C     | started by Martin Losch, April 2007
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "FFIELDS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#ifdef HACK_FOR_GMAO_CPL
# include "SEAICE_LAYERS.h"
#endif
#ifdef ALLOW_EXF
# include "DYNVARS.h"
# include "EXF_FIELDS.h"
# include "EXF_PARAM.h"
#endif

#ifdef ALLOW_DIAGNOSTICS
C     !FUNCTIONS:
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON
#endif /* ALLOW_DIAGNOSTICS */

C     !INPUT/OUTPUT PARAMETERS:
C   uIce   (inp) :: zonal      ice velocity (input)
C   vIce   (inp) :: meridional ice velocity (input)
C   icFrac (inp) :: seaice fraction (input)
C   SImaskU(inp) :: mask at U-point
C   SImaskV(inp) :: mask at V-point
C   taux   (out) :: zonal      wind stress over ice at U point
C   tauy   (out) :: meridional wind stress over ice at V point
C   myTime (inp) :: current time in simulation
C   myIter (inp) :: iteration number in simulation
C   myThid (inp) :: my Thread Id. number
      _RL uIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIce   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL icFrac (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SImaskU(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL SImaskV(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL taux   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL tauy   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef SEAICE_CGRID
C     !LOCAL VARIABLES:
C     i,j,bi,bj :: Loop counters
C     ks        :: vertical index of surface layer
      INTEGER bi, bj, i, j
      INTEGER ks
      _RL  COSWIN
      _RS  SINWIN
C     CDAIR   :: local wind stress coefficient (used twice)
C     oceTauX :: wind-stress over open-ocean (on Arakawa A-grid), X direction
C     oceTauY :: wind-stress over open-ocean (on Arakawa A-grid), Y direction
      _RL CDAIR  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_EXF
      _RL AAA
      _RL uTmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTmp   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# ifndef SEAICE_EXTERNAL_FLUXES
      _RL U1, V1
      _RL oceTauX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oceTauY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
# endif
#endif /* ALLOW_EXF */
#ifdef ALLOW_DIAGNOSTICS
      _RL locVar (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL locFrac
#endif /* ALLOW_DIAGNOSTICS */

C--   surface level
      ks = 1
      IF ( usingPcoords ) ks = Nr
C--   introduce turning angle (default is zero)
      SINWIN=SIN(SEAICE_airTurnAngle*deg2rad)
      COSWIN=COS(SEAICE_airTurnAngle*deg2rad)

C--   NOW SET UP FORCING FIELDS

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_EXF
C--   Wind stress is computed on center of C-grid cell
C     and interpolated to U and V points later

#ifndef SEAICE_EXTERNAL_FLUXES
C--   First compute wind-stress over open ocean: this will results in
C     over-writing fu and fv that were computed or read-in by pkg/exf.
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          U1=UWIND(i,j,bi,bj)
          V1=VWIND(i,j,bi,bj)
          AAA=U1**2+V1**2
          IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
             AAA=SEAICE_EPS
          ELSE
             AAA=SQRT(AAA)
          ENDIF
          CDAIR(i,j)=SEAICE_rhoAir*OCEAN_drag
     &         *(2.70 _d 0+0.142 _d 0*AAA+0.0764 _d 0*AAA*AAA)
          oceTauX(i,j)=CDAIR(i,j)*
     &         (COSWIN*U1-SIGN(SINWIN, _fCori(i,j,bi,bj))*V1)
          oceTauY(i,j)=CDAIR(i,j)*
     &         (SIGN(SINWIN, _fCori(i,j,bi,bj))*U1+COSWIN*V1)
         ENDDO
        ENDDO
C--   Interpolate wind stress over open ocean (N/m^2)
C     from A-grid to U and V points of C-grid
        DO j=1-OLy+1,sNy+OLy
         DO i=1-OLx+1,sNx+OLx
          fu(i,j,bi,bj) = 0.5 _d 0*( oceTauX(i,j) + oceTauX(i-1,j) )
     &                            * SIMaskU(i,j,bi,bj)
          fv(i,j,bi,bj) = 0.5 _d 0*( oceTauY(i,j) + oceTauY(i,j-1) )
     &                            * SIMaskV(i,j,bi,bj)
         ENDDO
        ENDDO
#endif /* ndef SEAICE_EXTERNAL_FLUXES */

# ifdef SEAICE_EXTERNAL_FLUXES
        IF ( useEXF .AND. useAtmWind ) THEN
# else  /* SEAICE_EXTERNAL_FLUXES */
        IF ( useEXF ) THEN
# endif /* SEAICE_EXTERNAL_FLUXES */
C     make local copies of wind field
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           uTmp(i,j)=uwind(i,j,bi,bj)
           vTmp(i,j)=vwind(i,j,bi,bj)
          ENDDO
         ENDDO
         IF ( useRelativeWind ) THEN
C     undo subtraction of ocean velocities and subtract ice velocities
          DO j=1-OLy,sNy+OLy-1
           DO i=1-OLx,sNx+OLx-1
            uTmp(i,j)=uwind(i,j,bi,bj)
     &          - 0.5 _d 0 * (uIce(i,j,bi,bj)+uIce(i+1,j,bi,bj))
            vTmp(i,j)=vwind(i,j,bi,bj)
     &          - 0.5 _d 0 * (vIce(i,j,bi,bj)+vIce(i,j+1,bi,bj))
           ENDDO
          ENDDO
         ENDIF
C--   Now compute ice surface stress
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           AAA=uTmp(i,j)**2+vTmp(i,j)**2
           IF ( AAA .LE. SEAICE_EPS_SQ ) THEN
            AAA=SEAICE_EPS
           ELSE
            AAA=SQRT(AAA)
           ENDIF
           IF ( yC(i,j,bi,bj) .LT. ZERO ) THEN
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag_south*AAA
           ELSE
            CDAIR(i,j) = SEAICE_rhoAir*SEAICE_drag*AAA
           ENDIF
          ENDDO
         ENDDO
         DO j=1-OLy+1,sNy+OLy
          DO i=1-OLx+1,sNx+OLx
C     interpolate to U points
           taux(i,j,bi,bj)=0.5 _d 0 *
     &         (  CDAIR(i  ,j)*(
     &          COSWIN                            *uTmp(i  ,j)
     &          -SIGN(SINWIN, _fCori(i  ,j,bi,bj))*vTmp(i  ,j) )
     &          + CDAIR(i-1,j)*(
     &          COSWIN                            *uTmp(i-1,j)
     &          -SIGN(SINWIN, _fCori(i-1,j,bi,bj))*vTmp(i-1,j) )
     &         )*SIMaskU(i,j,bi,bj)
C     interpolate to V points
           tauy(i,j,bi,bj)=0.5 _d 0 *
     &         (  CDAIR(i,j  )*(
     &          SIGN(SINWIN, _fCori(i,j  ,bi,bj))*uTmp(i,j  )
     &          +COSWIN                          *vTmp(i,j  ) )
     &          + CDAIR(i,j-1)*(
     &          SIGN(SINWIN, _fCori(i,j-1,bi,bj))*uTmp(i,j-1)
     &          +COSWIN                          *vTmp(i,j-1) )
     &         )*SIMaskV(i,j,bi,bj)
          ENDDO
         ENDDO

        ELSE
#else  /* ALLOW_EXF */
        IF (.TRUE.) THEN
#endif /* ALLOW_EXF */

C--   Wind stress is available on U and V points, copy it to seaice variables.
#ifdef HACK_FOR_GMAO_CPL
         DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx
           taux(i,j,bi,bj) = SIwindTauX(i,j,bi,bj)*SIMaskU(i,j,bi,bj)
           tauy(i,j,bi,bj) = SIwindTauY(i,j,bi,bj)*SIMaskV(i,j,bi,bj)
          ENDDO
         ENDDO
#else /* HACK_FOR_GMAO_CPL */
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
C now ice surface stress
           IF ( yC(i,j,bi,bj) .LT. ZERO ) THEN
            CDAIR(i,j) = SEAICE_drag_south/OCEAN_drag
           ELSE
            CDAIR(i,j) = SEAICE_drag      /OCEAN_drag
           ENDIF
           taux(i,j,bi,bj) = CDAIR(i,j)*fu(i,j,bi,bj)
     &                     *SIMaskU(i,j,bi,bj)
           tauy(i,j,bi,bj) = CDAIR(i,j)*fv(i,j,bi,bj)
     &                     *SIMaskV(i,j,bi,bj)
          ENDDO
         ENDDO
#endif /* HACK_FOR_GMAO_CPL */

        ENDIF

C--   end bi,bj loops
       ENDDO
      ENDDO

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
       CALL DIAGNOSTICS_FILL( taux, 'SItaux  ', 0,1, 0,1,1, myThid )
       CALL DIAGNOSTICS_FILL( tauy, 'SItauy  ', 0,1, 0,1,1, myThid )
       IF ( DIAGNOSTICS_IS_ON( 'SIatmTx ', myThid ) ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=2-OLy,sNy+OLy
            DO i=2-OLx,sNx+OLx
              locFrac = ( icFrac( i, j,bi,bj)
     &                  + icFrac(i-1,j,bi,bj) )*halfRL
              locVar(i,j) = taux(i,j,bi,bj)*locFrac
     &                      + fu(i,j,bi,bj)*(oneRL-locFrac)
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(locVar,'SIatmTx ',0,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF
       IF ( DIAGNOSTICS_IS_ON( 'SIatmTy ', myThid ) ) THEN
        DO bj=myByLo(myThid),myByHi(myThid)
         DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=2-OLy,sNy+OLy
            DO i=2-OLx,sNx+OLx
              locFrac = ( icFrac(i, j, bi,bj)
     &                  + icFrac(i,j-1,bi,bj) )*halfRL
              locVar(i,j) = tauy(i,j,bi,bj)*locFrac
     &                      + fv(i,j,bi,bj)*(oneRL-locFrac)
            ENDDO
           ENDDO
           CALL DIAGNOSTICS_FILL(locVar,'SIatmTy ',0,1,2,bi,bj,myThid)
         ENDDO
        ENDDO
       ENDIF
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* SEAICE_CGRID */

      RETURN
      END
