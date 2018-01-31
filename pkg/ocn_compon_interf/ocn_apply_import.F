#include "OCN_CPL_OPTIONS.h"

CBOP
C     !ROUTINE: OCN_APPLY_IMPORT
C     !INTERFACE:
      SUBROUTINE OCN_APPLY_IMPORT(
     I               apply2AllFields, myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE OCN_APPLY_IMPORT
C     | o Apply imported coupling data to forcing fields
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "CPL_PARAMS.h"
#include "SURFACE.h"
#include "FFIELDS.h"
#include "OCNCPL.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     apply2AllFields - flag: T= apply import to all coupling fields
C                 F= only forcing fields relevant for eta variations
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      LOGICAL apply2AllFields
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     !LOCAL VARIABLES:
C     === Local arrays ===
      INTEGER bi,bj,i,j
CEOP

C--  Use imported coupling data in place of input files data:
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)

C--     Aplly import to all coupling fields (standard way)
        IF ( apply2AllFields ) THEN

C-   Dynamical forcing
         IF ( useImportTau ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              fu(i,j,bi,bj) = tauX(i,j,bi,bj)
              fv(i,j,bi,bj) = tauY(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportSLP ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              pLoad(i,j,bi,bj) = atmSLPr(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              sIceLoad(i,j,bi,bj) = seaIceMass(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C-   Fresh-Water & Salinity forcing
         IF ( useImportFW ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              EmPmR(i,j,bi,bj) = FWFlux    (i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportFW ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              saltFlux(i,j,bi,bj)= iceSaltFlx(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C-   Heat Flux forcing
         IF ( useImportHFlx ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              Qnet(i,j,bi,bj)= HeatFlux  (i,j,bi,bj)
            ENDDO
           ENDDO
#ifdef SHORTWAVE_HEATING
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             Qsw(i,j,bi,bj) = qShortWave(i,j,bi,bj)
            ENDDO
           ENDDO
#endif
         ENDIF

#ifdef ALLOW_THSICE
         IF ( useImportThSIce .AND. useThSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             iceMask   (i,j,bi,bj) = sIceFrac_cpl (i,j,bi,bj)
             iceHeight (i,j,bi,bj) = sIceThick_cpl(i,j,bi,bj)
             snowHeight(i,j,bi,bj) = sIceSnowH_cpl(i,j,bi,bj)
             Qice1     (i,j,bi,bj) = sIceQ1_cpl   (i,j,bi,bj)
             Qice2     (i,j,bi,bj) = sIceQ2_cpl   (i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif /* ALLOW_THSICE */

C--     Apply only to forcings relevant for eta/surf.press variations
        ELSE

         IF ( useImportSIce .AND. useImportSLP ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi0surf(i,j,bi,bj) = atmSLPr(i,j,bi,bj)*recip_rhoConst
     &                 + gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ELSEIF ( useImportSIce ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)*recip_rhoConst
     &                 + gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ELSEIF ( useImportSLP ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phi0surf(i,j,bi,bj) = atmSLPr(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ENDIF

         IF ( useImportFW ) THEN
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              EmPmR(i,j,bi,bj) = FWFlux    (i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C--     end if apply2AllFields / else blocks
        ENDIF

C--  end bi,bj loop
       ENDDO
      ENDDO

C--  Fill in Halo region with valid values

#ifdef ATMOSPHERIC_LOADING
      IF ( useImportSLP . AND.
     &    ( ocn_cplExch_DIC .OR. apply2AllFields ) )
     &   _EXCH_XY_RS( pLoad,    myThid )
      IF ( apply2AllFields ) THEN
        IF ( useImportSIce ) _EXCH_XY_RS( sIceLoad, myThid )
      ELSEIF ( useImportSLP .OR. useImportSIce ) THEN
         _EXCH_XY_RS( phi0surf, myThid )
      ENDIF
#else
      IF ( useImportSLP . AND. ocn_cplExch_DIC )
     &   _EXCH_XY_RS( pLoad,    myThid )
#endif

      IF ( useImportFW  ) _EXCH_XY_RS( EmPmR, myThid )
      IF ( apply2AllFields ) THEN
        IF ( useImportTau  ) CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
        IF ( useImportFW   ) _EXCH_XY_RS( saltFlux, myThid )
        IF ( useImportHFlx ) _EXCH_XY_RS( Qnet , myThid )
#ifdef SHORTWAVE_HEATING
        IF ( useImportHFlx ) _EXCH_XY_RS( Qsw ,  myThid )
#endif
#ifdef ALLOW_THSICE
        IF ( useImportThSIce .AND. useThSIce ) THEN
          _EXCH_XY_RL( iceMask   , myThid )
          _EXCH_XY_RL( iceHeight , myThid )
          _EXCH_XY_RL( snowHeight, myThid )
          _EXCH_XY_RL( Qice1     , myThid )
          _EXCH_XY_RL( Qice2     , myThid )
        ENDIF
#endif /* ALLOW_THSICE */
      ENDIF

      RETURN
      END
