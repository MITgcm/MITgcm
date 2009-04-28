C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_apply_import.F,v 1.7 2009/04/28 18:44:21 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

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
#include "OCNIDS.h"
#include "OCNCPL.h"
#include "FFIELDS.h"
#include "SURFACE.h"
c #include "GRID.h"
c #include "DYNVARS.h"
#ifdef ALLOW_DIC
#  include "DIC_VARS.h"
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
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              fu(i,j,bi,bj) = tauX(i,j,bi,bj)
              fv(i,j,bi,bj) = tauY(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportSLP ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              pLoad(i,j,bi,bj) = atmSLPr(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportSIce ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              sIceLoad(i,j,bi,bj) = seaIceMass(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C-   Fresh-Water & Salinity forcing
         IF ( useImportFW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              EmPmR(i,j,bi,bj) = FWFlux    (i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportFW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              saltFlux(i,j,bi,bj)= iceSaltFlx(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C-   Heat Flux forcing
         IF ( useImportHFlx ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              Qnet(i,j,bi,bj)= HeatFlux  (i,j,bi,bj)
            ENDDO
           ENDDO
#ifdef SHORTWAVE_HEATING
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             Qsw(i,j,bi,bj) = qShortWave(i,j,bi,bj)
            ENDDO
           ENDDO
#endif
         ENDIF

#ifdef ALLOW_DIC
C-   Carbon/DIC exchanges
         IF ( useImportFice ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             fice(i,j,bi,bj)= fracIce(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportCO2 ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             AtmospCO2(i,j,bi,bj)= airCO2(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportWSpd ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
             wind(i,j,bi,bj)= surfWSpeed(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#endif /* ALLOW_DIC */

C--     Apply only to forcings relevant for eta/surf.press variations
        ELSE

         IF ( useImportSIce .AND. useImportSLP ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              phi0surf(i,j,bi,bj) = atmSLPr(i,j,bi,bj)*recip_rhoConst
     &                 + gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ELSEIF ( useImportSIce ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              phi0surf(i,j,bi,bj) = pLoad(i,j,bi,bj)*recip_rhoConst
     &                 + gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ELSEIF ( useImportSLP ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              phi0surf(i,j,bi,bj) = atmSLPr(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ENDIF

         IF ( useImportFW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
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
     &    ( ocnCpl_exchange_DIC .OR. apply2AllFields ) ) 
     &   _EXCH_XY_RS( pLoad,    myThid )
       IF ( apply2AllFields ) THEN
        IF ( useImportSIce)  _EXCH_XY_RS( sIceLoad, myThid )
       ELSEIF ( useImportSLP .OR. useImportSIce ) THEN
         _EXCH_XY_RS( phi0surf, myThid )
       ENDIF
#else
       IF ( useImportSLP . AND. ocnCpl_exchange_DIC) 
     &     _EXCH_XY_RS( pLoad,    myThid )
#endif

       IF ( useImportFW  ) _EXCH_XY_RS(EmPmR, myThid )
       IF ( apply2AllFields ) THEN
        IF (useImportTau ) CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
        IF (useImportFW  ) _EXCH_XY_RS(saltFlux, myThid )
        IF (useImportHFlx) _EXCH_XY_RS(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
        IF (useImportHFlx) _EXCH_XY_RS(Qsw ,  myThid )
#endif
#ifdef ALLOW_DIC
        IF ( useImportFice ) _EXCH_XY_RL(fice ,  myThid )
        IF ( useImportCO2 )  _EXCH_XY_RL(AtmospCO2 ,  myThid )
        IF ( useImportWSpd ) _EXCH_XY_RL(wind ,  myThid )
#endif
      ENDIF

      RETURN
      END
