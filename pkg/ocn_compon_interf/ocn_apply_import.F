C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_apply_import.F,v 1.2 2004/05/21 19:42:26 jmc Exp $
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
#include "OCNCPL.h"
#include "FFIELDS.h"
#include "SURFACE.h"
c #include "GRID.h"
c #include "DYNVARS.h"
 
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

C-   Dynamical forcing
         IF ( useImportTau .AND. apply2AllFields ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              fu(i,j,bi,bj) = tauX(i,j,bi,bj)
              fv(i,j,bi,bj) = tauY(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
#ifdef ATMOSPHERIC_LOADING
         IF ( useImportSLP ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              pLoad(i,j,bi,bj) = atmSLPr(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportFW .AND.
     &        nonlinFreeSurf.GT.0 .AND. useRealFreshWaterFlux ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
c             pLoad(i,j,bi,bj) = pLoad(i,j,bi,bj)
c    &            + gravity*seaIceMass(i,j,bi,bj)
C- note: might need to change directly phi0surf if OCN_APPLY_IMPORT 
C        is called after EXTERNAL_FORCING_SURF
              phi0surf(i,j,bi,bj) = pload(i,j,bi,bj)*recip_rhoConst
     &               + gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ELSE
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              phi0surf(i,j,bi,bj) = pload(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ENDIF
#else /* ATMOSPHERIC_LOADING */
         IF ( useImportFW .AND.
     &        nonlinFreeSurf.GT.0 .AND. useRealFreshWaterFlux ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              phi0surf(i,j,bi,bj) =
     &                 gravity*seaIceMass(i,j,bi,bj)*recip_rhoConst
            ENDDO
           ENDDO
         ENDIF
#endif /* ATMOSPHERIC_LOADING */

C-   Fresh-Water & Salinity forcing
         IF ( useImportFW ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              EmPmR(i,j,bi,bj) = FWFlux    (i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF
         IF ( useImportFW .AND. apply2AllFields ) THEN
           DO j=1-Oly,sNy+Oly
            DO i=1-Olx,sNx+Olx
              saltFlux(i,j,bi,bj)= iceSaltFlx(i,j,bi,bj)
            ENDDO
           ENDDO
         ENDIF

C-   Heat Flux forcing
         IF ( useImportHFlx .AND. apply2AllFields ) THEN
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

C--  end bi,bj loop
        ENDDO
       ENDDO

C--  Fill in Halo region with valid values

#ifdef ATMOSPHERIC_LOADING
c      IF ( useImportSLP ) _EXCH_XY_R4(pload, myThid )
       _EXCH_XY_R4(phi0surf, myThid )
#endif
       IF ( useImportFW  ) _EXCH_XY_R4(EmPmR, myThid )
       IF ( apply2AllFields ) THEN
        IF (useImportTau ) CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
        IF (useImportFW  ) _EXCH_XY_R4(saltFlux, myThid )
        IF (useImportHFlx) _EXCH_XY_R4(Qnet , myThid )
#ifdef SHORTWAVE_HEATING
        IF (useImportHFlx) _EXCH_XY_R4(Qsw ,  myThid )
#endif
       ENDIF

      RETURN
      END
