C $Header: /u/gcmpack/MITgcm/pkg/ocn_compon_interf/ocn_apply_import.F,v 1.1 2003/12/15 02:49:09 jmc Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"
 
CBOP
C     !ROUTINE: OCN_APPLY_IMPORT
C     !INTERFACE:
      SUBROUTINE OCN_APPLY_IMPORT( myTime, myIter, myThid )
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
c #include "GRID.h"
c #include "DYNVARS.h"
 
C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime - Simulation time
C     myIter - Simulation timestep number
C     myThid - Thread no. that called this routine.
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
 
c #ifndef INCLUDE_EXTERNAL_FORCING_PACKAGE

C     !LOCAL VARIABLES:
C     === Local arrays ===
      INTEGER bi,bj,i,j
CEOP

C--   use imported coupling data in place of input files data:
C     WARNING: need to check sign and Units !!!
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           IF (useImportTau) THEN
             fu(i,j,bi,bj) = tauX(i,j,bi,bj)
             fv(i,j,bi,bj) = tauY(i,j,bi,bj)
           ENDIF
           IF (useImportHFlx) Qnet(i,j,bi,bj) = HeatFlux(i,j,bi,bj)
           IF (useImportFW)  EmPmR(i,j,bi,bj) = FWFlux  (i,j,bi,bj)
#ifdef SHORTWAVE_HEATING
           Qsw(i,j,bi,bj)   = qShortwave(i,j,bi,bj)
#endif
#ifdef ATMOSPHERIC_LOADING
c          pload(i,j,bi,bj) = bWght*pload0(i,j,bi,bj)  
c    &                       +aWght*pload1(i,j,bi,bj)
#endif
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       IF (useImportTau)  CALL EXCH_UV_XY_RS(fu,fv,.TRUE.,myThid)
       IF (useImportHFlx) _EXCH_XY_R4(Qnet,  myThid )
       IF (useImportFW)   _EXCH_XY_R4(EmPmR, myThid )
#ifdef SHORTWAVE_HEATING
       _EXCH_XY_R4(Qsw, myThid )
#endif

c #endif /* INCLUDE_EXTERNAL_FORCING_PACKAGE undef */

      RETURN
      END
