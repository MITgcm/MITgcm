#include "LONGSTEP_OPTIONS.h"
#ifdef ALLOW_GMREDI
# include "GMREDI_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: LONGSTEP_AVERAGE
C     !INTERFACE:
      SUBROUTINE LONGSTEP_AVERAGE( myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE LONGSTEP_AVERAGE
C     | o Average variables needed for longstep
C     |   - myIter is used for determining the averaging interval
C     |     (LS_nIter timesteps, phase 0)
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "FFIELDS.h"
#include "SURFACE.h"
#include "LONGSTEP_PARAMS.h"
#include "LONGSTEP.h"
#ifdef ALLOW_GMREDI
#include "GMREDI.h"
#endif
#ifdef ALLOW_KPP
#include "KPP.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myTime     :: Current time in simulation
C     myIter     :: Current iteration number in simulation
C     myThid     :: my Thread Id number
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_LONGSTEP
C     !LOCAL VARIABLES:
C     == Local variables ==
CEOP

C     first iteration in a long time step? - reset averages
      IF ( MOD(myIter, LS_nIter) .EQ. 0 ) THEN

       CALL LONGSTEP_RESET_3D(LS_uVelCount, LS_uVel, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_vVelCount, LS_vVel, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_wVelCount, LS_wVel, Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_thetaCount,LS_theta,Nr, myThid)
       CALL LONGSTEP_RESET_3D(LS_saltCount, LS_salt, Nr, myThid)
       IF ( ivdc_kappa .NE. 0. _d 0 )
     &  CALL LONGSTEP_RESET_3D(LS_IVDConvCountCount,
     &                        LS_IVDConvCount, Nr, myThid)
#ifdef SHORTWAVE_HEATING
       CALL LONGSTEP_RESET_3D(LS_QswCount, LS_Qsw, 1, myThid)
#endif
       CALL LONGSTEP_RESET_3D(LS_fwFluxCount, LS_fwFlux, 1, myThid)
#ifdef ALLOW_GMREDI
       IF ( useGMRedi ) THEN
        CALL LONGSTEP_RESET_3D(LS_KwxCount, LS_Kwx, Nr, myThid)
        CALL LONGSTEP_RESET_3D(LS_KwyCount, LS_Kwy, Nr, myThid)
        CALL LONGSTEP_RESET_3D(LS_KwzCount, LS_Kwz, Nr, myThid)
        CALL LONGSTEP_RESET_3D(LS_KuxCount, LS_Kux, Nr, myThid)
        CALL LONGSTEP_RESET_3D(LS_KvyCount, LS_Kvy, Nr, myThid)
        IF ( GM_ExtraDiag ) THEN
         CALL LONGSTEP_RESET_3D(LS_KuzCount, LS_Kuz, Nr, myThid)
         CALL LONGSTEP_RESET_3D(LS_KvzCount, LS_Kvz, Nr, myThid)
        ENDIF
        IF ( GM_AdvForm ) THEN
         CALL LONGSTEP_RESET_3D(LS_PsiXCount, LS_PsiX, Nr, myThid)
         CALL LONGSTEP_RESET_3D(LS_PsiYCount, LS_PsiY, Nr, myThid)
        ENDIF
       ENDIF
#endif
#ifdef ALLOW_KPP
       IF ( useKPP ) THEN
        CALL LONGSTEP_RESET_3D(LS_KPPdiffKzSCount,
     &                         LS_KPPdiffKzS, Nr, myThid)
        CALL LONGSTEP_RESET_3D(LS_KPPghatCount,
     &                         LS_KPPghat, Nr, myThid)
       ENDIF
#endif

      ENDIF

C--   update average inside tile (no overlaps) for all bi,bj

      CALL LONGSTEP_FILL_3D_FAC(LS_uVelCount, LS_uVel, uVel, hFacW,
     &                          Nr, myThid)
      CALL LONGSTEP_FILL_3D_FAC(LS_vVelCount, LS_vVel, vVel, hFacS,
     &                          Nr, myThid)
      CALL LONGSTEP_FILL_3D(LS_wVelCount, LS_wVel, wVel, Nr, myThid)
      CALL LONGSTEP_FILL_3D(LS_thetaCount, LS_theta, theta, Nr, myThid)
      CALL LONGSTEP_FILL_3D(LS_saltCount, LS_salt, salt, Nr, myThid)
      IF ( ivdc_kappa .NE. 0. _d 0 )
     & CALL LONGSTEP_FILL_3D(LS_IVDConvCountCount,
     &                      LS_IVDConvCount, IVDConvCount, Nr, myThid)
#ifdef SHORTWAVE_HEATING
      CALL LONGSTEP_FILL_3D_RS(LS_QswCount, LS_Qsw, Qsw, 1, myThid)
#endif
      IF ( LS_usePmEpR ) THEN
       CALL LONGSTEP_FILL_3D(   LS_fwFluxCount,LS_fwFlux,PmEpR,1,myThid)
      ELSE
       CALL LONGSTEP_FILL_3D_RS(LS_fwFluxCount,LS_fwFlux,EmPmR,1,myThid)
      ENDIF
#ifdef ALLOW_GMREDI
      IF ( useGMRedi ) THEN
       CALL LONGSTEP_FILL_3D(LS_KwxCount, LS_Kwx, Kwx, Nr, myThid)
       CALL LONGSTEP_FILL_3D(LS_KwyCount, LS_Kwy, Kwy, Nr, myThid)
       CALL LONGSTEP_FILL_3D(LS_KwzCount, LS_Kwz, Kwz, Nr, myThid)
       CALL LONGSTEP_FILL_3D(LS_KuxCount, LS_Kux, Kux, Nr, myThid)
       CALL LONGSTEP_FILL_3D(LS_KvyCount, LS_Kvy, Kvy, Nr, myThid)
#ifdef GM_EXTRA_DIAGONAL
       IF ( GM_ExtraDiag ) THEN
        CALL LONGSTEP_FILL_3D(LS_KuzCount, LS_Kuz, Kuz, Nr, myThid)
        CALL LONGSTEP_FILL_3D(LS_KvzCount, LS_Kvz, Kvz, Nr, myThid)
       ENDIF
#endif
#ifdef GM_BOLUS_ADVEC
       IF ( GM_AdvForm ) THEN
        CALL LONGSTEP_FILL_3D(LS_PsiXCount, LS_PsiX, GM_PsiX,
     &                        Nr, myThid)
        CALL LONGSTEP_FILL_3D(LS_PsiYCount, LS_PsiY, GM_PsiY,
     &                        Nr, myThid)
       ENDIF
#endif
      ENDIF
#endif
#ifdef ALLOW_KPP
      IF ( useKPP ) THEN
       CALL LONGSTEP_FILL_3D(LS_KPPdiffKzSCount,
     &                       LS_KPPdiffKzS, KPPdiffKzS, Nr, myThid)
       CALL LONGSTEP_FILL_3D(LS_KPPghatCount,
     &                       LS_KPPghat, KPPghat, Nr, myThid)
      ENDIF
#endif

C     time for a ptracer time step? - finish average
      IF ( MOD(myIter, LS_nIter) .EQ. LS_nIter-1 ) THEN

       CALL LONGSTEP_AVERAGE_3D_FAC(LS_uVelCount, LS_uVel, hFacW,
     &                              Nr, myThid)
       CALL LONGSTEP_AVERAGE_3D_FAC(LS_vVelCount, LS_vVel, hFacS,
     &                              Nr, myThid)
       CALL LONGSTEP_AVERAGE_3D(LS_wVelCount, LS_wVel, Nr, myThid)
       CALL LONGSTEP_AVERAGE_3D(LS_thetaCount,LS_theta,Nr, myThid)
       CALL LONGSTEP_AVERAGE_3D(LS_saltCount, LS_salt, Nr, myThid)
       IF ( ivdc_kappa .NE. 0. _d 0 )
     &  CALL LONGSTEP_AVERAGE_3D(LS_IVDConvCountCount,
     &                        LS_IVDConvCount, Nr, myThid)
#ifdef SHORTWAVE_HEATING
       CALL LONGSTEP_AVERAGE_3D(LS_QswCount, LS_Qsw, 1, myThid)
#endif
       CALL LONGSTEP_AVERAGE_3D(LS_fwFluxCount, LS_fwFlux, 1, myThid)

#ifdef ALLOW_GMREDI
       IF ( useGMRedi ) THEN
        CALL LONGSTEP_AVERAGE_3D(LS_KwxCount, LS_Kwx, Nr, myThid)
        CALL LONGSTEP_AVERAGE_3D(LS_KwyCount, LS_Kwy, Nr, myThid)
        CALL LONGSTEP_AVERAGE_3D(LS_KwzCount, LS_Kwz, Nr, myThid)
        CALL LONGSTEP_AVERAGE_3D(LS_KuxCount, LS_Kux, Nr, myThid)
        CALL LONGSTEP_AVERAGE_3D(LS_KvyCount, LS_Kvy, Nr, myThid)
        IF ( GM_ExtraDiag ) THEN
         CALL LONGSTEP_AVERAGE_3D(LS_KuzCount, LS_Kuz, Nr, myThid)
         CALL LONGSTEP_AVERAGE_3D(LS_KvzCount, LS_Kvz, Nr, myThid)
        ENDIF
        IF ( GM_AdvForm ) THEN
         CALL LONGSTEP_AVERAGE_3D(LS_PsiXCount, LS_PsiX, Nr, myThid)
         CALL LONGSTEP_AVERAGE_3D(LS_PsiYCount, LS_PsiY, Nr, myThid)
        ENDIF
       ENDIF
#endif
#ifdef ALLOW_KPP
       IF ( useKPP ) THEN
        CALL LONGSTEP_AVERAGE_3D(LS_KPPdiffKzSCount,
     &                           LS_KPPdiffKzS, Nr, myThid)
        CALL LONGSTEP_AVERAGE_3D(LS_KPPghatCount,
     &                           LS_KPPghat, Nr, myThid)
       ENDIF
#endif

#if 0
C     and update overlaps
       CALL EXCH_UV_3D_RL( LS_uVel, LS_vVel, .TRUE., Nr, myThid )
       CALL EXCH_3D_RL( LS_wVel , Nr, myThid )
       CALL EXCH_3D_RL( LS_theta, Nr, myThid )
       CALL EXCH_3D_RL( LS_salt , Nr, myThid )
       IF ( ivdc_kappa .NE. 0. _d 0 )
     &  CALL EXCH_3D_RL( LS_IVDConvCount, myThid )
#ifdef SHORTWAVE_HEATING
       CALL EXCH_XY_RL( LS_Qsw, myThid )
#endif
       CALL EXCH_XY_RL( LS_fwFlux, myThid )
#ifdef ALLOW_GMREDI
       IF ( useGMRedi ) THEN
        CALL EXCH_UV_AGRID_3D_RL( LS_Kwx, LS_Kwy, .TRUE., Nr, myThid )
        CALL EXCH_3D_RL( LS_Kwz  , Nr, myThid )
        CALL EXCH_UV_3D_RL( LS_Kux, LS_Kvy, .FALSE., Nr, myThid )
        IF ( GM_ExtraDiag ) THEN
         CALL EXCH_UV_3D_RL( LS_Kuz, LS_Kvz, .TRUE., Nr, myThid )
        ENDIF
        IF ( GM_AdvForm ) THEN
         CALL EXCH_UV_3D_RL( LS_PsiX, LS_PsiY, .TRUE., Nr, myThid )
        ENDIF
       ENDIF
#endif
#ifdef ALLOW_KPP
       IF ( useKPP ) THEN
        CALL EXCH_3D_RL( LS_KPPdiffKzS, Nr, myThid )
        CALL EXCH_3D_RL( LS_KPPghat, Nr, myThid )
       ENDIF
#endif
#endif /* 0 */

#ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL(LS_uVel, 'LSuVel  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(LS_vVel, 'LSvVel  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(LS_wVel, 'LSwVel  ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(LS_theta,'LStheta ',0,Nr,0,1,1,myThid)
        CALL DIAGNOSTICS_FILL(LS_salt, 'LSsalt  ',0,Nr,0,1,1,myThid)
        IF ( ivdc_kappa .NE. 0. _d 0 .AND. .NOT. useKPP )
     &   CALL DIAGNOSTICS_FILL(LS_IVDConvCount,
     &                                 'LScnvAdj',0,Nr,0,1,1,myThid)
#ifdef SHORTWAVE_HEATING
        CALL DIAGNOSTICS_FILL(LS_Qsw , 'LSqsw   ',0,1 ,0,1,1,myThid)
#endif
        CALL DIAGNOSTICS_FILL(LS_fwFlux, 'LSfwFlux',0,1 ,0,1,1,myThid)
#ifdef ALLOW_GMREDI
        IF ( useGMRedi ) THEN
         CALL DIAGNOSTICS_FILL(LS_Kwx , 'LSkwx   ',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(LS_Kwy , 'LSkwy   ',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(LS_Kwz , 'LSkwz   ',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(LS_Kux , 'LSkux   ',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(LS_Kvy , 'LSkvy   ',0,Nr,0,1,1,myThid)
         IF ( GM_ExtraDiag ) THEN
          CALL DIAGNOSTICS_FILL(LS_Kuz, 'LSkuz   ',0,Nr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL(LS_Kvz, 'LSkvz   ',0,Nr,0,1,1,myThid)
         ENDIF
         IF ( GM_AdvForm ) THEN
          CALL DIAGNOSTICS_FILL(LS_PsiX, 'LS_PsiX ',0,Nr,0,1,1,myThid)
          CALL DIAGNOSTICS_FILL(LS_PsiY, 'LS_PsiY ',0,Nr,0,1,1,myThid)
         ENDIF
        ENDIF
#endif
#ifdef ALLOW_KPP
        IF ( useKPP ) THEN
         CALL DIAGNOSTICS_FILL(LS_KPPdiffKzs,
     &                                 'LSKPPdfS',0,Nr,0,1,1,myThid)
         CALL DIAGNOSTICS_FILL(LS_KPPghat,
     &                                 'LSKPPght',0,Nr,0,1,1,myThid)
        ENDIF
#endif
C       if useDiagnostics: end
       ENDIF
#endif /* ALLOW_DIAGNOSTICS */

       _BARRIER
       _BEGIN_MASTER(myThid)
C      we decide whether to do a timestep here and everybody else just
C      checks this variable.  This avoids complications with
C      staggerTimeStep
       LS_doTimeStep = .TRUE.
       _END_MASTER(myThid)
       _BARRIER

      ELSEIF ( MOD(myIter, LS_nIter) .EQ. 0 ) THEN

C      next time-step: switch back LS_doTimeStep
       _BARRIER
       _BEGIN_MASTER(myThid)
       LS_doTimeStep = .FALSE.
       _END_MASTER(myThid)
       _BARRIER

C      if MOD(myIter, LS_nIter): end
      ENDIF

#endif /* ALLOW_LONGSTEP */

      RETURN
      END

