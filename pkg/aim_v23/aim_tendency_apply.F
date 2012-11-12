C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_tendency_apply.F,v 1.10 2012/11/12 15:47:31 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

C--  File aim_tendency_apply.F: Routines to Add AIM tendency contributions
C--   Contents
C--   o AIM_TENDENCY_APPLY_U
C--   o AIM_TENDENCY_APPLY_V
C--   o AIM_TENDENCY_APPLY_T
C--   o AIM_TENDENCY_APPLY_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AIM_TENDENCY_APPLY_U
C     !INTERFACE:
      SUBROUTINE AIM_TENDENCY_APPLY_U(
     I           iMin, iMax, jMin, jMax, bi, bj, k,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_U
C     | o Add AIM tendency terms to U tendency.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_FRICTION_HEATING
# include "FFIELDS.h"
#endif

#include "AIM_PARAMS.h"
#include "AIM2DYN.h"
#include "AIM_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iMin,iMax :: Working range of tile indices for applying forcing.
C     jMin,jMax :: Working range of tile indices for applying forcing.
C     k         :: current level index
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     == Local variables in common block ==
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
C     aim_uStress :: surface stress applied to zonal wind
      COMMON /LOCAL_AIM_TENDENCY_APPLY_U/ aim_uStress,aim_KEuStr
      _RL aim_uStress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aim_KEuStr (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     == Local variables ==
C     i,j  :: Loop counters
      INTEGER i, j
      _RL uStr_tmp
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
      _RL aim_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
      IF ( myTime.EQ.startTime .AND. k.EQ.1 ) THEN
C-    Initialise diagnostic array aim_uStress
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         aim_uStress(i,j,bi,bj) = 0.
         aim_KEuStr(i,j,bi,bj)  = 0.
        ENDDO
       ENDDO
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( k.EQ.Nr .AND. aim_dragStrato.GT.0. ) THEN
C- Note: exclusive IF / ELSE is legitimate here since surface drag
C        is not supposed to be applied in stratosphere
       DO j=jMin,jMax
        DO i=iMin,iMax
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &     -maskW(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)/aim_dragStrato
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
          aim_dKE(i,j) =
     &     -maskW(i,j,k,bi,bj)*uVel(i,j,k,bi,bj)/aim_dragStrato
     &        *uVel(i,j,k,bi,bj)*hFacW(i,j,k,bi,bj)*drF(k)
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEuStr(i,j,bi,bj) = aim_KEuStr(i,j,bi,bj)
     &                          + aim_dKE(i,j)*recip_gravity
#endif
        ENDDO
       ENDDO
      ELSEIF ( k.EQ.1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskW(i,j,k,bi,bj) .NE. 0. ) THEN
          uStr_tmp =
     &     -( aim_drag(i-1,j,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * uVel(i,j,k,bi,bj)
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(k)
     &                       * recip_hFacW(i,j,k,bi,bj)
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
          aim_uStress(i,j,bi,bj) = uStr_tmp
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEuStr(i,j,bi,bj)  = uStr_tmp * uVel(i,j,k,bi,bj)
#endif
#ifdef ALLOW_FRICTION_HEATING
          aim_dKE(i,j) = gravity * uStr_tmp * uVel(i,j,k,bi,bj)
         ELSE
          aim_dKE(i,j) = 0.
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskW(i,j,k-1,bi,bj) .EQ. 0.
     &    .AND. maskW(i,j,k,bi,bj) .NE. 0. ) THEN
          uStr_tmp =
     &      -( (1.-maskC(i-1,j,k-1,bi,bj))*aim_drag(i-1,j,bi,bj)
     &        +(1.-maskC( i ,j,k-1,bi,bj))*aim_drag( i ,j,bi,bj)
     &       )* 0.5 _d 0 * uVel(i,j,k,bi,bj)
          gU(i,j,k,bi,bj) = gU(i,j,k,bi,bj)
     &                       + uStr_tmp*gravity*recip_drF(k)
     &                       * recip_hFacW(i,j,k,bi,bj)
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
          aim_uStress(i,j,bi,bj) = uStr_tmp
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEuStr(i,j,bi,bj)  = uStr_tmp * uVel(i,j,k,bi,bj)
#endif
#ifdef ALLOW_FRICTION_HEATING
          aim_dKE(i,j) = gravity * uStr_tmp * uVel(i,j,k,bi,bj)
         ELSE
          aim_dKE(i,j) = 0.
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_FRICTION_HEATING
      IF ( addFrictionHeating ) THEN
        DO j=1,sNy
         DO i=1,sNx
           frictionHeating(i,j,k,bi,bj) = frictionHeating(i,j,k,bi,bj)
     &                 -0.5 _d 0 * ( aim_dKE(i,j) + aim_dKE(i+1,j) )
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_FRICTION_HEATING */
#ifdef ALLOW_AIM_TAVE
      IF ( aim_taveFreq.NE.0 .AND. k.EQ.Nr ) THEN
        CALL TIMEAVE_CUMULATE( USTRtave, aim_uStress, 1,
     &                         deltaTClock, bi, bj, myThid )
      ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
      IF ( usediagnostics .AND. k.EQ.Nr ) THEN
        CALL DIAGNOSTICS_FILL( aim_uStress, 'UFLUX   ',
     &                         0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL( aim_KEuStr,  'dKE_Ustr',
     &                         0,1,1,bi,bj,myThid)
      ENDIF
#endif

#endif /* ALLOW_AIM */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AIM_TENDENCY_APPLY_V
C     !INTERFACE:
      SUBROUTINE AIM_TENDENCY_APPLY_V(
     I           iMin, iMax, jMin, jMax, bi, bj, k,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R TENDENCY_APPLY_V
C     | o Add AIM tendency terms to V tendency.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_FRICTION_HEATING
# include "FFIELDS.h"
#endif

#include "AIM_PARAMS.h"
#include "AIM2DYN.h"
#include "AIM_TAVE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iMin,iMax :: Working range of tile indices for applying forcing.
C     jMin,jMax :: Working range of tile indices for applying forcing.
C     k         :: current level index
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     == Local variables in common block ==
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
C     aim_vStress :: surface stress applied to meridional wind
      COMMON /LOCAL_AIM_TENDENCY_APPLY_V/ aim_vStress,aim_KEvStr
      _RL aim_vStress(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL aim_KEvStr(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
#endif

C     == Local variables ==
C     i,j  :: Loop counters
      INTEGER i, j
      _RL vStr_tmp
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
      _RL aim_dKE(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif

#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
      IF ( myTime.EQ.startTime .AND. k.EQ.1 ) THEN
C-    Initialise diagnostic array aim_uStress
       DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
         aim_vStress(i,j,bi,bj) = 0.
         aim_KEvStr(i,j,bi,bj)  = 0.
        ENDDO
       ENDDO
      ENDIF
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF ( k.EQ.Nr .AND. aim_dragStrato.GT.0. ) THEN
C- Note: exclusive IF / ELSE is legitimate here since surface drag
C        is not supposed to be applied in the stratosphere
       DO j=jMin,jMax
        DO i=iMin,iMax
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &     -maskS(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)/aim_dragStrato
#if ( defined ALLOW_FRICTION_HEATING ) || ( defined ALLOW_DIAGNOSTICS )
          aim_dKE(i,j) =
     &     -maskS(i,j,k,bi,bj)*vVel(i,j,k,bi,bj)/aim_dragStrato
     &        *vVel(i,j,k,bi,bj)*hFacS(i,j,k,bi,bj)*drF(k)
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEvStr(i,j,bi,bj) = aim_KEvStr(i,j,bi,bj)
     &                          + aim_dKE(i,j)*recip_gravity
#endif
        ENDDO
       ENDDO
      ELSEIF ( k.EQ.1 ) THEN
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskS(i,j,k,bi,bj) .NE. 0. ) THEN
          vStr_tmp =
     &     -( aim_drag(i,j-1,bi,bj)+aim_drag(i,j,bi,bj) )
     &       * 0.5 _d 0 * vVel(i,j,k,bi,bj)
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(k)
     &                       * recip_hFacS(i,j,k,bi,bj)
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
          aim_vStress(i,j,bi,bj) = vStr_tmp
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEvStr(i,j,bi,bj)  = vStr_tmp * vVel(i,j,k,bi,bj)
#endif
#ifdef ALLOW_FRICTION_HEATING
          aim_dKE(i,j) = gravity * vStr_tmp * vVel(i,j,k,bi,bj)
         ELSE
          aim_dKE(i,j) = 0.
#endif
         ENDIF
        ENDDO
       ENDDO
      ELSE
       DO j=jMin,jMax
        DO i=iMin,iMax
         IF ( maskS(i,j,k-1,bi,bj) .EQ. 0.
     &    .AND. maskS(i,j,k,bi,bj) .NE. 0. ) THEN
          vStr_tmp =
     &     -( (1.-maskC(i,j-1,k-1,bi,bj))*aim_drag(i,j-1,bi,bj)
     &       +(1.-maskC(i, j ,k-1,bi,bj))*aim_drag(i, j ,bi,bj)
     &      )* 0.5 _d 0 * vVel(i,j,k,bi,bj)
          gV(i,j,k,bi,bj) = gV(i,j,k,bi,bj)
     &                       + vStr_tmp*gravity*recip_drF(k)
     &                       * recip_hFacS(i,j,k,bi,bj)
#if ( defined ALLOW_AIM_TAVE ) || ( defined ALLOW_DIAGNOSTICS )
          aim_vStress(i,j,bi,bj) = vStr_tmp
#endif
#ifdef ALLOW_DIAGNOSTICS
          aim_KEvStr(i,j,bi,bj)  = vStr_tmp * vVel(i,j,k,bi,bj)
#endif
#ifdef ALLOW_FRICTION_HEATING
          aim_dKE(i,j) = gravity * vStr_tmp * vVel(i,j,k,bi,bj)
         ELSE
          aim_dKE(i,j) = 0.
#endif
         ENDIF
        ENDDO
       ENDDO
      ENDIF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_FRICTION_HEATING
      IF ( addFrictionHeating ) THEN
        DO j=1,sNy
         DO i=1,sNx
           frictionHeating(i,j,k,bi,bj) = frictionHeating(i,j,k,bi,bj)
     &                 -0.5 _d 0 * ( aim_dKE(i,j) + aim_dKE(i,j+1) )
         ENDDO
        ENDDO
      ENDIF
#endif /* ALLOW_FRICTION_HEATING */
#ifdef ALLOW_AIM_TAVE
      IF ( aim_taveFreq.NE.0 .AND. k.EQ.Nr ) THEN
        CALL TIMEAVE_CUMULATE( VSTRtave, aim_vStress, 1,
     &                         deltaTClock, bi, bj, myThid )
      ENDIF
#endif
#ifdef ALLOW_DIAGNOSTICS
      IF ( usediagnostics .AND. k.EQ.Nr ) THEN
        CALL DIAGNOSTICS_FILL( aim_vStress, 'VFLUX   ',
     &                         0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL( aim_KEvStr,  'dKE_Vstr',
     &                         0,1,1,bi,bj,myThid)
      ENDIF
#endif

#endif /* ALLOW_AIM */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AIM_TENDENCY_APPLY_T
C     !INTERFACE:
      SUBROUTINE AIM_TENDENCY_APPLY_T(
     I           iMin, iMax, jMin, jMax, bi, bj, k,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_T
C     | o Add AIM tendency to potential Temp tendency.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iMin,iMax :: Working range of tile indices for applying forcing.
C     jMin,jMax :: Working range of tile indices for applying forcing.
C     k         :: current level index
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j  :: Loop counters
      INTEGER I, J

C--   Forcing: add AIM heating/cooling tendency to gT:
      DO J=1,sNy
       DO I=1,sNx
        gT(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
     &       *( gT(i,j,k,bi,bj) + aim_dTdt(i,j,k,bi,bj) )
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: AIM_TENDENCY_APPLY_U
C     !INTERFACE:
      SUBROUTINE AIM_TENDENCY_APPLY_S(
     I           iMin, iMax, jMin, jMax, bi, bj, k,
     I           myTime, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R AIM_TENDENCY_APPLY_S
C     | o Add AIM tendency to Specific Humidity tendency.
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

#include "AIM2DYN.h"

C     !INPUT/OUTPUT PARAMETERS:
C     iMin,iMax :: Working range of tile indices for applying forcing.
C     jMin,jMax :: Working range of tile indices for applying forcing.
C     k         :: current level index
C     myTime    :: Current time in simulation
C     myThid    :: my Thread Id number
      INTEGER iMin, iMax, jMin, jMax, k, bi, bj
      _RL myTime
      INTEGER myThid
CEOP

#ifdef ALLOW_AIM
C     == Local variables ==
C     i,j  :: Loop counters
      INTEGER I, J

C--   Forcing: add AIM dq/dt tendency to gS:
      DO J=1,sNy
       DO I=1,sNx
        gS(i,j,k,bi,bj) = maskC(i,j,k,bi,bj)
     &       *( gS(i,j,k,bi,bj) + aim_dSdt(i,j,k,bi,bj) )
       ENDDO
      ENDDO

#endif /* ALLOW_AIM */

      RETURN
      END
