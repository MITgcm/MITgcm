C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.11 2006/11/01 01:56:23 jmc Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_THSICE
# include "THSICE_OPTIONS.h"
#endif /* ALLOW_THSICE */
#undef THSICE_NEW_ADVECT

CBOP
C !ROUTINE: SEAICE_ADVDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ADVDIFF(
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ADVDIFF
C     | o driver for different advection routines
C     |   calls an adaption of gad_advection to call different
C     |   advection routines of pkg/generic_advdiff
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables ===
C     UICE/VICE :: ice velocity
C     HEFF      :: scalar field to be advected
C     HEFFM     :: mask for scalar field
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_THSICE
# include "THSICE_PARAMS.h"
# include "THSICE_VARS.h"
#endif /* ALLOW_THSICE */

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     myTime    :: current time
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj :: Loop counters
C     ks        :: surface level index
C     uc/vc     :: current ice velocity on C-grid
C     uTrans    :: volume transport, x direction
C     vTrans    :: volume transport, y direction
C     iceFld    :: copy of seaice field
C     afx       :: horizontal advective flux, x direction
C     afy       :: horizontal advective flux, y direction
C     gFld      :: tendency of seaice field
C     xA,yA     :: "areas" of X and Y face of tracer cells
C     msgBuf    :: Informational/error meesage buffer
      INTEGER i, j, bi, bj
      INTEGER ks
      LOGICAL SEAICEmultiDimAdvection
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL uc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL uTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vTrans    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL iceFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afx       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL afy       (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL gFld      (1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_THSICE
      _RL recip_area(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL minIcArea, minIcHeff
      _RL r_minArea, r_minHeff
      LOGICAL extensiveFld
      LOGICAL dBugFlag
#include "THSICE_DEBUG.h"
#endif /* ALLOW_THSICE */
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      ks = 1

#ifdef ALLOW_THSICE
      dBugFlag = debugLevel.GE.debLevB
      minIcArea = 0. _d 0
      minIcHeff = 0. _d 0
      IF ( useThSice ) THEN
        minIcArea = iceMaskMin
C jmc: new version:
c       minIcHeff = iceMaskMin*himin
      ENDIF
      r_minArea = 0. _d 0
      r_minHeff = 0. _d 0
      IF ( minIcArea.GT.0. _d 0 ) r_minArea = 1. _d 0 / minIcArea
      IF ( minIcHeff.GT.0. _d 0 ) r_minHeff = 1. _d 0 / minIcHeff
#endif /* ALLOW_THSICE */

C--   Get rid of the time dimension for velocities and interpolate
C--   to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uc(i,j,bi,bj)=UICE(i,j,1,bi,bj)
          vc(i,j,bi,bj)=VICE(i,j,1,bi,bj)
         ENDDO
        ENDDO
#else /* not SEAICE_CGRID = BGRID */
C average seaice velocity to c-grid
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          uc(i,j,bi,bj)=.5 _d 0*(UICE(i,j,1,bi,bj)+UICE(i,j+1,1,bi,bj))
          vc(i,j,bi,bj)=.5 _d 0*(VICE(i,j,1,bi,bj)+VICE(I+1,J,1,bi,bj))
         ENDDO
        ENDDO
#endif /* SEAICE_CGRID */
       ENDDO
      ENDDO

#ifndef SEAICE_CGRID
C     Do we need this? I am afraid so.
      CALL EXCH_UV_XY_RL(uc,vc,.TRUE.,myThid)
#endif /* not SEAICE_CGRID */

CML      IF ( SEAICEuseDynamics ) THEN
      SEAICEmultidimadvection = .TRUE.
      IF ( SEAICEadvScheme.EQ.ENUM_CENTERED_2ND
     & .OR.SEAICEadvScheme.EQ.ENUM_UPWIND_3RD
     & .OR.SEAICEadvScheme.EQ.ENUM_CENTERED_4TH ) THEN
       SEAICEmultiDimAdvection = .FALSE.
      ENDIF


      IF ( SEAICEmultiDimAdvection ) THEN
#ifndef ALLOW_AUTODIFF_TAMC
C     This has to be done to comply with the time stepping in advect.F:
C     Making sure that the following routines see the different
C     time levels correctly
C     At the end of the routine ADVECT,
C     timelevel 1 is updated with advection contribution
C                 and diffusion contribution
C                 (which was computed in DIFFUS on timelevel 3)
C     timelevel 2 is the previous timelevel 1
C     timelevel 3 is the total diffusion tendency * deltaT
C                 (empty if no diffusion)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc     = comlev1, key = ikey_dynamics
CADJ STORE vc     = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C---   loops on tile indices bi,bj

#ifdef ALLOW_AUTODIFF_TAMC
C     Initialise for TAF
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          iceFld(i,j)     = 0. _d 0
          gFld(i,j)       = 0. _d 0
#ifdef ALLOW_THSICE
          recip_area(i,j) = 0. _d 0
#endif
         ENDDO
        ENDDO
#endif /* ALLOW_AUTODIFF_TAMC */

        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(i,j,3,bi,bj) = 0. _d 0
          HEFF(i,j,2,bi,bj) = HEFF(i,j,1,bi,bj)
          AREA(i,j,3,bi,bj) = 0. _d 0
          AREA(i,j,2,bi,bj) = AREA(i,j,1,bi,bj)
          recip_heff(i,j)   = 1. _d 0
         ENDDO
        ENDDO

C-    first compute cell areas used by all tracers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xA(i,j) = _dyG(i,j,bi,bj)*_maskW(i,j,ks,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)*_maskS(i,j,ks,bi,bj)
         ENDDO
        ENDDO
C-    Calculate "volume transports" through tracer cell faces.
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j)
          vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j)
         ENDDO
        ENDDO

C--   Effective Thickness (Volume)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          iceFld(i,j) = HEFF(i,j,1,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_ADVECTION(
     I       GAD_HEFF, SEAICEadvSchHeff, .TRUE.,
     I       uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I       uTrans, vTrans, iceFld, recip_heff,
     O       gFld, afx, afy,
     I       bi, bj, myTime, myIter, myThid )
        IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I       GAD_HEFF,
     I       HEFF(1-OLx,1-OLy,1,bi,bj), HEFFM, xA, yA,
     U       gFld,
     I       bi, bj, myTime, myIter, myThid )
        ENDIF
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          HEFF(i,j,1,bi,bj) = HEFFM(i,j,bi,bj) * (
     &         HEFF(i,j,1,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &         )
         ENDDO
        ENDDO
#ifdef ALLOW_THSICE
        IF ( useThSice ) THEN
C--   Compute conversion factors using updated thickness
c        DO j=1-Oly,sNy+Oly
c         DO i=1-Olx,sNx+Olx
         DO j=1,sNy
          DO i=1,sNx
#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(A,2I4,2I2,I12)') 'ICE_ADV: ij,bij,it=',
     &                                  i,j,bi,bj,myIter
            WRITE(6,'(2(A,1P2E14.6))')
     &       'ICE_ADV: uc=', uc(i,j,bi,bj), uc(i+1,j,bi,bj),
     &             ' , vc=', vc(i,j,bi,bj), vc(i,j+1,bi,bj)
            WRITE(6,'(2(A,1P2E14.6))')
     &       'ICE_ADV: heff_b,a=', HEFF(i,j,2,bi,bj),HEFF(i,j,1,bi,bj)
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: mFx=', gFld(i,j)
           ENDIF
#endif
C jmc: comment out the old version:
           IF ( HEFF(i,j,1,bi,bj) .GT. 0. _d 0 ) THEN
             recip_heff(i,j)  = 1. _d 0/HEFF(i,j,1,bi,bj)
           ELSE
             recip_heff(i,j)  = 0. _d 0
           ENDIF
C jmc: new version:
c          IF ( HEFF(i,j,1,bi,bj) .GE. minIcHeff ) THEN
c            recip_heff(i,j)  = 1. _d 0/HEFF(i,j,1,bi,bj)
c          ELSE
c            recip_heff(i,j)  = r_minHeff
c          ENDIF
C jmc: end new version:
          ENDDO
         ENDDO

C--   Enthalpy in layer 1
#ifdef THSICE_NEW_ADVECT
         extensiveFld = .FALSE.
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = Qice1(i,j,bi,bj)
           uTrans(i,j) = afx(i,j)
           vTrans(i,j) = afy(i,j)
          ENDDO
         ENDDO
#else /* not THSICE_NEW_ADVECT */
         extensiveFld = .TRUE.
C     Compute total enthalphy in cell
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceFld(i,j) = Qice1(i,j,bi,bj)*HEFF(i,j,2,bi,bj)
          ENDDO
         ENDDO
#endif /* THSICE_NEW_ADVECT */
         CALL SEAICE_ADVECTION(
     I        GAD_QICE1, SEAICEadvSchEnth, extensiveFld,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map total enthalpy back to enthalpy per area with
C     with current effective thickness
         DO j=1,sNy
          DO i=1,sNx
#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: Qice1_b,a=',
     &             Qice1(i,j,bi,bj),
     &        ( iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          ) * recip_heff(i,j)
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: q1Fx=', gFld(i,j)
           ENDIF
#endif
#ifdef THSICE_NEW_ADVECT
           Qice1(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          )
#else /* not THSICE_NEW_ADVECT */
           Qice1(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          ) * recip_heff(i,j)
#endif /* THSICE_NEW_ADVECT */
          ENDDO
         ENDDO

C--   Enthalpy in layer 2
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Compute total enthalphy in cell, using old effective thickness
           iceFld(i,j) = Qice2(i,j,bi,bj)*HEFF(i,j,2,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_QICE2, SEAICEadvSchEnth, .TRUE.,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_heff,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map total enthalpy back to enthalpy per area with
C     with current effective thickness
         DO j=1,sNy
          DO i=1,sNx
#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: Qice2_b,a=',
     &             Qice2(i,j,bi,bj),
     &        ( iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          ) * recip_heff(i,j)
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: q2Fx=', gFld(i,j)
           ENDIF
#endif
           Qice2(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          ) * recip_heff(i,j)
          ENDDO
         ENDDO

#ifdef THSICE_NEW_ADVECT
C-      set back volume transports:
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           uTrans(i,j) = uc(i,j,bi,bj)*xA(i,j)
           vTrans(i,j) = vc(i,j,bi,bj)*yA(i,j)
          ENDDO
         ENDDO
#endif /* THSICE_NEW_ADVECT */

        ENDIF
#endif /* ALLOW_THSICE */

C--   Fractional area
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          iceFld(i,j) = AREA(i,j,1,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_ADVECTION(
     I       GAD_AREA, SEAICEadvSchArea, .TRUE.,
     I       uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I       uTrans, vTrans, iceFld, recip_heff,
     O       gFld, afx, afy,
     I       bi, bj, myTime, myIter, myThid )
        IF ( diff1 .GT. 0. _d 0 ) THEN
C-    Add tendency due to diffusion
          CALL SEAICE_DIFFUSION(
     I       GAD_AREA,
     I       AREA(1-OLx,1-OLy,1,bi,bj), HEFFM, xA, yA,
     U       gFld,
     I       bi, bj, myTime, myIter, myThid )
        ENDIF
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          AREA(i,j,1,bi,bj) = HEFFM(i,j,bi,bj) * (
     &         AREA(i,j,1,bi,bj) + SEAICE_deltaTtherm * gFld(i,j)
     &         )
         ENDDO
        ENDDO

#ifdef ALLOW_THSICE
        IF ( useThSice ) THEN
C--   Compute conversion factors using updated area
c        DO j=1-Oly,sNy+Oly
c         DO i=1-Olx,sNx+Olx
         DO j=1,sNy
          DO i=1,sNx
#ifdef ALLOW_DBUG_THSICE
           IF ( dBug(i,j,bi,bj) ) THEN
            WRITE(6,'(2(A,1P2E14.6))')
     &       'ICE_ADV: area_b,a=', AREA(i,j,2,bi,bj),AREA(i,j,1,bi,bj)
            WRITE(6,'(A,1P4E14.6)') 'ICE_ADV: mFx=', gFld(i,j)
           ENDIF
#endif
C jmc: comment out the old version:
           IF ( AREA(i,j,1,bi,bj) .GT. 0. _d 0 ) THEN
             recip_area(i,j) = 1. _d 0/AREA(i,j,1,bi,bj)
           ELSE
             recip_area(i,j) = 0. _d 0
           ENDIF
C jmc: new version:
c          IF ( HEFF(i,j,1,bi,bj) .GE. minIcHeff ) THEN
C-    where there is ice, ensure that fractional Ice Area is > minIcArea & < 1
c           IF ( AREA(i,j,1,bi,bj) .GT. 1. _d 0 ) THEN
c            AREA(i,j,1,bi,bj)= 1. _d 0
c            recip_area(i,j)  = 1. _d 0
c           ELSEIF ( AREA(i,j,1,bi,bj) .LT. minIcArea ) THEN
c            AREA(i,j,1,bi,bj)= minIcArea
c            recip_area(i,j)  = r_minArea
c           ELSE
c            recip_area(i,j)  = 1. _d 0/AREA(i,j,1,bi,bj)
c           ENDIF
c          ELSE
C-    not enough ice
c            recip_area(i,j)  = r_minArea
c          ENDIF
C jmc: end new version:
          ENDDO
         ENDDO

C--   Effective snow thickness (Volume), from old AREA
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Compute  effective snow thickness
           iceFld(i,j) = snowHeight(i,j,bi,bj)*AREA(i,j,2,bi,bj)
c          uTrans(i,j) = afx(i,j)
c          vTrans(i,j) = afy(i,j)
          ENDDO
         ENDDO
         CALL SEAICE_ADVECTION(
     I        GAD_SNOW, SEAICEadvSchSnow, .TRUE.,
     I        uc(1-OLx,1-OLy,bi,bj), vc(1-OLx,1-OLy,bi,bj),
     I        uTrans, vTrans, iceFld, recip_area,
     O        gFld, afx, afy,
     I        bi, bj, myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map effective snow thickness back to actual snow thickness
C     with current area
         DO j=1,sNy
          DO i=1,sNx
           snowHeight(i,j,bi,bj) = HEFFM(i,j,bi,bj) * (
     &          iceFld(i,j) + SEAICE_deltaTtherm * gFld(i,j)
     &          ) * recip_area(i,j)
          ENDDO
         ENDDO

C--   Correct for "disappearing" ice (new effective ice thickness < minIcHeff):
C      put this small ice volume into the neighbouring grid box with highest HEFF
C     But this does not work next to the tile edges ! For now, just reset to zero:
         DO j=1,sNy
          DO i=1,sNx
           IF ( HEFF(i,j,1,bi,bj) .LT. minIcHeff ) THEN
             HEFF(i,j,1,bi,bj) = 0. _d 0
             Qice1(i,j,bi,bj)  = 0. _d 0
             Qice2(i,j,bi,bj)  = 0. _d 0
             AREA(i,j,1,bi,bj) = 0. _d 0
             snowHeight(i,j,bi,bj)= 0. _d 0
           ENDIF
          ENDDO
         ENDDO

C--   Map HEFF to ice thickness, using the latest area
         DO j=1,sNy
          DO i=1,sNx
           iceHeight(i,j,bi,bj) = HEFF(i,j,1,bi,bj)*recip_area(i,j)
           iceMask(i,j,bi,bj)   = AREA(i,j,1,bi,bj)
          ENDDO
         ENDDO

        ENDIF
#endif /* ALLOW_THSICE */

C---   end bi,bj loops
       ENDDO
      ENDDO

#else
      STOP 'SEAICEmultiDimAdvection not yet implemented for adjoint'
#endif /* ndef ALLOW_AUTODIFF_TAMC */
      ELSE
C--   if not multiDimAdvection

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE uc   = comlev1, key = ikey_dynamics
CADJ STORE vc   = comlev1, key = ikey_dynamics
#endif /* ALLOW_AUTODIFF_TAMC */

       CALL ADVECT( uc, vc, HEFF, HEFFM, myThid )
       CALL ADVECT( uc, vc, AREA, HEFFM, myThid )
#ifdef ALLOW_THSICE
       IF ( useThSice ) THEN
        WRITE(msgBuf,'(2A)') 'S/R SEAICE_ADVDIFF: ',
     &       'traditional advection/diffusion not yet implemented'
        CALL PRINT_ERROR( msgBuf , myThid)
        WRITE(msgBuf,'(2A)') '                    ',
     &       'for ThSice variable Qice1, Qice2, SnowHeight. Sorry!'
        CALL PRINT_ERROR( msgBuf , myThid)
          STOP 'ABNORMAL: END: S/R SEAICE_ADVDIFF'
       ENDIF
#endif /* ALLOW_THSICE */

C--   end if multiDimAdvection
      ENDIF
C--   end if SEAICEuseDynamics
CML      ENDIF
#endif /* ALLOW_SEAICE */

      RETURN
      END
