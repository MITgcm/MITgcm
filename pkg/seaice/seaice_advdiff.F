C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.6 2006/06/05 22:46:50 mlosch Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
 
CBOP
C !ROUTINE: SEAICE_ADVDIFF

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ADVDIFF( 
     I     myTime, myIter, myThid )

C !DESCRIPTION: \bv
C     /===========================================================\
C     | SUBROUTINE SEAICE_ADVDIFF                                 |
C     | o driver for different advection routines                 |
C     |   calls an adaption of gad_advection to call different    |
C     |   advection routines of pkg/generic_advdiff               |
C     \===========================================================/
      IMPLICIT NONE
c \ev
 
C !USES: ===============================================================
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "GAD.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"
#ifdef ALLOW_THSICE
# include "THSICE_VARS.h"
#endif /* ALLOW_THSICE */

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     UICE/VICE - ice velocity
C     HEFF      - scalar field to be advected
C     HEFFM     - mask for scalar field
C     myTime - current time
C     myIter - iteration number
C     myThid - Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,k,bi,bj - Loop counters
C     uc/vc     - current ice velocity on C-grid
C     fld       - copy of scalar field
C     gfld      - tendency of scalar field
C     xA,yA     - "areas" of X and Y face of tracer cells
C     msgBuf    - Informational/error meesage buffer
      INTEGER i, j, k, bi, bj
      LOGICAL SEAICEmultiDimAdvection
      CHARACTER*(MAX_LEN_MBUF) msgBuf

      _RL uc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc        (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gfld      (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
      _RS xA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS yA        (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#ifdef ALLOW_THSICE
      _RL recip_area(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL recip_heff(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
#endif /* ALLOW_THSICE */
CEOP

      k = 1
#ifdef ALLOW_AUTODIFF_TAMC
C     Initialise for TAF
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fld (I,J,bi,bj)    = 0. _d 0
          gFld(I,J,bi,bj)    = 0. _d 0
          recip_area(I,J)    = 0. _d 0 
          recip_heff(I,J)    = 0. _d 0 
         ENDDO
        ENDDO
       ENDDO
      ENDDO
#endif

C--   Get rid of the time dimension for velocities and interpolate
C--   to C-points if necessary
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
#ifdef SEAICE_CGRID
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          uc(I,J,bi,bj)=UICE(I,J,1,bi,bj)
          vc(I,J,bi,bj)=VICE(I,J,1,bi,bj)
         ENDDO
        ENDDO
#else /* not SEAICE_CGRID = BGRID */
C average seaice velocity to c-grid
        DO j=1-Oly,sNy+Oly-1
         DO i=1-Olx,sNx+Olx-1
          uc(I,J,bi,bj)=.5 _d 0*(UICE(I,J,1,bi,bj)+UICE(I,J+1,1,bi,bj))
          vc(I,J,bi,bj)=.5 _d 0*(VICE(I,J,1,bi,bj)+VICE(I+1,J,1,bi,bj))
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
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          HEFF(I,J,3,bi,bj) = 0. _d 0 !HEFF(I,J,2,bi,bj)
          HEFF(I,J,2,bi,bj) = HEFF(I,J,1,bi,bj)
          AREA(I,J,3,bi,bj) = 0. _d 0 !AREA(I,J,2,bi,bj)
          AREA(I,J,2,bi,bj) = AREA(I,J,1,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     first compute cell areas used by all tracers
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          xA(i,j) = _dyG(i,j,bi,bj)*_maskW(i,j,k,bi,bj)
          yA(i,j) = _dxG(i,j,bi,bj)*_maskS(i,j,k,bi,bj)
#ifdef ALLOW_THSICE
          recip_area(I,J) = 0. _d 0 
          IF ( AREA(I,J,1,bi,bj) .GT. 0. _d 0 ) 
     &         recip_area(I,J) = 1./AREA(I,J,1,bi,bj)
          recip_heff(I,J) = 0. _d 0 
          IF ( HEFF(I,J,1,bi,bj) .GT. 0. _d 0 ) 
     &         recip_heff(I,J) = 1./HEFF(I,J,1,bi,bj)
#endif /* ALLOW_THSICE */
         ENDDO
        ENDDO
C--   Effective Thickness (Volume)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fld(I,J,bi,bj) = HEFF(I,J,1,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_calc_rhs( 
     I       uc, vc, fld, HEFFM, xA, yA,
     I       SEAICEadvScheme, GAD_HEFF, bi, bj, 
     O       gFld,
     I       myTime, myIter, myThid )
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          HEFF(I,J,1,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &         HEFF(I,J,1,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj)
     &         )
         ENDDO
        ENDDO
C--   Fractional area
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fld(I,J,bi,bj) = AREA(I,J,1,bi,bj)
         ENDDO
        ENDDO
        CALL SEAICE_calc_rhs( 
     I       uc, vc, fld, HEFFM, xA, yA,
     I       SEAICEadvScheme, GAD_AREA, bi, bj, 
     O       gFld,
     I       myTime, myIter, myThid )
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          AREA(I,J,1,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &         AREA(I,J,1,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj)
     &         )
         ENDDO
        ENDDO
#ifdef ALLOW_THSICE
        IF ( useThSice ) THEN   
C--   Compute conversion factors using updated thickness and area
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           recip_area(I,J) = 0. _d 0 
           IF ( AREA(I,J,1,bi,bj) .GT. 0. _d 0 ) 
     &          recip_area(I,J) = 1./AREA(I,J,1,bi,bj)
           recip_heff(I,J) = 0. _d 0 
           IF ( HEFF(I,J,1,bi,bj) .GT. 0. _d 0 ) 
     &          recip_heff(I,J) = 1./HEFF(I,J,1,bi,bj)
          ENDDO
         ENDDO
C--   Map HEFF to ice thickness, using the latest area
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
           iceHeight(I,J,bi,bj) = HEFF(I,J,1,bi,bj)*recip_area(I,J)
           iceMask(I,J,bi,bj)   = AREA(I,J,1,bi,bj)
          ENDDO
         ENDDO
C--   Enthalpy in layer 1
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Compute total enthalphy in cell
           fld(i,j,bi,bj) = Qice1(i,j,bi,bj)*HEFF(i,j,2,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_calc_rhs( 
     I        uc, vc, fld, HEFFM, xA, yA,
     I        SEAICEadvScheme, GAD_QICE1, bi, bj, 
     O        gFld,
     I        myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map total enthalpy back to enthalpy per area with
C     with current effective thickness
         DO j=1,sNy
          DO i=1,sNx
           Qice1(I,J,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &          fld(I,J,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj) 
     &          ) * recip_heff(I,J) 
          ENDDO
         ENDDO
C--   Enthalpy in layer 2
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Compute total enthalphy in cell, using old effective thickness
           fld(i,j,bi,bj) = Qice2(i,j,bi,bj)*HEFF(i,j,2,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_calc_rhs( 
     I        uc, vc, fld, HEFFM, xA, yA,
     I        SEAICEadvScheme, GAD_QICE2, bi, bj, 
     O        gFld,
     I        myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map total enthalpy back to enthalpy per area with
C     with current effective thickness
         DO j=1,sNy
          DO i=1,sNx
           Qice2(I,J,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &          fld(I,J,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj) 
     &          ) * recip_heff(I,J) 
          ENDDO
         ENDDO
C--   Effective snow thickness (Volume), from old AREA
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
C     Compute  effective snow thickness
           fld(i,j,bi,bj) = snowHeight(i,j,bi,bj)*AREA(i,j,2,bi,bj)
          ENDDO
         ENDDO
         CALL SEAICE_calc_rhs( 
     I        uc, vc, fld, HEFFM, xA, yA,
     I        SEAICEadvScheme, GAD_SNOW, bi, bj, 
     O        gFld,
     I        myTime, myIter, myThid )
C     now do the "explicit" time step
C     and map effective snow thickness back to actual snow thickness
C     with current area
         DO j=1,sNy
          DO i=1,sNx
           snowHeight(I,J,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &          fld(I,J,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj)
     &          ) * recip_area(I,J)
          ENDDO
         ENDDO
        ENDIF
#endif / * ALLOW_THSICE */
       ENDDO
      ENDDO

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
