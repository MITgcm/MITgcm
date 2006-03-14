C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_advdiff.F,v 1.3 2006/03/14 15:42:13 mlosch Exp $
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
CML      _RL UICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
CML      _RL VICE       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
CML      _RL HEFF       (1-OLx:sNx+OLx,1-OLy:sNy+OLy,3,nSx,nSy)
CML      _RL HEFFM      (1-OLx:sNx+OLx,1-OLy:sNy+OLy,  nSx,nSy)
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef ALLOW_SEAICE
C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj - Loop counters
C     uc/vc     - current ice velocity on C-grid
C     fld       - copy of scalar field
C     gfld      - tendency of scalar field
      INTEGER i, j, bi, bj, k3
      LOGICAL SEAICEmultiDimAdvection

      _RL uc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vc    (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL fld   (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL gfld  (1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)
CEOP

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

C     Thickness (Volume)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fld (I,J,bi,bj) = HEFF(I,J,1,bi,bj)
          gfld(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     advection
        CALL SEAICE_ADVECTION(
     I       SEAICEadvScheme, GAD_HEFF,
     I       uc, vc, fld,
     O       gfld,
     I       bi, bj, myTime, myIter, myThid)
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          HEFF(I,J,1,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &         HEFF(I,J,1,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj)
     &         )
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      IF ( DIFF1 .GT. 0. _d 0 ) THEN
C     Do we need this?
       CALL SEAICE_EXCH( HEFF, myThid )
C     diffusion
       CALL SEAICE_DIFFUSION( 
     U      HEFF, 
     I      HEFFM, SEAICE_deltaTtherm, myTime, myIter, myThid )
      ENDIF
      
C     Fractional Area
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx
          fld (I,J,bi,bj) = AREA(I,J,1,bi,bj)
          gfld(I,J,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
C     advection
        CALL SEAICE_ADVECTION(
     I       SEAICEadvScheme, GAD_AREA,
     I       uc, vc, fld,
     O       gfld,
     I       bi, bj, myTime, myIter, myThid)
C     now do the "explicit" time step
        DO j=1,sNy
         DO i=1,sNx
          AREA(I,J,1,bi,bj) = HEFFM(I,J,bi,bj) * ( 
     &         AREA(I,J,1,bi,bj) + SEAICE_deltaTtherm * gFld(I,J,bi,bj)
     &         ) 
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( DIFF1 .GT. 0. _d 0 ) THEN
C     Do we need this? Probably not, but it is done in ADVECT
       CALL SEAICE_EXCH( AREA, myThid )
C     diffusion
       CALL SEAICE_DIFFUSION( 
     U      AREA, 
     I      HEFFM, SEAICE_deltaTtherm, myTime, myIter, myThid )
      ENDIF

      ELSE
C     if not multiDimAdvection
       CALL ADVECT( UICE, VICE, HEFF, HEFFM, myThid )
       CALL ADVECT( UICE, VICE, AREA, HEFFM, myThid )
      ENDIF

#endif /* ALLOW_SEAICE */

      RETURN
      END
