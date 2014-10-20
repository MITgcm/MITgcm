C $Header: /u/gcmpack/MITgcm/pkg/seaice/seaice_oceandrag_coeffs.F,v 1.3 2014/10/20 03:20:58 gforget Exp $
C $Name:  $

#include "SEAICE_OPTIONS.h"
#ifdef ALLOW_OBCS
# include "OBCS_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SEAICE_OCEANDRAG_COEFFS
C     !INTERFACE:
      SUBROUTINE SEAICE_OCEANDRAG_COEFFS(
     I     uIceLoc, vIceLoc,
     O     CwatC,
     I     iStep, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SEAICE_OCEANDRAG_COEFFS
C     | o Compute the drag coefficients for ice-ocean drag,
C     |   so that we can use the same code for different solvers
C     *==========================================================*
C     | written by Martin Losch, Oct 2012
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C     !INPUT/OUTPUT PARAMETERS:
C     === Routine arguments ===
C     myTime :: Simulation time
C     myIter :: Simulation timestep number
C     myThid :: my Thread Id. number
C     iStep  :: current sub-time step iterate 
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
      INTEGER iStep
C     u/vIceLoc :: local copies of the current ice velocity
      _RL uIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL vIceLoc(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     CwatC     :: drag coefficients
      _RL CwatC(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#if ( (defined SEAICE_CGRID) && (defined SEAICE_ALLOW_DYNAMICS) )
C     === local variables ===
C     i,j,bi,bj,ksrf :: loop indices
      INTEGER i,j,bi,bj
      INTEGER kSrf
      _RL     TEMPVAR
CEOP

      kSrf=1

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+Oly-1
         DO i=1-OLx,sNx+OLy-1
C     non-linear water drag coefficients DWATN
#ifdef OBCS_UVICE_OLD
          TEMPVAR = 0.25 _d 0*(
     &            ( ( uIceLoc(i  ,j,bi,bj)-uVel(i  ,j,kSrf,bi,bj) )
     &             +( uIceLoc(i+1,j,bi,bj)-uVel(i+1,j,kSrf,bi,bj) )
     &            )**2
     &          + ( ( vIceLoc(i, j ,bi,bj)-vVel(i, j ,kSrf,bi,bj) )
     &             +( vIceLoc(i,j+1,bi,bj)-vVel(i,j+1,kSrf,bi,bj) )
     &            )**2 )
#else /* OBCS_UVICE_OLD */
          TEMPVAR = 0.25 _d 0*(
     &            ( ( uIceLoc(i  ,j,bi,bj)-uVel(i  ,j,kSrf,bi,bj) )
     &             *maskInW( i ,j,bi,bj)
     &             +( uIceLoc(i+1,J,bi,bj)-uVel(i+1,j,kSrf,bi,bj) )
     &             *maskInW(i+1,j,bi,bj) )**2
     &          + ( ( vIceLoc(i,j  ,bi,bj)-vVel(i,j  ,kSrf,bi,bj) )
     &             *maskInS(i, j ,bi,bj)
     &             +( vIceLoc(i,j+1,bi,bj)-vVel(i,j+1,kSrf,bi,bj) )
     &             *maskInS(i,j+1,bi,bj) )**2 )
#endif /* OBCS_UVICE_OLD */
          IF ( YC(I,J,bi,bj) .LT. ZERO ) THEN
           IF ( SEAICE_waterDrag_south.LE.0. ) THEN
            CwatC(I,J,bi,bj)=0.
           ELSEIF ( TEMPVAR
     &           .LE.(0.25 _d 0/SEAICE_waterDrag_south)**2 ) THEN
            CwatC(I,J,bi,bj)=0.25 _d 0
           ELSE
            CwatC(I,J,bi,bj)=SEAICE_waterDrag_south*SQRT(TEMPVAR)
           ENDIF
          ELSE
           IF ( SEAICE_waterDrag.LE.0. ) THEN
            CwatC(I,J,bi,bj)=0.
           ELSEIF ( TEMPVAR.LE.(0.25 _d 0/SEAICE_waterDrag)**2 ) THEN
            CwatC(I,J,bi,bj)=0.25 _d 0
           ELSE
            CwatC(I,J,bi,bj)=SEAICE_waterDrag*SQRT(TEMPVAR)
           ENDIF
          ENDIF
          CwatC(I,J,bi,bj) = CwatC(I,J,bi,bj) * maskC(I,J,kSrf,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

#endif /* SEAICE_ALLOW_DYNAMICS and SEAICE_CGRID */

      RETURN
      END
