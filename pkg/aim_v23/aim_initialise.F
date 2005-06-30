C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_initialise.F,v 1.10 2005/06/30 23:09:08 molod Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_INITIALISE( myThid )
C     *==================================================================*
C     | S/R AIM_INITIALISE
C     *==================================================================*
C     | Initialisation of AIM atmospheric physics package :
C     | 1) call iniphys (=> set parameters to default value)
C     | 2) read AIM parameters 
C     *==================================================================*
      IMPLICIT NONE

C     -------------- Global variables ------------------------------------
#include "SIZE.h" 
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"
#include "DYNVARS.h"
#include "AIM_PARAMS.h"
#include "AIM_FFIELDS.h"
c #include "AIM_GRID.h"
#include "AIM_DIAGS.h"
#ifdef ALLOW_MNC
#include "MNC_PARAMS.h"
#endif

C     == Routine arguments ==
C     myThid -  Number of this instance
      INTEGER myThid

#ifdef ALLOW_AIM
C     == Local variables ==
C     HSG  - Cell face in vertical
C     pGround - Lower boundary pressure
C     i, j, k, bi,bj  - Loop counters
      _RL HSG(0:Nr)
      _RL pGround, tmpPgrnd
      INTEGER i, j, K, bi, bj
      INTEGER Katm

C--  Set default value for AIM interface code (AIM_PARAMS.h):
      aim_useFMsurfBC = .TRUE.
      aim_useMMsurfFc = .FALSE.
      aim_surfPotTemp = .FALSE.
      aim_energPrecip = .FALSE.
      aim_splitSIOsFx = .FALSE.
      aim_clrSkyDiag  = .FALSE.
      aim_tave_mdsio  = .TRUE.
#ifdef ALLOW_MNC
      aim_tave_mnc    = timeave_mnc
#else
      aim_tave_mnc    = .FALSE.
#endif
      aim_MMsufx = '.bin'
      aim_MMsufxLength = 4
      aim_LandFile = ' '
      aim_albFile  = ' '
      aim_vegFile  = ' '
      aim_sstFile  = ' '
      aim_lstFile  = ' '
      aim_oiceFile = ' '
      aim_snowFile = ' '
      aim_swcFile  = ' '
      aim_dragStrato = 0.
      aim_taveFreq = taveFreq
      aim_diagFreq = dumpFreq
      aim_tendFreq = 0.

C--  Set default value for atmos. physics parameters:
      pGround = atm_Po
      DO k=1,Nr
       Katm = _KD2KA( k )
       HSG(Katm) = rF(k)/pGround
      ENDDO
       k=Nr+1
       Katm = _KD2KA( k )
       HSG(Katm) = rF(k)/pGround

c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)

C--   set default value for all atmos. physics parameter:
        CALL INPHYS( HSG, myThid )

c      ENDDO
c     ENDDO

C--   Read AIM parameters (from file data.aimphys):
      CALL AIM_READPARMS( myThid )

C--   set energy fractions in LW bands as a function of temperature:
C     initialize common block RADFIX (originally called from FORDATE in SPEEDY)
      _BEGIN_MASTER(myThid)  
       CALL RADSET( myThid)
      _END_MASTER ( myThid)  

C--   Set truncSurfP : used to correct for truncation (because of hFacMin)
C      of surface reference pressure Ro_surf that affects Surf.Temp.
      CALL INI_P_GROUND(1, topoZ, truncSurfP, myThid )
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          tmpPgrnd = MIN(truncSurfP(i,j,bi,bj),atm_Po)
          truncSurfP(i,j,bi,bj)= 
     &     ( Ro_surf(i,j,bi,bj)/tmpPgrnd )**atm_kappa
         ENDDO
        ENDDO
        IF (aim_useMMsurfFc .AND. aim_surfPotTemp) THEN
         DO j=1,sNy
          DO i=1,sNx
           truncSurfP(i,j,bi,bj) =
     &      ( Ro_surf(i,j,bi,bj)/atm_Po )**atm_kappa
          ENDDO
         ENDDO
        ENDIF
       ENDDO
      ENDDO
      
C--   Initialise Land Fraction (in AIM_FFIELDS.h):
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-Oly,sNy+Oly
         DO i=1-Olx,sNx+Olx  
          aim_landFr   (i,j,bi,bj) = 0.
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( aim_LandFile .NE. ' '  ) THEN
         CALL READ_REC_XY_RS(aim_LandFile,aim_landFr,1,nIter0,myThid)
      ENDIF

#ifdef ALLOW_MNC
      IF (useMNC) THEN
        CALL AIM_MNC_INIT( myThid )
      ENDIF
#endif /*  ALLOW_MNC  */

#ifdef ALLOW_DIAGNOSTICS
      if ( useDiagnostics ) then
        call aim_diagnostics_init( myThid )
      endif
#endif

#endif /* ALLOW_AIM */

      RETURN
      END
