C $Header: /u/gcmpack/MITgcm/pkg/aim_v23/aim_initialise.F,v 1.4 2003/02/26 03:10:50 jmc Exp $
C $Name:  $

#include "AIM_OPTIONS.h"

      SUBROUTINE AIM_INITIALISE( myThid )
C     *==================================================================*
C     | S/R AIM_INITIALISE
C     *==================================================================*
C     | Initialisation of AIM atmospheric physics package :
C     | 1) call iniphys (=> set parameters to default value)
C     | 2) read AIM parameters 
C     | 3) initialisation of AIM arrays (time-ave)
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
      
C--   Initialise surface forcing fields (in AIM_FFIELDS.h):
c     DO bj = myByLo(myThid), myByHi(myThid)
c      DO bi = myBxLo(myThid), myBxHi(myThid)
c       DO j=1-Oly,sNy+Oly
c        DO i=1-Olx,sNx+Olx  
c         aim_surfTemp(i,j,bi,bj) = 300.
c         aim_soilWater(i,j,bi,bj) = 0.
c         aim_albedo   (i,j,bi,bj) = 0.
c         aim_landFr   (i,j,bi,bj) = 0.
c        ENDDO
c       ENDDO
c      ENDDO
c     ENDDO

#ifdef ALLOW_AIM_TAVE 
C     Initialise diagnostic counters (these are cleared on model start
C      i.e. not loaded from history file for now ).
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        CALL TIMEAVE_RESET(USTRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(VSTRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(TSRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(OLRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SSRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SLRtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(SHFtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(EVAPtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(PRECNVtave,1, bi, bj, myThid)
        CALL TIMEAVE_RESET(PRECLStave,1, bi, bj, myThid)
        CALL TIMEAVE_RESET(CLOUDCtave,1, bi, bj, myThid)
        CALL TIMEAVE_RESET(CLTOPtave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(CBMFtave,  1, bi, bj, myThid)
        CALL TIMEAVE_RESET(DRAGtave,  1, bi, bj, myThid)
        CALL TIMEAVE_RESET(aimV0tave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(aimT0tave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(aimQ0tave, 1, bi, bj, myThid)
        CALL TIMEAVE_RESET(aimRHtave,Nr, bi, bj, myThid)
        DO k=1,Nr
          aim_timeAve(k,bi,bj) = 0.
        ENDDO
       ENDDO
      ENDDO
#endif /* ALLOW_AIM_TAVE */

#endif /* ALLOW_AIM */

      RETURN
      END
