C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_diagnostics_fill.F,v 1.1 2005/01/04 00:25:31 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DIAGNOSTICS_FILL
C     !INTERFACE:
      SUBROUTINE THSICE_DIAGNOSTICS_FILL( 
     I                      frwAtm, flxAtm,
     I                      myTime, myIter, bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_DIAGNOSTICS_FILL                                              
C     | o fill-in diagnostics array: 
C     |   call diagnostics_fill for each THSICE field
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     frwAtm  :: net fresh-water flux (E-P) to the atmosphere [kg/m2/s]
C     flxAtm  :: net heat flux from the atmosphere (+=down) [W/m2]
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     bi,bj   :: tile indices
C     myThid  :: thread number for this instance of the routine.
      _RL frwAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL flxAtm(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL  myTime
      INTEGER myIter
      INTEGER bi,bj
      INTEGER myThid
CEOP

#ifdef ALLOW_DIAGNOSTICS
C     == Local variables ==
      _RL tmpFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER i,j
      LOGICAL  DIAGNOSTICS_IS_ON
      EXTERNAL DIAGNOSTICS_IS_ON

      IF ( useDiagnostics ) THEN

        CALL DIAGNOSTICS_FILL(iceMask,'SI_Fract',0,1,1,bi,bj,myThid)

       IF ( DIAGNOSTICS_IS_ON('SI_Thick',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*iceheight(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Fract',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_SnowH',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*snowheight(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_SnowH',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_Tsrf ',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*Tsrf(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Tsrf ',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_Tice1',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*Tice1(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Tice1',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_Tice2',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*Tice2(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Tice2',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_Qice1',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*iceheight(i,j,bi,bj)
     &                                    *Qice1(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Qice1',0,1,2,bi,bj,myThid)
       ENDIF

       IF ( DIAGNOSTICS_IS_ON('SI_Qice2',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*iceheight(i,j,bi,bj)
     &                                    *Qice2(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SI_Qice2',0,1,2,bi,bj,myThid)
       ENDIF

C- Pb: a) iceMask & albedo are not at the same time-step.
C         same Pb with snowPrecip
C      b) albedo comes either from S/R AIM_SICE2AIM (fluildIsAir)
C                          or from S/R THSICE_STEP_FWD (fluidIsWater)
c      IF ( DIAGNOSTICS_IS_ON('SIalbedo',myThid) ) THEN
c       DO j=1,sNy
c        DO i=1,sNx
c         tmpFld(i,j) = iceMask(i,j,bi,bj)*Tice2(i,j,bi,bj)
c        ENDDO
c       ENDDO
c       CALL DIAGNOSTICS_FILL(tmpFld,'SIalbedo',0,1,2,bi,bj,myThid)
c      ENDIF

        CALL DIAGNOSTICS_FILL(snowAge,'SIsnwAge',0,1,1,bi,bj,myThid)

       IF ( DIAGNOSTICS_IS_ON('SIsnwPrc',myThid) ) THEN
        DO j=1,sNy
         DO i=1,sNx
          tmpFld(i,j) = iceMask(i,j,bi,bj)*snowPrc(i,j,bi,bj)
         ENDDO
        ENDDO
        CALL DIAGNOSTICS_FILL(tmpFld,'SIsnwPrc',0,1,2,bi,bj,myThid)
       ENDIF

C-- Atmospheric fluxes : 
       IF ( fluidIsWater ) THEN
        CALL DIAGNOSTICS_FILL(flxAtm,'SIflxAtm',0,1,2,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(flxAtm,'SIfrwAtm',0,1,2,bi,bj,myThid)
       ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Oceanic fluxes : 
c #ifndef CHECK_ENERGY_CONSERV
        CALL DIAGNOSTICS_FILL(Qnet,    'SIflx2oc',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(EmPmR,   'SIfrw2oc',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(saltFlux,'SIsaltFx',0,1,1,bi,bj,myThid)
c #endif /* CHECK_ENERGY_CONSERV */

C-- Ocean Mixed-Layer temp. & salinity
       IF ( stepFwd_oceMxL ) THEN
        CALL DIAGNOSTICS_FILL(tOceMxL,'SItOcMxL',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(sOceMxL,'SIsOcMxL',0,1,1,bi,bj,myThid)
       ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
