C $Header: /u/gcmpack/MITgcm/pkg/thsice/Attic/thsice_diagnostics_fill.F,v 1.2 2005/01/31 19:42:59 jmc Exp $
C $Name:  $

#include "THSICE_OPTIONS.h"

CBOP
C     !ROUTINE: THSICE_DIAGNOSTICS_FILL
C     !INTERFACE:
      SUBROUTINE THSICE_DIAGNOSTICS_FILL( 
     I                      myTime, myIter, bi, bj, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R  THSICE_DIAGNOSTICS_FILL                                              
C     | o fill-in diagnostics array for THSICE state variables 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
c #include "FFIELDS.h"
#include "THSICE_PARAMS.h"
#include "THSICE_VARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime  :: time counter for this thread
C     myIter  :: iteration counter for this thread
C     bi,bj   :: tile indices
C     myThid  :: thread number for this instance of the routine.
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
        CALL DIAGNOSTICS_FILL(snowAge,'SIsnwAge',0,1,1,bi,bj,myThid)

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

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-- Ocean Mixed-Layer temp. & salinity
       IF ( stepFwd_oceMxL ) THEN
        CALL DIAGNOSTICS_FILL(tOceMxL,'SItOcMxL',0,1,1,bi,bj,myThid)
        CALL DIAGNOSTICS_FILL(sOceMxL,'SIsOcMxL',0,1,1,bi,bj,myThid)
       ENDIF

      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

      RETURN
      END
