C $Header: /u/gcmpack/MITgcm/model/src/Attic/balance_relax.F,v 1.1 2012/08/06 14:22:53 gforget Exp $
C $Name:  $

#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: BALANCE_RELAX
C     !INTERFACE:
      SUBROUTINE BALANCE_RELAX( myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | S/R BALANCE_RELAX                                                
C     | o balancing surface forcing relaxation of tracers
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "GRID.h"
#include "FFIELDS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine Arguments ==
C     myTime - Current time in simulation
C     myIter - Current iteration number in simulation
C     myThid :: Thread no. that called this routine.
      _RL myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_BALANCE_RELAX

C     !LOCAL VARIABLES:
C     == Local variables ==
C     Tfreezing :: Freezing threshold temperature.
      INTEGER bi,bj,i,j,k

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL tmpFac
CEOP

      IF ( usingPCoords ) THEN
        k = Nr
      ELSE
        k = 1
      ENDIF

      IF ( balanceThetaClimRelax ) THEN
      CALL GLOBAL_SUM_TILE_RL( SSTrlxTile, SSTrlxGlob, myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          surfaceForcingT(i,j,bi,bj) = surfaceForcingT(i,j,bi,bj)
     &            - SSTrlxGlob / globalArea
          SSTrlx(i,j,bi,bj) = SSTrlx(i,j,bi,bj)
     &            - SSTrlxGlob / globalArea
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF
      
      IF ( balanceSaltClimRelax ) THEN
      CALL GLOBAL_SUM_TILE_RL( SSSrlxTile, SSSrlxGlob, myThid )
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          surfaceForcingS(i,j,bi,bj) = surfaceForcingS(i,j,bi,bj)
     &            - SSSrlxGlob / globalArea
          SSSrlx(i,j,bi,bj) = SSSrlx(i,j,bi,bj)
     &            - SSSrlxGlob / globalArea
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      ENDIF

# ifdef ALLOW_DIAGNOSTICS
       IF ( useDiagnostics.AND.balanceThetaClimRelax ) THEN
C     tRelax (temperature relaxation [W/m2], positive <-> increasing Theta)
       tmpFac = HeatCapacity_Cp*rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL( SSTrlx,tmpFac,1,
     &                             'TRELAX  ',0, 1,0,1,1,myThid )
       ENDIF

       IF ( useDiagnostics.AND.balanceSaltClimRelax ) THEN
C     sRelax (salt relaxation [g/m2/s], positive <-> increasing Salt)
       tmpFac = rUnit2mass
       CALL DIAGNOSTICS_SCALE_FILL( SSSrlx,tmpFac,1,
     &                             'SRELAX  ',0, 1,0,1,1,myThid )
       ENDIF
# endif /* ALLOW_DIAGNOSTICS */

      IF ( balancePrintMean.AND.balanceThetaClimRelax ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A,A,E24.17)') 'rm Global mean of ',
     &      'SSTrlx = ', SSTrlxGlob / globalArea
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid)
       _END_MASTER( myThid )
      ENDIF

      IF ( balancePrintMean.AND.balanceSaltClimRelax ) THEN
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A,A,E24.17)') 'rm Global mean of ',
     &      'SSSrlx = ', SSSrlxGlob / globalArea
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &      SQUEEZE_RIGHT , myThid)
       _END_MASTER( myThid )
      ENDIF

#endif
      RETURN
      END
