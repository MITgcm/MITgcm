C $Header: 
C $Name:  

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_PRESSURE
C     !INTERFACE:
      SUBROUTINE INI_PRESSURE( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_PRESSURE
C     | o initialise the pressure field consistently with 
C     |   temperature and salinity
C     |   - needs to be called after ini_theta, ini_salt, and 
C     |     ini_psurf
C     |   - does not include surface pressure loading, because
C     |     that is only available until after 
C     |     CALL packages_init_variables
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "EOS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid -  Number of this instance of INI_PRESSURE
      INTEGER myThid

C     !LOCAL VARIABLES:
C     == Local variables ==
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hyd. Potential
C     bi,bj  - Loop counters
C     I,J,K
      INTEGER bi, bj
      INTEGER  I,  J, K
      INTEGER ncount, iMin, iMax, jMin, jMax, npiter
      _RL phiHyd(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr) 
      _RL dPhiHydX(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL dPhiHydY(1-Olx:sNx+Olx,1-Oly:sNy+Oly)
      _RL pold(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr)
      _RL rmspp, rmsppold 

      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP
      iMin = 1-OLx
      iMax = sNx+OLx
      jMin = 1-OLy
      jMax = sNy+OLy
      
      WRITE(msgBuf,'(a)')
     &     'Start initial hydrostatic pressure computation'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)
C     initialise pressure to water column thickness times a constant
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           phiHyd(i,j,k) = 0. _d 0
           dPhiHydX(i,j) = 0. _d 0
           dPhiHydY(i,j) = 0. _d 0
          ENDDO
         ENDDO
         CALL STORE_PRESSURE(  bi, bj, k, phiHyd, myThid )
        ENDDO
       ENDDO
      ENDDO

      IF ( buoyancyRelation .EQ. 'OCEANIC' ) THEN

       rmspp    = 1. _d 0
       rmsppold = 0. _d 0
       npiter = 0

C     iterate pressure p = integral of (g*rho(p)*dz)
       DO npiter = 1, 15
        rmsppold = rmspp
        rmspp    = 0. _d 0
        ncount   = 0
        DO bj = myByLo(myThid), myByHi(myThid)
         DO bi = myBxLo(myThid), myBxHi(myThid)
          DO k = 1, Nr
C     for each level save old pressure and compute new pressure 
           DO j=jMin,jMax
            DO i=iMin,iMax
             pold(i,j,k) = pressure(i,j,k,bi,bj)
            ENDDO
           ENDDO
           CALL CALC_PHI_HYD(
     I          bi, bj, iMin, iMax, jMin, jMax, k,
     I          theta, salt, 
     U          phiHyd, 
     O          dPhiHydX, dPhiHydY,
     I          startTime, nIter0, myThid)
           CALL STORE_PRESSURE(  bi, bj, k, phiHyd, myThid )
          ENDDO
C     compute convergence criterion
          DO k = 1, Nr
           DO j=jMin,jMax
            DO i=iMin,iMax
             rmspp = rmspp 
     &            + (pressure(i,j,k,bi,bj)-pold(i,j,k))**2
             ncount = ncount + int(maskC(i,j,k,bi,bj))
            ENDDO
           ENDDO
          ENDDO
         ENDDO
        ENDDO
Cml        WRITE(msgBuf,'(I10.10)') npiter
Cml        CALL WRITE_FLD_XYZ_RL( 'POLD.',msgBuf,pold,npiter,myThid)
Cml        CALL WRITE_FLD_XYZ_RL( 'PINI.',msgBuf,pressure,npiter,myThid)
        IF ( ncount .EQ. 0 ) THEN
           rmspp = 0. _d 0
        ELSE
           rmspp = sqrt(rmspp/ncount)
        ENDIF
        WRITE(msgBuf,'(a,i2,a,e20.13)')
     &       'Iteration ', npiter, ', RMS-difference = ', rmspp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &       SQUEEZE_RIGHT , 1)

       ENDDO
C     print some diagnostics	
       IF ( rmspp .ne. 0. ) THEN
        IF ( rmspp .EQ. rmsppold ) THEN
         WRITE(msgBuf,'(a)')
     &      'Initial hydrostatic pressure did not converge perfectly,' 
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(a)')
     &      'but the RMS-difference is constant, indicating that the'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
         WRITE(msgBuf,'(a)')
     &      'algorithm converged within machine precision.'    
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &        SQUEEZE_RIGHT , 1)
        ELSE
         WRITE(msgBuf,'(a,i2,a)')
     &       'Initial hydrostatic pressure did not converge after ', 
     &          npiter-1, ' steps'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PRESSURE'
        ENDIF
       ENDIF
       WRITE(msgBuf,'(a)')
     &     'Initial hydrostatic pressure converged.' 
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &      SQUEEZE_RIGHT , 1)

      ELSE
C     print a message and DO nothing
       WRITE(msgBuf,'(a,a)')
     &        'Pressure is predetermined for buoyancyRelation ',
     &        buoyancyRelation(1:11)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &      SQUEEZE_RIGHT , 1)

      ENDIF

      WRITE(msgBuf,'(a)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit, 
     &     SQUEEZE_RIGHT , 1)

      RETURN 
      END
