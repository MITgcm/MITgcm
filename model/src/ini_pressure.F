#include "PACKAGES_CONFIG.h"
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
C     myThid     :: my Thread Id number
      INTEGER myThid

C     !LOCAL VARIABLES:
C     dPhiHydX,Y :: Gradient (X & Y directions) of Hyd. Potential
C     bi,bj      :: tile indices
C     i,j,k      :: Loop counters
      INTEGER bi, bj
      INTEGER  i,  j, k
#ifndef ALLOW_AUTODIFF
      INTEGER  iMin, iMax, jMin, jMax, npiter
      _RL PhiHydF (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL PhiHydC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydX(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL dPhiHydY(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL oldPhi  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL count, rmspp, rmsppold
      _RL sumTile, rmsTile
#endif

      CHARACTER*(MAX_LEN_MBUF) msgBuf
CEOP

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(a)')
     &     'Start initial hydrostatic pressure computation'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO k = 1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           totPhiHyd(i,j,k,bi,bj) = 0. _d 0
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      IF ( storePhiHyd4Phys ) THEN

#ifndef ALLOW_AUTODIFF
cph-- deal with this iterative loop for AD once it will
cph-- really be needed;
cph-- would need storing of totPhiHyd for each npiter

       iMin = 1-OLx
       iMax = sNx+OLx
       jMin = 1-OLy
       jMax = sNy+OLy

       rmspp    = 1. _d 0
       rmsppold = 0. _d 0
       npiter = 0

C     iterate pressure p = integral of (g*rho(p)*dz)
       DO npiter= 1, 15
        IF ( rmspp.GT.zeroRL ) THEN
         rmsppold = rmspp
         rmspp    = 0. _d 0
         count    = 0. _d 0
         DO bj = myByLo(myThid), myByHi(myThid)
          DO bi = myBxLo(myThid), myBxHi(myThid)
           rmsTile = 0. _d 0
           sumTile = 0. _d 0
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
              phiHydF(i,j) = 0. _d 0
            ENDDO
           ENDDO
           DO k = 1, Nr
C     for each level save old pressure and compute new pressure
            DO j=jMin,jMax
             DO i=iMin,iMax
              oldPhi(i,j) = totPhiHyd(i,j,k,bi,bj)
             ENDDO
            ENDDO
            CALL CALC_PHI_HYD(
     I           bi, bj, iMin, iMax, jMin, jMax, k,
     U           phiHydF,
     O           phiHydC, dPhiHydX, dPhiHydY,
     I           startTime, -1, myThid )
C     compute convergence criterion
            DO j=1,sNy
             DO i=1,sNx
              rmsTile = rmsTile
     &            + (totPhiHyd(i,j,k,bi,bj)-oldPhi(i,j))**2
     &             * maskC(i,j,k,bi,bj)
              sumTile = sumTile + maskC(i,j,k,bi,bj)
             ENDDO
            ENDDO
C --      end k loop
           ENDDO
           rmspp = rmspp + rmsTile
           count = count + sumTile
          ENDDO
         ENDDO
Cml        WRITE(msgBuf,'(I10.10)') npiter
Cml        CALL WRITE_FLD_XYZ_RL( 'POLD.',msgBuf,pold,npiter,myThid)
Cml        CALL WRITE_FLD_XYZ_RL( 'PINI.',msgBuf,pressure,npiter,myThid)
         _GLOBAL_SUM_RL( rmspp, myThid )
         _GLOBAL_SUM_RL( count, myThid )
         IF ( count .EQ. 0. ) THEN
           rmspp = 0. _d 0
         ELSE
           rmspp = SQRT(rmspp/count)
         ENDIF
         _BEGIN_MASTER( myThid )
         WRITE(msgBuf,'(A,I4,A,1PE20.12)')
     &        'Iteration', npiter, ', RMS-difference =', rmspp
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         _END_MASTER( myThid )

        ENDIF
C --   end npiter loop
       ENDDO

C     print some diagnostics
       _BEGIN_MASTER( myThid )
       IF ( rmspp .NE. 0. ) THEN
        IF ( rmspp .EQ. rmsppold ) THEN
         WRITE(msgBuf,'(A)')
     &      'Initial hydrostatic pressure did not converge perfectly,'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)')
     &      'but the RMS-difference is constant, indicating that the'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
         WRITE(msgBuf,'(A)')
     &      'algorithm converged within machine precision.'
         CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                       SQUEEZE_RIGHT, myThid )
        ELSE
         WRITE(msgBuf,'(A,I2,A)')
     &       'Initial hydrostatic pressure did not converge after ',
     &          npiter-1, ' steps'
         CALL PRINT_ERROR( msgBuf, myThid )
         STOP 'ABNORMAL END: S/R INI_PRESSURE'
        ENDIF
       ENDIF
       WRITE(msgBuf,'(A)')
     &     'Initial hydrostatic pressure converged.'
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )

#endif /* ALLOW_AUTODIFF */

c-- else of if storePhiHyd4Phys
      ELSE
C     print a message and DO nothing
       _BEGIN_MASTER( myThid )
       WRITE(msgBuf,'(A,A)')
     &        'Pressure is predetermined for buoyancyRelation ',
     &        buoyancyRelation(1:11)
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                     SQUEEZE_RIGHT, myThid )
       _END_MASTER( myThid )

      ENDIF

      _BEGIN_MASTER( myThid )
      WRITE(msgBuf,'(A)') ' '
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT, myThid )
      _END_MASTER( myThid )

      RETURN
      END
