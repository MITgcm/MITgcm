
#include "SEAICE_OPTIONS.h"

C !ROUTINE: SEAICE_ITD_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE SEAICE_ITD_PICKUP(
     I     myIter, myThid )

C !DESCRIPTION: \bv
C     *===========================================================*
C     | SUBROUTINE SEAICE_ITD_PICKUP
C     | o called in case pickup file does not contain
C     |   ITD variables but mean ice thickness and concentration
C     |
C     | o choose between two schemes:
C     |
C     |   a) a simple scheme where the mean values are just put
C     |      into the first ITD category and then redustributed
C     |      into the correct category by SEAICE_ITD_REDIST
C     |      -> simpleSchemeFlag = .TRUE.
C     |
C     |   b) a scheme that assumes a log-normal distribution based
C     |      on the mean ice thickness and a standard decviation 
C     |      of LND_sigma=0.25
C     |      -> simpleSchemeFlag = .FALSE.
C     |
C     | Torge Martin, Mai 2012, torge@mit.edu
C     *===========================================================*
C \ev

C !USES: ===============================================================
      IMPLICIT NONE

C     === Global variables needed ===
C     AREA      :: total sea ice area fraction
C     HEFF      :: mean in-situ sea ice thickness
C     HSNOW     :: mean in-situ snow layer depth
C
C     === Global variables to be changed ===
C     AREAITD   :: sea ice area      by category
C     HEFFITD   :: sea ice thickness by category
C     HSNOWITD  :: snow thickness    by category
C
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SEAICE_SIZE.h"
#include "SEAICE_PARAMS.h"
#include "SEAICE.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     === Routine arguments ===
C     myIter    :: iteration number
C     myThid    :: Thread no. that called this routine.
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#ifdef SEAICE_ITD

C !LOCAL VARIABLES: ====================================================
C     === Local variables ===
C     i,j,bi,bj,k :: Loop counters
C     nITD        :: number of sea ice thickness categories
C
      INTEGER i, j, bi, bj, k
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER itmpkey
#endif /* ALLOW_AUTODIFF_TAMC */
      _RL dummyTime

C     local variables for picking up ITD from single category pickup file 
      INTEGER LND_i, LND_iend
C     parameters for log-normal distribution (LND)
      _RL LND_sigma, LND_mu
      PARAMETER(LND_sigma=0.25)
      _RL LND_dx
      _RL LND_tmp
C     bin width of distribution
      PARAMETER(LND_dx=0.1)
      PARAMETER(LND_iend=INT(100./LND_dx))
      _RL LND_x  (INT(LND_iend))
      _RL LND_pdf(INT(LND_iend))
C     flag for pickup scheme
      LOGICAL simpleSchemeFlag

      simpleSchemeFlag = .TRUE.
      dummyTime = 1.0

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      IF (simpleSchemeFlag) THEN
C--      Put all ice into one bin:
C
         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
             AREAITD(i,j,1,bi,bj)  = AREA(i,j,bi,bj)
             HEFFITD(i,j,1,bi,bj)  = HEFF(i,j,bi,bj)
             HSNOWITD(i,j,1,bi,bj) = HSNOW(i,j,bi,bj)
            ENDDO
           ENDDO
          ENDDO
         ENDDO

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
      ELSE
C--      Assume log-normal ITD:

         DO bj=myByLo(myThid),myByHi(myThid)
          DO bi=myBxLo(myThid),myBxHi(myThid)
           DO j=1-OLy,sNy+OLy
            DO i=1-OLx,sNx+OLx
C
C            initialize log-normal distribution
       	     LND_mu = log(HEFF(i,j,bi,bj)/AREA(i,j,bi,bj)) 
     &                - 0.5*LND_sigma*LND_sigma
	     LND_x(1) = 0.+LND_dx/2.
C            make thickness bins
	     DO LND_i=2,LND_iend
	      LND_x(LND_i)=LND_x(LND_i-1)+LND_dx
	     ENDDO
C            log-normal distribution:
	     DO LND_i=2,LND_iend
	      LND_tmp = log(LND_x(LND_i))-LND_mu
	      LND_pdf(LND_i)= 1.
     &             / (LND_x(LND_i)*LND_sigma*sqrt(2*3.1416)) 
     &             * exp( -(LND_tmp*LND_tmp)
     &             /       (2*LND_sigma*LND_sigma) )
     &             * AREA(i,j,bi,bj)
	     ENDDO
C            assign bins to ice thickness categories
             k=1
	     DO LND_i=1,LND_iend
	      IF ( LND_x(LND_i).GT.Hlimit(k) ) k=k+1
              AREAITD(i,j,k,bi,bj) = AREAITD(i,j,k,bi,bj)
     &                             + LND_pdf(LND_i)*LND_dx
              HEFFITD(i,j,k,bi,bj) = HEFFITD(i,j,k,bi,bj)
     &                             + LND_pdf(LND_i)*LND_x(LND_i)*LND_dx
	     ENDDO
C
            ENDDO
	   ENDDO
          ENDDO
	 ENDDO

      ENDIF

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     finally sort into correct ice thickness category 
C      and compute bulk variables 
C      (needed for dynamic solver at beginning of seaice_model.F)
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        CALL SEAICE_ITD_REDIST( bi, bj, dummyTime, myIter, myThid)
        CALL SEAICE_ITD_SUM( bi, bj, dummyTime, myIter, myThid)
       ENDDO
      ENDDO

C---+-|--1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* SEAICE_ITD */
      RETURN
      END SUBROUTINE SEAICE_ITD_PICKUP

