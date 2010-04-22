C $Header: /u/gcmpack/MITgcm/pkg/autodiff/global_sum_ad.F,v 1.9 2010/04/22 22:23:37 jmc Exp $
C $Name:  $

#include "AUTODIFF_OPTIONS.h"

C--   File global_adsum.F: Routines that perform adjoint of
C                          global sum on an array of thread values.
C      Contents
C      o GLOBAL_ADSUM_R4
C      o GLOBAL_ADSUM_R8

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_ADSUM_R4

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE GLOBAL_ADSUM_R4(
     I                            myThid,
     U                            adsumPhi
     &                          )
#else
      SUBROUTINE GLOBAL_ADSUM_R4(
     U                            adsumPhi,
     I                            myThid
     &                          )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_ADSUM_R4
C     | o Handle sum for real*4 data.
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*4 adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     adphi  :: Array to be summed.
C     I      :: Loop counters
C     mpiRC  :: MPI return code
      Real*4 adphi(lShare4,MAX_NO_THREADS)
      INTEGER I
      Real*4  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      tmp = adsumPhi

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
         CALL MPI_Bcast(tmp, 1, MPI_REAL, 0
     &        , MPI_COMM_MODEL, mpiRC
     &        )
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C---- Testing stage: print a warning (both to std err & outp) when
C     current Proc input value is different from Proc-0 value
      IF ( tmp.NE.adsumPhi ) THEN
C-    might need to improve this test if some MPI truncation happen
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_R4: ** WARNING ** input =', adsumPhi
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_R4: ** WARNING ** output=', tmp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
C----

      DO I=1,nThreads
         adphi(1,I) = tmp
      ENDDO

      _END_MASTER( myThid )
C--
      _BARRIER
C
C--   every thread takes its adjoint sum
      adsumPhi = adphi(1,myThid)

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP
C     !ROUTINE: GLOBAL_ADSUM_R8

C     !INTERFACE:
#ifdef AUTODIFF_TAMC_COMPATIBILITY
      SUBROUTINE GLOBAL_ADSUM_R8(
     I                            myThid,
     U                            adsumPhi
     &                          )
#else
      SUBROUTINE GLOBAL_ADSUM_R8(
     U                            adsumPhi,
     I                            myThid
     &                          )
#endif

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GLOBAL_ADSUM_R8
C     | o Handle sum for real*8 data.
C     *==========================================================*
C     *==========================================================*

C     !USES:
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     sumPhi :: Result of sum.
C     myThid :: My thread id.
      Real*8 adsumPhi
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     == Local variables ==
C     adphi  :: Array to be summed.
C     I      :: Loop counters
C     mpiRC  :: MPI return code
      Real*8 adphi(lShare8,MAX_NO_THREADS)
      INTEGER I
      Real*8  tmp
#ifdef   ALLOW_USE_MPI
      INTEGER mpiRC
#endif /* ALLOW_USE_MPI */
      CHARACTER*(MAX_LEN_MBUF) msgBuf

C--   Can not start until everyone is ready
      _BARRIER

C--   broadcast to all processes
      _BEGIN_MASTER( myThid )

      tmp = adsumPhi

#ifdef  ALLOW_USE_MPI
#ifndef ALWAYS_USE_MPI
      IF ( usingMPI ) THEN
#endif
         CALL MPI_Bcast(tmp, 1, MPI_DOUBLE_PRECISION, 0
     &        , MPI_COMM_MODEL, mpiRC
     &        )
#ifndef ALWAYS_USE_MPI
      ENDIF
#endif
#endif /*  ALLOW_USE_MPI */

C---- Testing stage: print a warning (both to std err & outp) when
C     current Proc input value is different from Proc-0 value
      IF ( tmp.NE.adsumPhi ) THEN
C-    might need to improve this test if some MPI truncation happen
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_R8: ** WARNING ** input =', adsumPhi
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        WRITE(msgBuf,'(A,1PE22.14)')
     &    'GLOBAL_ADSUM_R8: ** WARNING ** output=', tmp
        CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
        CALL PRINT_MESSAGE( msgBuf, errorMessageUnit,
     &                      SQUEEZE_RIGHT, myThid )
      ENDIF
C----

      DO I=1,nThreads
         adphi(1,I) = tmp
      ENDDO

      _END_MASTER( myThid )
C--
      _BARRIER
C
C--   every thread takes its adjoint sum
      adsumPhi = adphi(1,myThid)

      RETURN
      END
