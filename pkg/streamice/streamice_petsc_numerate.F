C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_petsc_numerate.F,v 1.3 2014/03/26 17:49:51 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_PETSC_NUMERATE (myThid)

C     /============================================================\
C     | SUBROUTINE                                                 |
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "GRID.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#ifdef ALLOW_PETSC
# ifdef ALLOW_USE_MPI
#  include "EESUPPORT.h"
# endif
#endif

      INTEGER myThid

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, ki, kj
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_USE_MPI
      integer mpiRC, mpiMyWid
#endif
#ifdef ALLOW_PETSC
      _RS DoFCount
      integer n_dofs_proc_loc (0:nPx*nPy-1)
      integer n_dofs_cum_sum (0:nPx*nPy-1)


      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
           streamice_petsc_dofs_u (i,j,bi,bj) = -2.0
           streamice_petsc_dofs_v (i,j,bi,bj) = -2.0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DoFCount = -1.0
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx


C   DOFS ARE NUMBERED AS FOLLOWS ON PROCESSOR DOMAIN:
C    grid is stepped through in order bj, bi, j, i
C    1) if umask(i,j,bi,bj)==1, the counter is updated by 1;
C        streamice_petsc_dofs_u is assigned the counter;
C        o/w  streamice_petsc_dofs_u is assigned -1
C    2) if vmask(i,j,bi,bj)==1, the counter is updated by 1;
C        streamice_petsc_dofs_v is assigned the counter;
C        o/w  streamice_petsc_dofs_v is assigned -1
C    NOTE THESE NUMBERING ARRAYS ARE USED TO CONSTRUCT PETSC VECTORS AND MATRIX

          if (STREAMICE_umask (i,j,bi,bj).eq.1.0) THEN
           DoFCount = DoFCount + 1.0
           streamice_petsc_dofs_u (i,j,bi,bj) = DoFCount
          else
           streamice_petsc_dofs_u (i,j,bi,bj) = -1.0
          endif

          if (STREAMICE_vmask (i,j,bi,bj).eq.1.0) THEN
           DoFCount = DoFCount + 1.0
           streamice_petsc_dofs_v (i,j,bi,bj) = DoFCount
          else
           streamice_petsc_dofs_v (i,j,bi,bj) = -1.0
          endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      print *, "DOF_COUNT", dofcount

#ifdef ALLOW_USE_MPI

      DO i=0,nPx*nPy-1
       n_dofs_proc_loc (i) = 0
      ENDDO

      CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpiMyWId, mpiRC )

      n_dofs_proc_loc (mpiMyWId) = INT(DoFCount)+1

      CALL MPI_Allreduce(n_dofs_proc_loc,n_dofs_process,nPx*nPy,
     &       MPI_INTEGER, MPI_SUM,MPI_COMM_MODEL,mpiRC)

      n_dofs_cum_sum(0) = 0

      DO i=1,nPx*nPy-1
       n_dofs_cum_sum(i) = n_dofs_cum_sum(i-1)+
     &                     n_dofs_process(i-1)
      ENDDO

#else /* ALLOW_USE_MPI */

      n_dofs_process (0) = INT(DoFCount)+1
      n_dofs_cum_sum (0) = INT(DoFCount)+1

#endif /* ALLOW_USE_MPI */

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF (streamice_petsc_dofs_u(i,j,bi,bj).ge.0 ) THEN
           streamice_petsc_dofs_u(i,j,bi,bj) =
     &      streamice_petsc_dofs_u(i,j,bi,bj) +
     &      n_dofs_cum_sum(mpimywid)
          ENDIF
          IF (streamice_petsc_dofs_v(i,j,bi,bj).ge.0 ) THEN
           streamice_petsc_dofs_v(i,j,bi,bj) =
     &      streamice_petsc_dofs_v(i,j,bi,bj) +
     &      n_dofs_cum_sum(mpimywid)
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RS(streamice_petsc_dofs_u,myThid)
      _EXCH_XY_RS(streamice_petsc_dofs_v,myThid)


#endif /* ALLOW_PETSC */


#endif
      RETURN
      END
