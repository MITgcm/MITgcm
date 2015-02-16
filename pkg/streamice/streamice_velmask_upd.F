C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_velmask_upd.F,v 1.9 2015/02/16 16:46:45 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_VELMASK_UPD ( myThid )

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
!#ifdef ALLOW_PETSC
!# ifdef ALLOW_USE_MPI
!#  include "EESUPPORT.h"
!# endif
!#endif
! #include "STREAMICE_ADV.h"

#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
#endif

      INTEGER myThid

#ifdef ALLOW_STREAMICE

      INTEGER i, j, bi, bj, ki, kj
      INTEGER maskFlag, myThidCopy
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#ifdef ALLOW_USE_MPI
      integer mpiRC, mpiMyWid
#endif
#ifdef ALLOW_PETSC
      _RS DoFCount
      integer n_dofs_proc_loc (0:nPx*nPy-1)
      integer n_dofs_cum_sum (0:nPx*nPy-1)
#endif

      _EXCH_XY_RL( H_streamice, myThid )
      _EXCH_XY_RL( area_shelf_streamice, myThid )
      _EXCH_XY_RS( STREAMICE_hmask, myThid )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          STREAMICE_umask(i,j,bi,bj) = -1. _d 0
          STREAMICE_vmask(i,j,bi,bj) = -1. _d 0
          STREAMICE_ufacemask(i,j,bi,bj) = 0. _d 0
          STREAMICE_vfacemask(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=0,sNy+1
         DO i=0,sNx+1
          IF (STREAMICE_hmask(i,j,bi,bj) .eq. 1.0) THEN

           DO kj=0,1
            DO ki=0,1
             if (STREAMICE_umask(i+ki,j+kj,bi,bj).eq.-1.0) then
              STREAMICE_umask (i+ki,j+kj,bi,bj) = 1.0
             endif
             if (STREAMICE_vmask(i+ki,j+kj,bi,bj).eq.-1.0) then
              STREAMICE_vmask (i+ki,j+kj,bi,bj) = 1.0
             endif
            ENDDO
           ENDDO

           DO ki=0,1
            maskFlag=INT(STREAMICE_ufacemask_bdry(i+ki,j,bi,bj))
             IF (maskFlag.EQ.3) THEN
              DO kj=0,1
               if (STREAMICE_umask(i+ki,j+kj,bi,bj).ne.0.0) then
                STREAMICE_umask(i+ki,j+kj,bi,bj) = 3.0
               endif
               if(STREAMICE_vmask(i+ki,j+kj,bi,bj).ne.0.0) then
                STREAMICE_vmask(i+ki,j+kj,bi,bj) = 3.0
               endif
              ENDDO
              STREAMICE_ufacemask(i+ki,j,bi,bj) = 3.0
             ELSE IF (maskFlag.EQ.2) THEN
              !DO kj=0,1
               STREAMICE_ufacemask(i+ki,j,bi,bj) = 2.0
              !ENDDO
             ELSE IF (maskFlag.EQ.4) THEN
              DO kj=0,1
               STREAMICE_umask(i+ki,j+kj,bi,bj) = 0.0
               STREAMICE_vmask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
              STREAMICE_ufacemask(i+ki,j,bi,bj) = 4.0
             ELSE IF (maskFlag.EQ.0) THEN
              DO kj=0,1
               STREAMICE_umask(i+ki,j+kj,bi,bj) = 0.0
               STREAMICE_vmask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
              STREAMICE_ufacemask(i+ki,j,bi,bj) = 0.0
             ELSE IF (maskFlag.EQ.1) THEN
              DO kj=0,1
               STREAMICE_umask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
            END IF
           ENDDO

           DO kj=0,1
            maskFlag=INT(STREAMICE_vfacemask_bdry(i,j+kj,bi,bj))
            IF (maskFlag.EQ.3) THEN
              DO ki=0,1
               if(STREAMICE_vmask(i+ki,j+kj,bi,bj).ne.0.0) then
                STREAMICE_vmask(i+ki,j+kj,bi,bj) = 3.0
               endif
               if(STREAMICE_umask(i+ki,j+kj,bi,bj).ne.0.0) then
                STREAMICE_umask(i+ki,j+kj,bi,bj) = 3.0
               endif
              ENDDO
              STREAMICE_vfacemask(i,j+kj,bi,bj) = 3.0
           ELSE IF (maskFlag.EQ.2) THEN
              !DO ki=0,1
               STREAMICE_vfacemask(i,j+kj,bi,bj) = 2.0
              !ENDDO
           ELSE IF (maskFlag.EQ.4) THEN
              DO ki=0,1
               STREAMICE_umask(i+ki,j+kj,bi,bj) = 0.0
               STREAMICE_vmask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
              STREAMICE_vfacemask(i,j+kj,bi,bj) = 4.0
           ELSE IF (maskFlag.EQ.0) THEN
              DO ki=0,1
               STREAMICE_umask(i+ki,j+kj,bi,bj) = 0.0
               STREAMICE_vmask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
              STREAMICE_vfacemask(i+ki,j,bi,bj) = 0.0
           ELSE IF (maskFlag.EQ.1) THEN
              DO ki=0,1
               STREAMICE_vmask(i+ki,j+kj,bi,bj) = 0.0
              ENDDO
            ENDIF
           ENDDO

           IF (i .lt. sNx+OLx) THEN
            IF ((STREAMICE_hmask(i+1,j,bi,bj) .eq. 0.0) .OR.
     &          (STREAMICE_hmask(i+1,j,bi,bj) .eq. 2.0))  THEN
            !right boundary or adjacent to unfilled cell
             STREAMICE_ufacemask(i+1,j,bi,bj) = 2.0
            ENDIF
           ENDIF

           IF (i .gt. 1-OLx) THEN
            IF ((STREAMICE_hmask(i-1,j,bi,bj) .eq. 0.0) .OR.
     &          (STREAMICE_hmask(i-1,j,bi,bj) .eq. 2.0))  THEN
            !left boundary or adjacent to unfilled cell
             STREAMICE_ufacemask(i,j,bi,bj) = 2
            ENDIF
           ENDIF

           IF (j .lt. sNy+OLy) THEN
            IF ((STREAMICE_hmask(i,j+1,bi,bj) .eq. 0.0) .OR.
     &          (STREAMICE_hmask(i,j+1,bi,bj) .eq. 2.0))  THEN
            !top boundary or adjacent to unfilled cell
             STREAMICE_vfacemask(i,j+1,bi,bj) = 2
            ENDIF
           ENDIF

           IF (j .gt. 1-OLy) THEN
            IF ((STREAMICE_hmask(i,j-1,bi,bj) .eq. 0.0) .OR.
     &          (STREAMICE_hmask(i,j-1,bi,bj) .eq. 2.0))  THEN
            !bot boundary or adjacent to unfilled cell
             STREAMICE_vfacemask(i,j,bi,bj) = 2.0
            ENDIF
           ENDIF

          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

!$TAF STORE streamice_umask = comlev1, key=ikey_dynamics
!$TAF STORE streamice_vmask = comlev1, key=ikey_dynamics

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          IF(streamice_umask(i,j,bi,bj).eq.-1.0) THEN
           streamice_umask(i,j,bi,bj)=0.0
          ENDIF
          IF(streamice_vmask(i,j,bi,bj).eq.-1.0) THEN
           streamice_vmask(i,j,bi,bj)=0.0
          ENDIF
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RS( STREAMICE_ufacemask, myThid )
      _EXCH_XY_RS( STREAMICE_vfacemask, myThid )
      _EXCH_XY_RS( STREAMICE_umask, myThid )
      _EXCH_XY_RS( STREAMICE_vmask, myThid )

!       CALL WRITE_FULLARRAY_RL ("umask",STREAMICE_umask,
!      c   1,0,0,1,0,myThid)
!      CALL WRITE_FLD_XY_RL ("umask","",STREAMICE_umask,0,myThid)
!      CALL WRITE_FLD_XY_RL ("vmask","",STREAMICE_vmask,0,myThid)
!      CALL WRITE_FLD_XY_RL ("ufacemask","",STREAMICE_ufacemask,0,myThid)
!      CALL WRITE_FLD_XY_RL ("vfacemask","",STREAMICE_vfacemask,0,myThid)

#ifdef ALLOW_PETSC
      myThidCopy = myThid
      if (streamice_use_petsc) then
      call streamice_petsc_numerate (myThidCopy)
      endif
#endif

#endif
      RETURN
      END
