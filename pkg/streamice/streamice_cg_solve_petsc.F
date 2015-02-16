C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_cg_solve_petsc.F,v 1.3 2015/02/16 16:46:44 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_CG_SOLVE_PETSC( 
     U                               cg_Uin,     ! x-velocities
     U                               cg_Vin,     ! y-velocities
     I                               cg_Bu,      ! force in x dir
     I                               cg_Bv,      ! force in y dir
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     I                               A_uv,       ! section of matrix that multiplies v and projects on u
     I                               A_vu,       ! section of matrix that multiplies u and projects on v
     I                               A_vv,       ! section of matrix that multiplies v and projects on v
     I                               tolerance, 
     O                               iters,
     I                               maxIter,
     I                               myThid )
C     /============================================================\
C     | SUBROUTINE                                                 |   
C     | o                                                          |
C     |============================================================|
C     |                                                            |
C     \============================================================/
      IMPLICIT NONE

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"



#ifdef ALLOW_PETSC
#include "finclude/petsc.h"
! UNCOMMENT IF V3.0
!#include "finclude/petscvec.h"
!#include "finclude/petscmat.h"
!#include "finclude/petscksp.h"
!#include "finclude/petscpc.h"
#endif
C     === Global variables ===

      
C     !INPUT/OUTPUT ARGUMENTS
C     cg_Uin, cg_Vin - input and output velocities
C     cg_Bu, cg_Bv - driving stress
      INTEGER myThid
      INTEGER iters, maxiter
      _RL tolerance
      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL 
     & A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_uv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1),
     & A_vv (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)

C     LOCAL VARIABLES
      INTEGER i, j, bi, bj, cg_halo, conv_flag
      INTEGER iter, is, js, ie, je, colx, coly, k
      _RL dot_p1, dot_p2, alpha_k, beta_k, resid, resid_0
      _RL dot_p1_tile (nSx,nSy)
      _RL dot_p2_tile (nSx,nSy)
      CHARACTER*(MAX_LEN_MBUF) msgBuf


#ifdef ALLOW_PETSC
      INTEGER indices(2*(snx*nsx*sny*nsy))
      INTEGER n_dofs_cum_sum (0:nPx*nPy-1), idx(1)
      _RL rhs_values(2*(snx*nsx*sny*nsy))
      _RL solution_values(2*(snx*nsx*sny*nsy))
!      _RL mat_values (2*Nx*Ny,2*(snx*nsx*sny*nsy))
      _RL mat_values (18,1), mat_val_return(1)
      INTEGER indices_col(18)
      INTEGER local_dofs, global_dofs, dof_index, dof_index_col
      INTEGER local_offset
      Mat matrix
      KSP ksp
      PC  pc
      Vec rhs
      Vec solution
      PetscErrorCode ierr
#ifdef ALLOW_USE_MPI
      integer mpiRC, mpiMyWid
#endif 
#endif


#ifdef ALLOW_STREAMICE




#ifdef ALLOW_PETSC

#ifdef ALLOW_USE_MPI


      CALL MPI_COMM_RANK( MPI_COMM_WORLD, mpiMyWId, mpiRC )
      local_dofs = n_dofs_process (mpiMyWid)
      global_dofs = 0
      
      n_dofs_cum_sum(0) = 0
      DO i=0,nPx*nPy-1
       global_dofs = global_dofs + n_dofs_process (i)
       if (i.ge.1) THEN
         n_dofs_cum_sum(i) = n_dofs_cum_sum(i-1)+
     &                     n_dofs_process(i-1)
       endif
      ENDDO
      local_offset = n_dofs_cum_sum(mpimywid)

#else

      local_dofs = n_dofs_process (0)
      global_dofs = local_dofs
      local_offset = 0

#endif 

!      call petscInitialize(PETSC_NULL_CHARACTER,ierr)

!----------------------

      CALL TIMER_START ('STREAMICE_PETSC_SETUP',myThid)

      call VecCreate(PETSC_COMM_WORLD, rhs, ierr)
      call VecSetSizes(rhs, local_dofs, global_dofs, ierr)
      call VecSetType(rhs, VECMPI, ierr)

      call VecCreate(PETSC_COMM_WORLD, solution, ierr)
      call VecSetSizes(solution, local_dofs, global_dofs, ierr) 
      call VecSetType(solution, VECMPI, ierr)

      do i=1,local_dofs
        indices(i) = i-1 + local_offset
      end do
      do i=1,2*nSx*nSy*sNx*sNy
        rhs_values (i) = 0. _d 0
        solution_values (i) = 0. _d 0
      enddo

! gather rhs and initial guess values to populate petsc vectors

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          dof_index = INT(streamice_petsc_dofs_u(i,j,bi,bj))
     &                - local_offset

          if (dof_index.ge.0) THEN
           
           rhs_values(dof_index+1) = cg_Bu(i,j,bi,bj)
           solution_values(dof_index+1) = cg_Uin(i,j,bi,bj)

          endif

!---------------

          dof_index = INT(streamice_petsc_dofs_v(i,j,bi,bj))
     &                - local_offset

          if (dof_index.ge.0) THEN

           rhs_values(dof_index+1) = cg_Bv(i,j,bi,bj)
           solution_values(dof_index+1) = cg_Vin(i,j,bi,bj)

          endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO


      call VecSetValues(rhs, local_dofs, indices, rhs_values,
     &                  INSERT_VALUES, ierr)
      call VecAssemblyBegin(rhs, ierr)
      call VecAssemblyEnd(rhs, ierr)


      call VecSetValues(solution, local_dofs, indices,
     &                  solution_values, INSERT_VALUES, ierr)
      call VecAssemblyBegin(solution, ierr)
      call VecAssemblyEnd(solution, ierr)

!     IF USING v3.0 THEN
!     call MatCreateMPIAIJ (PETSC_COMM_WORLD,
      call MatCreateAIJ (PETSC_COMM_WORLD, 
     &                      local_dofs, local_dofs,
     &                      global_dofs, global_dofs, 
     &                      18, PETSC_NULL_INTEGER, 
     &                      18, PETSC_NULL_INTEGER,
     &                      matrix, ierr)


! populate petsc matrix

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          dof_index = INT(streamice_petsc_dofs_u(i,j,bi,bj))
!     &                - local_offset

          IF (dof_index .ge. 0) THEN

           DO k=1,18
            indices_col(k) = 0
            mat_values(k,1) = 0. _d 0
           ENDDO
           k=0

           DO coly=-1,1
            DO colx=-1,1

             dof_index_col = streamice_petsc_dofs_u(i+colx,j+coly,bi,bj)

             if (dof_index_col.ge.0) THEN
!               pscal = A_uu(i,j,bi,bj,colx,coly) 
!               CALL MatSetValue (matrix,dof_index, dof_index_col, 
!     &              pscal,INSERT_VALUES,ierr)
                k=k+1
                mat_values (k,1) = A_uu(i,j,bi,bj,colx,coly)
                indices_col (k) = dof_index_col
             endif
              
             dof_index_col = streamice_petsc_dofs_v(i+colx,j+coly,bi,bj)

             if (dof_index_col.ge.0) THEN
!               CALL MatSetValue (matrix,dof_index, dof_index_col,
!     &              A_uv(i,j,bi,bj,colx,coly),INSERT_VALUES,ierr)
                k=k+1
                mat_values (k,1) = A_uv(i,j,bi,bj,colx,coly)
                indices_col (k) = dof_index_col
             endif
            
            ENDDO
           ENDDO

           call matSetValues (matrix, 1, dof_index, k, indices_col,
     &                        mat_values,INSERT_VALUES,ierr)


          ENDIF

! ----------------------------------------------

          dof_index = INT(streamice_petsc_dofs_v(i,j,bi,bj))
!     &                - local_offset

          IF (dof_index .ge. 0) THEN

           DO k=1,18
            indices_col(k) = 0
            mat_values(k,1) = 0. _d 0
           ENDDO
           k=0

           DO coly=-1,1
            DO colx=-1,1

             dof_index_col = streamice_petsc_dofs_u(i+colx,j+coly,bi,bj)

             if (dof_index_col.ge.0) THEN
!               CALL MatSetValue (matrix,dof_index, dof_index_col,
!     &              A_vu(i,j,bi,bj,colx,coly),INSERT_VALUES,ierr)
                k=k+1
                mat_values (k,1) = A_vu(i,j,bi,bj,colx,coly)
                indices_col (k) = dof_index_col
             endif

             dof_index_col = streamice_petsc_dofs_v(i+colx,j+coly,bi,bj)

             if (dof_index_col.ge.0) THEN
!               CALL MatSetValue (matrix,dof_index, dof_index_col,
!     &              A_vv(i,j,bi,bj,colx,coly),INSERT_VALUES,ierr)
                k=k+1
                mat_values (k,1) = A_vv(i,j,bi,bj,colx,coly)
                indices_col (k) = dof_index_col
             endif

            ENDDO
           ENDDO

           call matSetValues (matrix, 1, dof_index, k, indices_col,
     &                        mat_values,INSERT_VALUES,ierr)
          ENDIF

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      call MatAssemblyBegin(matrix,MAT_FINAL_ASSEMBLY,ierr)
      call MatAssemblyEnd(matrix,MAT_FINAL_ASSEMBLY,ierr)


      call KSPCreate(PETSC_COMM_WORLD, ksp, ierr)
      call KSPSetOperators(ksp, matrix, matrix, 
     &                     DIFFERENT_NONZERO_PATTERN, ierr)

      SELECT CASE (PETSC_SOLVER_TYPE)
       CASE ('CG')
       PRINT *, "PETSC SOLVER: SELECTED CG"
       call KSPSetType(ksp, KSPCG, ierr)
       CASE ('GMRES')
       PRINT *, "PETSC SOLVER: SELECTED GMRES"
       call KSPSetType(ksp, KSPGMRES, ierr)
       CASE ('BICG')
       PRINT *, "PETSC SOLVER: SELECTED BICG"
       call KSPSetType(ksp, KSPBICG, ierr)
       CASE DEFAULT
       PRINT *, "PETSC SOLVER: SELECTED DEFAULT"
       call KSPSetType(ksp, KSPCG, ierr)
      END SELECT

      call KSPGetPC(ksp, pc, ierr)
      call KSPSetTolerances(ksp,tolerance,
     &     PETSC_DEFAULT_DOUBLE_PRECISION,     
     &     PETSC_DEFAULT_DOUBLE_PRECISION,
     &     maxiter,ierr)

      SELECT CASE (PETSC_PRECOND_TYPE)
       CASE ('BLOCKJACOBI')
       PRINT *, "PETSC PRECOND: SELECTED BJACOBI"
       call PCSetType(pc, PCBJACOBI, ierr)
       CASE ('JACOBI')
       PRINT *, "PETSC PRECOND: SELECTED JACOBI"
       call PCSetType(pc, PCJACOBI, ierr)
       CASE ('ILU')
       PRINT *, "PETSC PRECOND: SELECTED ILU"
       call PCSetType(pc, PCILU, ierr)

       CASE ('GAMG')
       PRINT *, "PETSC PRECOND: SELECTED GAMG"
       call PCSetType(pc, PCGAMG, ierr)
       call PCGAMGSetCoarseEqLim(pc,10000,ierr)
       call PCGAMGSetSymGraph(pc, PETSC_TRUE,ierr)
       call PCGAMGSetNSmooths(pc, 0,ierr)
       call PCGAMGSetThreshold(pc, .001,ierr)
!       call PCGAMGSetReuseProl(pc,PETSC_FALSE,ierr)

       CASE DEFAULT
       PRINT *, "PETSC PRECOND: SELECTED DEFAULT"
       call PCSetType(pc, PCBJACOBI, ierr)
      END SELECT


      CALL TIMER_STOP ('STREAMICE_PETSC_SETUP',myThid)

      CALL TIMER_START ('STREAMICE_PETSC_SOLVE',myThid)

      call KSPSolve(ksp, rhs, solution, ierr)

      CALL TIMER_STOP ('STREAMICE_PETSC_SOLVE',myThid)

      call KSPGetIterationNumber(ksp,iters,ierr)

      call VecGetValues(solution,local_dofs,indices,
     &      solution_values,ierr)

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx

          dof_index = INT(streamice_petsc_dofs_u(i,j,bi,bj))
     &                - local_offset
          if (dof_index.ge.0) THEN
           cg_Uin(i,j,bi,bj) = solution_values(dof_index+1)
          endif
          
          dof_index = INT(streamice_petsc_dofs_v(i,j,bi,bj))
     &                - local_offset
          if (dof_index.ge.0) THEN
           cg_Vin(i,j,bi,bj) = solution_values(dof_index+1)
          endif

         ENDDO
        ENDDO
       ENDDO
      ENDDO

      call KSPDestroy (ksp, ierr)
      call VecDestroy (rhs, ierr)
      call VecDestroy (solution, ierr)
      call MatDestroy (matrix, ierr)
         

!      call PetscFinalize(ierr)


#endif



#endif
      RETURN
      END
