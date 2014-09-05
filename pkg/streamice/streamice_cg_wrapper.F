C $Header: /u/gcmpack/MITgcm/pkg/streamice/streamice_cg_wrapper.F,v 1.4 2014/09/05 14:25:11 dgoldberg Exp $
C $Name:  $

#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP
      SUBROUTINE STREAMICE_CG_WRAPPER( 
     U                               cg_Uin, 
     U                               cg_Vin, 
     I                               cg_tauU,
     I                               cg_tauV,
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

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "STREAMICE.h"
#include "STREAMICE_CG.h"

      
C     !INPUT/OUTPUT ARGUMENTS
C     cg_Uin, cg_Vin - input and output velocities
C     cg_Bu, cg_Bv - driving stress

      INTEGER myThid
      INTEGER iters
      INTEGER maxIter
      _RL tolerance

      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_tauU (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_tauV (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#ifdef ALLOW_STREAMICE

      _RL dot_p1, dot_p2
      _RL dot_p1_tile (nSx,nSy)
      _RL dot_p2_tile (nSx,nSy)
      INTEGER i, j, bi, bj



      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1,sNy
         DO i=1,sNx
          
          DIAGu_SI (i,j,bi,bj) = 0. _d 0
          DIAGv_SI (i,j,bi,bj) = 0. _d 0
          ubd_SI (i,j,bi,bj) = 0. _d 0
          vbd_SI (i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO
       ENDDO
      ENDDO

C     DIRICHLET BOUNDARY VALUES ADDED TO RHS

      CALL STREAMICE_CG_BOUND_VALS( myThid,     
     O    ubd_SI, 
     O    vbd_SI)

      _EXCH_XY_RL(ubd_SI, myThid)
      _EXCH_XY_RL(vbd_SI, myThid)

!      CALL WRITE_FLD_XY_RL ( "ubd_SI", "",
!     &   ubd_SI, 0, myThid )

!      CALL WRITE_FLD_XY_RL ( "vbd_SI", "",
!     &   STREAMICE_vmask, 0, myThid )

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          RHSu_SI (i,j,bi,bj) = cg_tauU (i,j,bi,bj) 
     &     - ubd_SI(i,j,bi,bj)
          RHSv_SI (i,j,bi,bj) = cg_tauV (i,j,bi,bj) 
     &     - vbd_SI(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL( RHSu_SI, myThid )
      _EXCH_XY_RL( RHSv_SI, myThid )

C     GET DIAGONAL OF MATRIX      

      CALL STREAMICE_CG_ADIAG( myThid, 
     O    DIAGu_SI, 
     O    DIAGv_SI)

      _EXCH_XY_RL( DIAGu_SI, myThid )
      _EXCH_XY_RL( DIAGv_SI, myThid )

C     ccccc

      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLy,sNx+OLy
          IF (STREAMICE_umask(i,j,bi,bj).ne.1.0)
     &     cg_Uin(i,j,bi,bj)=0.0
          IF (STREAMICE_vmask(i,j,bi,bj).ne.1.0)
     &     cg_Vin(i,j,bi,bj)=0.0

!           print *, "rhs", i,j,RHSu_SI(i,j,bi,bj)

         ENDDO
        ENDDO
       ENDDO
      ENDDO


#ifdef STREAMICE_CONSTRUCT_MATRIX

      
      CALL STREAMICE_CG_MAKE_A(myThid)

!      call write_fld_xy_rl ("streamicb_cg_A1_m1_m1","",
!     & streamice_cg_A1(:,:,1,1,-1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_m1_0","",
c     & streamice_cg_A1(:,:,1,1,-1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_m1_p1","",
c     & streamice_cg_A1(:,:,1,1,-1,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_0_m1","",
c     & streamice_cg_A1(:,:,1,1,0,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_0_0","",
c     & streamice_cg_A1(:,:,1,1,0,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_0_p1","",
c     & streamice_cg_A1(:,:,1,1,0,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_p1_m1","",
c     & streamice_cg_A1(:,:,1,1,1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_p1_0","",
c     & streamice_cg_A1(:,:,1,1,1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A1_p1_p1","",
c     & streamice_cg_A1(:,:,1,1,1,1),0,myThid)
c
c      call write_fld_xy_rl ("streamicb_cg_A2_m1_m1","",
c     & streamice_cg_A2(:,:,1,1,-1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_m1_0","",
c     & streamice_cg_A2(:,:,1,1,-1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_m1_p1","",
c     & streamice_cg_A2(:,:,1,1,-1,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_0_m1","",
c     & streamice_cg_A2(:,:,1,1,0,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_0_0","",
c     & streamice_cg_A2(:,:,1,1,0,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_0_p1","",
c     & streamice_cg_A2(:,:,1,1,0,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_p1_m1","",
c     & streamice_cg_A2(:,:,1,1,1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_p1_0","",
c     & streamice_cg_A2(:,:,1,1,1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A2_p1_p1","",
c     & streamice_cg_A2(:,:,1,1,1,1),0,myThid)
c
c      call write_fld_xy_rl ("streamicb_cg_A3_m1_m1","",
c     & streamice_cg_A3(:,:,1,1,-1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_m1_0","",
c     & streamice_cg_A3(:,:,1,1,-1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_m1_p1","",
c     & streamice_cg_A3(:,:,1,1,-1,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_0_m1","",
c     & streamice_cg_A3(:,:,1,1,0,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_0_0","",
c     & streamice_cg_A3(:,:,1,1,0,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_0_p1","",
c     & streamice_cg_A3(:,:,1,1,0,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_p1_m1","",
c     & streamice_cg_A3(:,:,1,1,1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_p1_0","",
c     & streamice_cg_A3(:,:,1,1,1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A3_p1_p1","",
c     & streamice_cg_A3(:,:,1,1,1,1),0,myThid)
c
c      call write_fld_xy_rl ("streamicb_cg_A4_m1_m1","",
c     & streamice_cg_A4(:,:,1,1,-1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_m1_0","",
c     & streamice_cg_A4(:,:,1,1,-1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_m1_p1","",
c     & streamice_cg_A4(:,:,1,1,-1,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_0_m1","",
c     & streamice_cg_A4(:,:,1,1,0,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_0_0","",
c     & streamice_cg_A4(:,:,1,1,0,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_0_p1","",
c     & streamice_cg_A4(:,:,1,1,0,1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_p1_m1","",
c     & streamice_cg_A4(:,:,1,1,1,-1),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_p1_0","",
c     & streamice_cg_A4(:,:,1,1,1,0),0,myThid)
c      call write_fld_xy_rl ("streamicb_cg_A4_p1_p1","",
c     & streamice_cg_A4(:,:,1,1,1,1),0,myThid)
c

!       print *, "MATRIX 1"
!       do i=1,sNx
!        print *, i, 
!      & streamice_cg_A1(i,1,1,1,-1,0),
!      & streamice_cg_A1(i,1,1,1,0,0),
!      & streamice_cg_A1(i,1,1,1,1,0),
!      & streamice_cg_A1(i,2,1,1,-1,0),
!      & streamice_cg_A1(i,2,1,1,0,0),
!      & streamice_cg_A1(i,2,1,1,1,0),
!      & streamice_cg_A1(i,3,1,1,-1,0),
!      & streamice_cg_A1(i,3,1,1,0,0),
!      & streamice_cg_A1(i,3,1,1,1,0),
!      & visc_streamice(i,1,1,1),visc_streamice(i,2,1,1),
!      & visc_streamice(i,3,1,1)  
!       enddo

      CALL STREAMICE_CG_SOLVE( 
     &  cg_Uin,
     &  cg_Vin,
     &  RHSu_SI,
     &  RHSv_SI,
     &  streamice_cg_A1,
     &  streamice_cg_A2,
     &  streamice_cg_A3,
     &  streamice_cg_A4, 
     &  tolerance, 
     &  iters,
     &  maxIter,
     &  myThid )

      _EXCH_XY_RL( RHSu_SI, myThid )
      _EXCH_XY_RL( RHSv_SI, myThid )

!       DO bj = myByLo(myThid), myByHi(myThid)
!        DO bi = myBxLo(myThid), myBxHi(myThid)
!         DO j=1-OLy,sNy+OLy
!          DO i=1-OLx,sNx+OLx
!           cg_Uin(i,j,bi,bj) = cg_Uin(i,j,bi,bj) + 
!      &     0.0 * cg_Uin(i,j,bi,bj)**2
!           cg_Vin(i,j,bi,bj) = cg_Vin(i,j,bi,bj) + 
!      &     0.0 * cg_Vin(i,j,bi,bj)**2
!          ENDDO
!         ENDDO
!        ENDDO
!       ENDDO

      

#else

      CALL STREAMICE_CG_SOLVE_MATFREE( 
     &  cg_Uin,
     &  cg_Vin,
     &  RHSu_SI,
     &  RHSv_SI,
     &  tolerance, 
     &  iters,
     &  myThid )

#endif


C    ACTUAL CG CALL




      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO j=1-OLy,sNy+OLy
         DO i=1-OLy,sNx+OLy
          IF (STREAMICE_umask(i,j,bi,bj).eq.3.0)
     &     cg_Uin(i,j,bi,bj)=u_bdry_values_SI(i,j,bi,bj)
          IF (STREAMICE_vmask(i,j,bi,bj).eq.3.0)
     &     cg_Vin(i,j,bi,bj)=v_bdry_values_SI(i,j,bi,bj)
          
!           print *, "rhs", i,j,RHSu_SI(i,j,bi,bj)
          
         ENDDO
        ENDDO
       ENDDO
      ENDDO       

      _EXCH_XY_RL( cg_Uin, myThid )
      _EXCH_XY_RL( cg_Vin, myThid )  


#endif
      RETURN
      END      

