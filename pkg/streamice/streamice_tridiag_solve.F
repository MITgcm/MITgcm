#include "STREAMICE_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CBOP

      SUBROUTINE STREAMICE_TRIDIAG_SOLVE( 
     U                               cg_Uin,     ! x-velocities
     U                               cg_Vin,     ! x-velocities
     U                               cg_Bu,      ! force in x dir
     I                               A_uu,       ! section of matrix that multiplies u and projects on u
     I                               umask,
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

      _RL cg_Uin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Vin (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL cg_Bu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL A_uu (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy,-1:1,-1:1)
      _RS umask (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      INTEGER myThid

      INTEGER iMin,iMax,i,j,k
      _RL aMat(1:Nx)
      _RL bMat(1:Nx)
      _RL cMat(1:Nx)
      _RL yMat(1:Nx)
      _RL bet(1:Nx)
      _RL tmpvar
      INTEGER errCode
 

!      CALL WRITE_FLD_XY_RL ("taud_tri","",cg_Bu,0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_m1m1","",A_uu(:,:,:,:,-1,-1),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_m1_0","",A_uu(:,:,:,:,-1,0),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_m1p1","",A_uu(:,:,:,:,-1,1),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_0_m1","",A_uu(:,:,:,:,0,-1),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_0_0","",A_uu(:,:,:,:,0,0),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_0_p1","",A_uu(:,:,:,:,0,1),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_p1m1","",A_uu(:,:,:,:,1,-1),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_p1_0","",A_uu(:,:,:,:,1,0),0,mythid)
!      CALL WRITE_FLD_XY_RL ("A_p1p1","",A_uu(:,:,:,:,1,1),0,mythid)


      
      IF (nPx.gt.1 .or. nSx.gt.1) THEN
       STOP 'must be serial for tridiag solve'
      ENDIF

      errCode = 0

      imax = 0
      iMin = 2
      do i=imin,Nx
       if (umask(i,1,1,1).eq.1.0) THEN

        aMat(i)=0.0
        bmat(i)=0.0
        cmat(i)=0.0
        ymat(i)=0.0
        do j=-1,1
        do k=1,3
        aMat(i) = amat(i)+A_uu(i,k,1,1,-1,j)
        bMat(i) = bmat(i)+A_uu(i,k,1,1,0,j)
        cMat(i) = cmat(i)+A_uu(i,k,1,1,1,j)
        enddo
        yMat(i) = ymat(i)+cg_Bu(i,j+2,1,1)
        enddo
       else
        iMax = i-1
        exit
       endif
      enddo
      
      IF(imax.eq.0) THEN
       imax=Nx
      ENDIF
 

      IF ( bMat(imin).NE.0. _d 0 ) THEN 
       bet(imin) = 1. _d 0 / bMat(imin)
      ELSE
       bet(imin) = 0. _d 0
       errCode = 1
      ENDIF
       
      DO i=imin+1,imax
       tmpvar = bmat(i) - amat(i)*cmat(i-1)*bet(i-1)
       IF ( tmpvar .NE. 0. _d 0 ) THEN
        bet(i) = 1. _d 0 / tmpvar
       ELSE
        bet(i) = 0. _d 0
        errCode = 1
       ENDIF
      ENDDO


      ymat(imin) = ymat(imin)*bet(imin)

      DO i=imin+1,imax
       ymat(i) = ( ymat(i) 
     &            - amat(i)*ymat(i-1)
     &            )*bet(i) 
      ENDDO


      DO i=imax-1,imin,-1
          ymat(i) = ymat(i)
     &     - cmat(i)*bet(i)*ymat(i+1)
      ENDDO

      DO j=1,sNy
       DO i=imin,imax
        cg_Uin (i,j,1,1) = ymat(i)
       ENDDO
      ENDDO

      DO j=1,sNy
       DO i=1,sNx
        cg_Vin (i,j,1,1) = 0. _d 0
       ENDDO
      ENDDO

      print *, "ERRORCODE", errcode

      RETURN
      END
