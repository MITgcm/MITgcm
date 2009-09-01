C $Header: /u/gcmpack/MITgcm/pkg/mdsio/mdsio_segxtorx_2d.F,v 1.1 2009/09/01 19:16:51 jmc Exp $
C $Name:  $

#include "MDSIO_OPTIONS.h"

C--  File mdsio_segxtorx_2d.F: Routines to pass segment to/from 2D section array
C--   Contents
C--   o MDS_SEG4toRL_2D
C--   o MDS_SEG4toRS_2D
C--   o MDS_SEG8toRL_2D
C--   o MDS_SEG8toRS_2D

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_SEG4toRL_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)

C IN:
C     sn,ol,nNz  integer :: size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,   integer :: indices to array "arr"
C     copyTo     logical :: flag to indicate tranfer direction.
C                           .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg        Real*4  :: 1-D vector of length sn
C OUT:
C     arr         _RL    :: model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      INTEGER sn,ol,nNz,bi,bj,k
      LOGICAL copyTo
      Real*4 seg(sn)
      _RL arr(1-ol:sn+ol,nNz,nSx,nSy)

C Local
      INTEGER ii
C     ------------------------------------------------------------------
      IF (copyTo) THEN
       DO ii=1,sn
        arr(ii,k,bi,bj)=seg(ii)
       ENDDO
      ELSE
       DO ii=1,sn
        seg(ii)=arr(ii,k,bi,bj)
       ENDDO
      ENDIF
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_SEG4toRS_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)

C IN:
C     sn,ol,nNz  integer :: size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,   integer :: indices to array "arr"
C     copyTo     logical :: flag to indicate tranfer direction.
C                           .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg        Real*4  :: 1-D vector of length sn
C OUT:
C     arr         _RS    :: model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      INTEGER sn,ol,nNz,bi,bj,k
      LOGICAL copyTo
      Real*4 seg(sn)
      _RS arr(1-ol:sn+ol,nNz,nSx,nSy)

C Local
      INTEGER ii
C     ------------------------------------------------------------------
      IF (copyTo) THEN
       DO ii=1,sn
        arr(ii,k,bi,bj)=seg(ii)
       ENDDO
      ELSE
       DO ii=1,sn
        seg(ii)=arr(ii,k,bi,bj)
       ENDDO
      ENDIF
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_SEG8toRL_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)

C IN:
C     sn,ol,nNz  integer :: size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,   integer :: indices to array "arr"
C     copyTo     logical :: flag to indicate tranfer direction.
C                           .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg        Real*8  :: 1-D vector of length sn
C OUT:
C     arr         _RL    :: model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      INTEGER sn,ol,nNz,bi,bj,k
      LOGICAL copyTo
      Real*8 seg(sn)
      _RL arr(1-ol:sn+ol,nNz,nSx,nSy)

C Local
      INTEGER ii
C     ------------------------------------------------------------------
      IF (copyTo) THEN
       DO ii=1,sn
        arr(ii,k,bi,bj)=seg(ii)
       ENDDO
      ELSE
       DO ii=1,sn
        seg(ii)=arr(ii,k,bi,bj)
       ENDDO
      ENDIF
C     ------------------------------------------------------------------
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MDS_SEG8toRS_2D(sn,ol,nNz,bi,bj,k,copyTo,seg,arr)

C IN:
C     sn,ol,nNz  integer :: size of 'arr'. sn,ol can be sNx,oLx OR sNy,oLy
C     k,bi,bj,   integer :: indices to array "arr"
C     copyTo     logical :: flag to indicate tranfer direction.
C                           .TRUE.: seg -> arr, .FALSE.: arr -> seg
C     seg        Real*8  :: 1-D vector of length sn
C OUT:
C     arr         _RS    :: model vertical slice (array)
C
C Created: 06/03/00 spk@ocean.mit.edu

      IMPLICIT NONE
C Global variables / common blocks
#include "SIZE.h"

C Arguments
      INTEGER sn,ol,nNz,bi,bj,k
      LOGICAL copyTo
      Real*8 seg(sn)
      _RS arr(1-ol:sn+ol,nNz,nSx,nSy)

C Local
      INTEGER ii
C     ------------------------------------------------------------------
      IF (copyTo) THEN
       DO ii=1,sn
        arr(ii,k,bi,bj)=seg(ii)
       ENDDO
      ELSE
       DO ii=1,sn
        seg(ii)=arr(ii,k,bi,bj)
       ENDDO
      ENDIF
C     ------------------------------------------------------------------
      RETURN
      END
