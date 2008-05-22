C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_dump.F,v 1.3 2008/05/22 12:21:19 mlosch Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_DUMP

C     !INTERFACE:
      SUBROUTINE MNC_DUMP( myThid )

C     !DESCRIPTION:
C     Create a short (usually incomplete) summary of the current state
C     of the MNC lookup tables.  This can be helpful for debugging MNC.
      
C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP
      CALL MNC_DUMP_ALL(5, 20, myThid)
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 1
C     !ROUTINE: MNC_DUMP_ALL

C     !INTERFACE:
      SUBROUTINE MNC_DUMP_ALL( n_id, n_items, myThid )

C     !DESCRIPTION:
C     Create a summary of a specified portion of the current state of
C     the MNC lookup tables.  This can be helpful for debugging MNC.

C     !USES:
#include "MNC_COMMON.h"

C     !INPUT PARAMETERS:
      integer n_id, n_items, myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j

      write(*,*) 'mnc_f_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_f_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_g_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_g_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_v_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_v_names(i)(1:40)
      ENDDO
      write(*,*) 'mnc_d_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_d_names(i)(1:40)
      ENDDO

      write(*,*) 'mnc_d_ids, mnc_d_size :'
      DO i = 1,n_id
        write(*,*) i,': ',mnc_d_ids(i), mnc_d_size(i)
      ENDDO

      write(*,*) 'mnc_f_info :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_f_info(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_fd_ind :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_fd_ind(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_fv_ids :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_fv_ids(i,j), j=1,n_items)
      ENDDO
      write(*,*) 'mnc_f_alld :'
      DO i = 1,n_id
        write(*,*) i,': ',(mnc_f_alld(i,j), j=1,n_items)
      ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
