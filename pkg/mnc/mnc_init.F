C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_init.F,v 1.5 2004/01/25 00:22:57 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_INIT()

#include "mnc_common.h"

C     Arguments

C     Local Variables
      integer i,j
      character blank*(MNC_MAX_CHAR)

C     Write blanks or zeros to all the name and ID tables
      DO i = 1,mnc_max_char
        blank(i:i) = ' ';
      ENDDO
      mnc_blank_name(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
      DO i = 1,MNC_MAX_ID
        mnc_d_size(i) = 0
        mnc_d_ids(i)  = 0
        mnc_f_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_g_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_v_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_d_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        DO j = 1,MNC_MAX_INFO
          mnc_f_info(i,j) = 0
          mnc_fv_ids(i,j) = 0
          mnc_fd_ind(i,j) = 0
          mnc_f_alld(i,j) = 0
        ENDDO
      ENDDO

      RETURN
      END

C==================================================================

      SUBROUTINE MNC_DUMP()

#include "mnc_common.h"

C     Arguments

C     Local Variables
      integer i,j, n_id

C     n_id = mnc_max_id
      n_id = 5

      write(*,*) 'mnc_f_names :'
      DO i = 1,n_id
        write(*,'(a,a40)') '  :', mnc_f_names(i)
      ENDDO
      write(*,*) 'mnc_g_names :'
      DO i = 1,n_id
        write(*,'(a,a40)') '  :', mnc_g_names(i)
      ENDDO
      write(*,*) 'mnc_v_names :'
      DO i = 1,n_id
        write(*,'(a,a40)') '  :', mnc_v_names(i)
      ENDDO
      write(*,*) 'mnc_d_names :'
      DO i = 1,n_id
        write(*,'(a,a40)') '  :', mnc_d_names(i)
      ENDDO

      write(*,*) 'mnc_d_ids, mnc_d_size :'
      DO i = 1,n_id
        write(*,*) mnc_d_ids(i), mnc_d_size(i)
      ENDDO

      write(*,*) 'mnc_f_info :'
      DO i = 1,n_id
        write(*,*) (mnc_f_info(i,j), j=1,30)
      ENDDO
      write(*,*) 'mnc_fd_ind :'
      DO i = 1,n_id
        write(*,*) (mnc_fd_ind(i,j), j=1,30)
      ENDDO
      write(*,*) 'mnc_fv_ids :'
      DO i = 1,n_id
        write(*,*) (mnc_fv_ids(i,j), j=1,30)
      ENDDO
      write(*,*) 'mnc_f_alld :'
      DO i = 1,n_id
        write(*,*) (mnc_f_alld(i,j), j=1,30)
      ENDDO

      RETURN
      END

