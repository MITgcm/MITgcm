C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_init.F,v 1.15 2004/04/05 06:01:07 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: MNC_INIT

C     !INTERFACE:
      SUBROUTINE MNC_INIT( myThid )

C     !DESCRIPTION:
C     Initialize (zero) the look-up tables.  This routine should
C     \textbf{always} be run before any other MNC subroutines to ensure
C     that the lookup tables start in a well-defined state.
      
C     !USES:
#include "mnc_common.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j, g, v
      character blank*(MNC_MAX_CHAR)

C     Write blanks or zeros to all the internal names and ID tables
      DO i = 1,MNC_MAX_CHAR
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

C     Blank the CW tables
      DO g = 1,MNC_MAX_ID
        mnc_cw_gname(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_vname(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_vgind(g) = 0
        mnc_cw_ndim(g) = 0
        mnc_cw_fgnm(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_fgud(g) = 0
        DO i = 1,MNC_CW_MAX_I
          mnc_cw_dn(i,g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
          mnc_cw_vtnm(i,g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
          mnc_cw_vinm(i,g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
          mnc_cw_vdnm(i,g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
          mnc_cw_vtat(i,g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
          mnc_cw_dims(i,g) = 0
          mnc_cw_is(i,g) = 0
          mnc_cw_ie(i,g) = 0
          mnc_cw_viat(i,g) = 0
          mnc_cw_vdat(i,g) = 0.0D0
        ENDDO
        DO i = 1,3
          mnc_cw_vnat(i,g) = 0
        ENDDO
        DO i = 1,2
           mnc_cw_vbij(i,g) = 0
        ENDDO
      ENDDO

      RETURN
      END

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
#include "mnc_common.h"

C     !INPUT PARAMETERS:
      integer n_id, n_items, myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j

      write(*,*) 'mnc_f_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_f_names(i)
      ENDDO
      write(*,*) 'mnc_g_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_g_names(i)
      ENDDO
      write(*,*) 'mnc_v_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_v_names(i)
      ENDDO
      write(*,*) 'mnc_d_names :'
      DO i = 1,n_id
        write(*,'(i3,a,a40)') i,':', mnc_d_names(i)
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
