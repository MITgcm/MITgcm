C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_init.F,v 1.2 2004/01/06 23:19:27 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C==================================================================

      SUBROUTINE MNC_INIT()

#include "mnc_common.h"

C     Arguments

C     Local Variables
      integer i
      character blank*(MNC_MAX_CHAR)

C     Write blanks or zeros to all the name and ID tables
      DO i = 1,mnc_max_char
        blank(i:i) = ' ';
      ENDDO
      mnc_blank_name(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
      DO i = 1,mnc_max_id
        mnc_f_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_g_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_v_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_d_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_f_info(i,1) = 0
        mnc_f_info(i,2) = 0
        mnc_f_info(i,3) = 0
        mnc_fv_ids(i,1) = 0
      ENDDO

      RETURN
      END

