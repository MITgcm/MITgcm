C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_init.F,v 1.26 2011/05/23 01:08:22 jmc Exp $
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
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "MNC_COMMON.h"
#include "MNC_PARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      integer i,j, g
      character blank*(MNC_MAX_CHAR)
      character bpath*(MNC_MAX_PATH)

C     Write blanks or zeros to all the internal names and ID tables
      DO i = 1,MNC_MAX_CHAR
        blank(i:i) = ' '
      ENDDO
      DO i = 1,MNC_MAX_PATH
        bpath(i:i) = ' '
      ENDDO

      mnc_blank_name(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
      DO i = 1,MNC_MAX_FID
        mnc_f_names(i)(1:MNC_MAX_PATH) = bpath(1:MNC_MAX_PATH)
        DO j = 1,MNC_MAX_INFO
          mnc_f_info(i,j) = 0
          mnc_fv_ids(i,j) = 0
          mnc_fd_ind(i,j) = 0
          mnc_f_alld(i,j) = 0
        ENDDO
      ENDDO
      DO i = 1,MNC_MAX_ID
        mnc_d_size(i) = 0
        mnc_d_ids(i)  = 0
        mnc_g_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_v_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_d_names(i)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
      ENDDO

C     Blank the CW tables
      DO g = 1,MNC_MAX_ID
        mnc_cw_gname(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_vname(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_vgind(g) = 0
        mnc_cw_vfmv(g) = 0
        mnc_cw_ndim(g) = 0
        mnc_cw_fgnm(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
        mnc_cw_fgud(g) = 0
        mnc_cw_fgis(g) = 0
        mnc_cw_fgci(g) = 1
C       mnc_cw_cvnm(g)(1:MNC_MAX_CHAR) = blank(1:MNC_MAX_CHAR)
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
           mnc_cw_vmvi(i,g) = 0
           mnc_cw_vmvr(i,g) = 0.0
           mnc_cw_vmvd(i,g) = 0.0D0
C          mnc_cw_cvse(i,g) = 0
        ENDDO
      ENDDO

      DO i = 1,MNC_MAX_INFO
        mnc_cw_cit(1,i) = 0
        mnc_cw_cit(2,i) = 0
        mnc_cw_cit(3,i) = -1
      ENDDO

      DO i = 1,2
       mnc_def_imv(i) = UNSET_I
       mnc_def_rmv(i) = UNSET_FLOAT4
       mnc_def_dmv(i) = UNSET_FLOAT8
      ENDDO

C     The default for all file types is to add the iter to the name and
C     grow them in "lock step" together
      mnc_cw_cit(1,1) = 1
      mnc_cw_cit(2,1) = nIter0

C     Here, we do not add the iter to the file name
      mnc_cw_cit(1,2) = -1
      mnc_cw_cit(2,2) = -1

C     For checkpoint files, we want to use the current iter but we do
C     not (by default, anyway) want to update the current iter for
C     everything else
      mnc_cw_cit(1,3) = 3
      mnc_cw_cit(2,3) = nIter0

C     DO i = 1,MNC_CW_CVDAT
C     mnc_cw_cvdt(i) = 0.0D0
C     ENDDO

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
