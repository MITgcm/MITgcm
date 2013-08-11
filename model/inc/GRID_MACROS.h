C $Header: /u/gcmpack/MITgcm/model/inc/GRID_MACROS.h,v 1.13 2013/08/11 14:27:37 jmc Exp $
C $Name:  $
C
CBOP
C    !ROUTINE: GRID_MACROS.h
C    !INTERFACE:
C    include GRID_MACROS.h
C    !DESCRIPTION: \bv
C     *==========================================================*
C     | GRID_MACROS.h
C     *==========================================================*
C     | These macros are used to substitute definitions for
C     | GRID.h variables for particular configurations.
C     | In setting these variables the following convention
C     | applies.
C     | undef  phi_CONST   - Indicates the variable phi is fixed
C     |                      in X, Y and Z.
C     | undef  phi_FX      - Indicates the variable phi only
C     |                      varies in X (i.e.not in X or Z).
C     | undef  phi_FY      - Indicates the variable phi only
C     |                      varies in Y (i.e.not in X or Z).
C     | undef  phi_FXY     - Indicates the variable phi only
C     |                      varies in X and Y ( i.e. not Z).
C     *==========================================================*
C     \ev
CEOP

#undef    DXC_CONST
#undef    DXC_FX
#undef    DXC_FY
#include "DXC_MACROS.h"

#undef    DXF_CONST
#undef    DXF_FX
#undef    DXF_FY
#include "DXF_MACROS.h"

#undef    DXG_CONST
#undef    DXG_FX
#undef    DXG_FY
#include "DXG_MACROS.h"

#undef    DXV_CONST
#undef    DXV_FX
#undef    DXV_FY
#include "DXV_MACROS.h"

#undef    DYC_CONST
#undef    DYC_FX
#undef    DYC_FY
#include "DYC_MACROS.h"

#undef    DYF_CONST
#undef    DYF_FX
#undef    DYF_FY
#include "DYF_MACROS.h"

#undef    DYG_CONST
#undef    DYG_FX
#undef    DYG_FY
#include "DYG_MACROS.h"

#undef    DYU_CONST
#undef    DYU_FX
#undef    DYU_FY
#include "DYU_MACROS.h"

#undef    HFACC_CONST
#undef    HFACC_FX
#undef    HFACC_FY
#undef    HFACC_FXY
#include "HFACC_MACROS.h"

#undef    HFACS_CONST
#undef    HFACS_FX
#undef    HFACS_FY
#undef    HFACS_FXY
#include "HFACS_MACROS.h"

#undef    HFACW_CONST
#undef    HFACW_FX
#undef    HFACW_FY
#undef    HFACW_FXY
#include "HFACW_MACROS.h"

#undef    RECIP_DXC_CONST
#undef    RECIP_DXC_FX
#undef    RECIP_DXC_FY
#include "RECIP_DXC_MACROS.h"

#undef    RECIP_DXF_CONST
#undef    RECIP_DXF_FX
#undef    RECIP_DXF_FY
#include "RECIP_DXF_MACROS.h"

#undef    RECIP_DXG_CONST
#undef    RECIP_DXG_FX
#undef    RECIP_DXG_FY
#include "RECIP_DXG_MACROS.h"

#undef    RECIP_DXV_CONST
#undef    RECIP_DXV_FX
#undef    RECIP_DXV_FY
#include "RECIP_DXV_MACROS.h"

#undef    RECIP_DYC_CONST
#undef    RECIP_DYC_FX
#undef    RECIP_DYC_FY
#include "RECIP_DYC_MACROS.h"

#undef    RECIP_DYF_CONST
#undef    RECIP_DYF_FX
#undef    RECIP_DYF_FY
#include "RECIP_DYF_MACROS.h"

#undef    RECIP_DYG_CONST
#undef    RECIP_DYG_FX
#undef    RECIP_DYG_FY
#include "RECIP_DYG_MACROS.h"

#undef    RECIP_DYU_CONST
#undef    RECIP_DYU_FX
#undef    RECIP_DYU_FY
#include "RECIP_DYU_MACROS.h"

#undef    RECIP_HFACC_CONST
#undef    RECIP_HFACC_FX
#undef    RECIP_HFACC_FY
#undef    RECIP_HFACC_FXY
#include "RECIP_HFACC_MACROS.h"

#undef    RECIP_HFACS_CONST
#undef    RECIP_HFACS_FX
#undef    RECIP_HFACS_FY
#undef    RECIP_HFACS_FXY
#include "RECIP_HFACS_MACROS.h"

#undef    RECIP_HFACW_CONST
#undef    RECIP_HFACW_FX
#undef    RECIP_HFACW_FY
#undef    RECIP_HFACW_FXY
#include "RECIP_HFACW_MACROS.h"

#undef    XC_CONST
#undef    XC_FX
#undef    XC_FY
#include "XC_MACROS.h"

#undef    YC_CONST
#undef    YC_FX
#undef    YC_FY
#include "YC_MACROS.h"

#undef    RA_CONST
#undef    RA_FX
#undef    RA_FY
#include "RA_MACROS.h"
#include "RAW_MACROS.h"
#include "RAS_MACROS.h"

#undef    MASKW_CONST
#undef    MASKW_FX
#undef    MASKW_FY
#undef    MASKW_FXY
#include "MASKW_MACROS.h"

#undef    MASKS_CONST
#undef    MASKS_FX
#undef    MASKS_FY
#undef    MASKS_FXY
#include "MASKS_MACROS.h"

#undef    TANPHIATU_CONST
#undef    TANPHIATU_FX
#undef    TANPHIATU_FY
#include "TANPHIATU_MACROS.h"

#undef    TANPHIATV_CONST
#undef    TANPHIATV_FX
#undef    TANPHIATV_FY
#include "TANPHIATV_MACROS.h"

#undef    FCORI_CONST
#undef    FCORI_FX
#undef    FCORI_FY
#include "FCORI_MACROS.h"
