C $Header: /u/gcmpack/MITgcm/model/inc/GRID_MACROS.h,v 1.7 1998/06/08 21:43:00 cnh Exp $
C
C     /==========================================================\
C     | GRID_MACROS.h                                            |
C     |==========================================================|
C     | These macros are used to substitute definitions for      |
C     | GRID.h variables for particular configurations.          |
C     | In setting these variables the following convention      |
C     | applies.                                                 |
C     | undef  phi_CONST   - Indicates the variable phi is fixed |
C     |                      in X, Y and Z.                      |
C     | undef  phi_FX      - Indicates the variable phi only     |  
C     |                      varies in X (i.e.not in X or Z).    |
C     | undef  phi_FY      - Indicates the variable phi only     |  
C     |                      varies in Y (i.e.not in X or Z).    |
C     | undef  phi_FXY     - Indicates the variable phi only     |  
C     |                      varies in X and Y ( i.e. not Z).    |
C     \==========================================================/

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

#undef    RDXC_CONST
#undef    RDXC_FX
#undef    RDXC_FY
#include "RDXC_MACROS.h"

#undef    RDXF_CONST
#undef    RDXF_FX
#undef    RDXF_FY
#include "RDXF_MACROS.h"

#undef    RDXG_CONST
#undef    RDXG_FX
#undef    RDXG_FY
#include "RDXG_MACROS.h"

#undef    RDXV_CONST
#undef    RDXV_FX
#undef    RDXV_FY
#include "RDXV_MACROS.h"

#undef    RDYC_CONST
#undef    RDYC_FX
#undef    RDYC_FY
#include "RDYC_MACROS.h"

#undef    RDYF_CONST
#undef    RDYF_FX
#undef    RDYF_FY
#include "RDYF_MACROS.h"

#undef    RDYG_CONST
#undef    RDYG_FX
#undef    RDYG_FY
#include "RDYG_MACROS.h"

#undef    RDYU_CONST
#undef    RDYU_FX
#undef    RDYU_FY
#include "RDYU_MACROS.h"

#undef    RHFACC_CONST   
#undef    RHFACC_FX
#undef    RHFACC_FY
#undef    RHFACC_FXY
#include "RHFACC_MACROS.h"

#undef    RHFACS_CONST   
#undef    RHFACS_FX
#undef    RHFACS_FY
#undef    RHFACS_FXY
#include "RHFACS_MACROS.h"

#undef    RHFACW_CONST   
#undef    RHFACW_FX
#undef    RHFACW_FY
#undef    RHFACW_FXY
#include "RHFACW_MACROS.h"

#undef    XC_CONST   
#undef    XC_FX
#undef    XC_FY
#include "XC_MACROS.h"

#undef    YC_CONST   
#undef    YC_FX
#undef    YC_FY
#include "YC_MACROS.h"

#undef    ZA_CONST   
#undef    ZA_FX
#undef    ZA_FY
#include "ZA_MACROS.h"

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
