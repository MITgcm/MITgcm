C $Header: /u/gcmpack/MITgcm/model/inc/GRID_MACROS.h,v 1.3 1998/05/27 05:57:02 cnh Exp $
C
C     /==========================================================\
C     | GRID_MACROS.h                                            |
C     |==========================================================|
C     | These macros are used to substitute definitions for      |
C     | GRID.h variables for particular configurations.          |
C     | In setting these variables the following convention      |
C     | applies.                                                 |
C     | define phi_CONST   - Indicates the variable phi is fixed |
C     |                      in X, Y and Z.                      |
C     | define phi_FX      - Indicates the variable phi only     |  
C     |                      varies in X.                        |
C     | define phi_FY      - Indicates the variable phi only     |  
C     |                      varies in Y.                        |
C     | define phi_FXY     - Indicates the variable phi only     |  
C     |                      varies in X and Y ( i.e. not Z).    |
C     \==========================================================/

#undef  DXC_CONST   
#undef  DXC_FX
#undef  DXC_FY
#include "DXC_MACROS.h"

#undef  DXF_CONST   
#undef  DXF_FX
#undef  DXF_FY
#include "DXF_MACROS.h"

#undef  DXG_CONST   
#undef  DXG_FX
#undef  DXG_FY
#include "DXG_MACROS.h"

#undef  DXV_CONST   
#undef  DXV_FX
#undef  DXV_FY
#include "DXV_MACROS.h"

#undef  DYC_CONST   
#undef  DYC_FX
#undef  DYC_FY
#include "DYC_MACROS.h"

#undef  DYF_CONST   
#undef  DYF_FX
#undef  DYF_FY
#include "DYF_MACROS.h"

#undef  DYG_CONST   
#undef  DYG_FX
#undef  DYG_FY
#include "DYG_MACROS.h"

#undef  DYU_CONST   
#undef  DYU_FX
#undef  DYU_FY
#include "DYU_MACROS.h"
