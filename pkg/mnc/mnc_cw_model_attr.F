C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_model_attr.F,v 1.1 2004/02/05 00:13:47 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_SET_GATTR( 
     I     myThid, 
     I     fname,
     I     bi, bj, tnum ) 

      implicit none
#include "SIZE.h"
#include "EEPARAMS.h"
#include "EESUPPORT.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"

C     Arguments
      integer myThid, bi,bj, tnum
      character*(*) fname

      CALL MNC_FILE_REDEF(myThid,fname)

C     Global attributes
      CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'MITgcm_ref',
     &     'http://mitgcm.org' )
      CALL MNC_FILE_ADD_ATTR_STR(myThid,fname, 'MITgcm_tag_index', 
     &     MNC_TAG_ID )
      CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'MITgcm_mnc_ver', 
     &     1, 0.1D0 )

C     Grid info included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'tile_number', 1, tnum)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'bi', 1, bi)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'bj', 1, bj)
      CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'xC0', 1, xC0)
      CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'yC0', 1, yC0)
      CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'gravitySign', 
     &     1, gravitySign )
      CALL MNC_FILE_ADD_ATTR_DBL(myThid,fname, 'rkFac', 1, rkFac)
      
C     Model parameters (SIZE.h) included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'sNx', 1, sNx)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'sNy', 1, sNy)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'OLx', 1, OLx)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'OLy', 1, OLy)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nSx', 1, nSx)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nSy', 1, nSy)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nPx', 1, nPx)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'nPy', 1, nPy)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Nx', 1, Nx)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Ny', 1, Ny)
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 'Nr', 1, Nr)
      

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

