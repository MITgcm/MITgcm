C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_model_attr.F,v 1.4 2004/02/23 19:28:29 edhill Exp $
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
#ifdef ALLOW_EXCH2
#include "W2_EXCH2_TOPOLOGY.h"
#include "W2_EXCH2_PARAMS.h"
#endif

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

#ifdef ALLOW_EXCH2
C     W2/exch2 information
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_nNeighbours', 1, exch2_nNeighbours(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_neighbourId', exch2_nNeighbours(tnum), 
     &     exch2_neighbourId(1,tnum))

      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_pi', 2*exch2_nNeighbours(tnum), exch2_pi(1,1,tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_pj', 2*exch2_nNeighbours(tnum), exch2_pj(1,1,tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_oi', exch2_nNeighbours(tnum), exch2_oi(1,tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_oj', exch2_nNeighbours(tnum), exch2_oj(1,tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_oi_f', exch2_nNeighbours(tnum), exch2_oi_f(1,tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_oj_f', exch2_nNeighbours(tnum), exch2_oj_f(1,tnum))

      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_isNedge', 1, exch2_isNedge(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_isSedge', 1, exch2_isSedge(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_isEedge', 1, exch2_isEedge(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_isWedge', 1, exch2_isWedge(tnum))

      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_myFace', 1, exch2_myFace(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_txglobalo', 1, exch2_txglobalo(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_tyglobalo', 1, exch2_tyglobalo(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_tbasex', 1, exch2_tbasex(tnum))
      CALL MNC_FILE_ADD_ATTR_INT(myThid,fname, 
     &     'exch2_tbasey', 1, exch2_tbasey(tnum))
#endif
      
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

