C $Header: /u/gcmpack/MITgcm/pkg/mnc/mnc_cw_model_attr.F,v 1.5 2004/03/19 03:28:36 edhill Exp $
C $Name:  $
      
#include "MNC_OPTIONS.h"
      
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      SUBROUTINE MNC_CW_SET_GATTR( 
     I     fname,
     I     bi, bj, tnum,
     I     myThid ) 

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

      CALL MNC_FILE_REDEF(fname, myThid)

C     Global attributes
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'MITgcm_ref',
     &     'http://mitgcm.org', myThid )
      CALL MNC_FILE_ADD_ATTR_STR(fname, 'MITgcm_tag_index', 
     &     MNC_TAG_ID, myThid )
      CALL MNC_FILE_ADD_ATTR_DBL(fname, 'MITgcm_mnc_ver', 
     &     1, 0.1D0, myThid )

C     Grid info included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(fname,'tile_number', 1, tnum, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,'bi', 1, bi, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname,'bj', 1, bj, myThid)
      CALL MNC_FILE_ADD_ATTR_DBL(fname,'xC0', 1, xC0, myThid)
      CALL MNC_FILE_ADD_ATTR_DBL(fname,'yC0', 1, yC0, myThid)
      CALL MNC_FILE_ADD_ATTR_DBL(fname,'gravitySign', 
     &     1, gravitySign, myThid )
      CALL MNC_FILE_ADD_ATTR_DBL(fname,'rkFac', 1, rkFac, myThid)

#ifdef ALLOW_EXCH2
C     W2/exch2 information
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_nNeighbours', 1, exch2_nNeighbours(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_neighbourId', exch2_nNeighbours(tnum), 
     &     exch2_neighbourId(1,tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_pi', 
     &     2*exch2_nNeighbours(tnum), exch2_pi(1,1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_pj', 
     &     2*exch2_nNeighbours(tnum), exch2_pj(1,1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oi', 
     &     exch2_nNeighbours(tnum), exch2_oi(1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oj', 
     &     exch2_nNeighbours(tnum), exch2_oj(1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oi_f', 
     &     exch2_nNeighbours(tnum), exch2_oi_f(1,tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'exch2_oj_f', 
     &     exch2_nNeighbours(tnum), exch2_oj_f(1,tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_isNedge', 1, exch2_isNedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_isSedge', 1, exch2_isSedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_isEedge', 1, exch2_isEedge(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_isWedge', 1, exch2_isWedge(tnum), myThid)

      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_myFace', 1, exch2_myFace(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_txglobalo', 1, exch2_txglobalo(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_tyglobalo', 1, exch2_tyglobalo(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_tbasex', 1, exch2_tbasex(tnum), myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 
     &     'exch2_tbasey', 1, exch2_tbasey(tnum), myThid)
#endif
      
C     Model parameters (SIZE.h) included as attributes
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'sNx', 1, sNx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'sNy', 1, sNy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'OLx', 1, OLx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'OLy', 1, OLy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nSx', 1, nSx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nSy', 1, nSy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nPx', 1, nPx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'nPy', 1, nPy, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Nx', 1, Nx, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Ny', 1, Ny, myThid)
      CALL MNC_FILE_ADD_ATTR_INT(fname, 'Nr', 1, Nr, myThid)
      
      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

