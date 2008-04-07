C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_write_pickup.F,v 1.5 2008/04/07 20:31:16 dfer Exp $
C $Name:  $

#include "DIC_OPTIONS.h"

CBOP
C !ROUTINE: DIC_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_WRITE_PICKUP( permPickup,
     I                             suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes DIC arrays (needed for a restart) to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIC_VARS.h"

C !INPUT PARAMETERS: ===================================================
C     permPickup :: write a permanent pickup
C     suff       :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime     :: Current time in simulation
C     myIter     :: Current iteration number in simulation
C     myThid     :: My Thread Id number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL     myTime
      INTEGER myIter
      INTEGER myThid
CEOP

#ifdef ALLOW_DIC
#ifdef DIC_BIOTIC

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      LOGICAL glf
      INTEGER prec, j, nj
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 2 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf

c     IF ( DIC_pickup_write_mdsio ) THEN
        prec = precFloat64
        WRITE(fn,'(A,A)') 'pickup_dic.',suff
        j = 0

C     Firstly, write 3-D fields as consecutive records,

C-    switch to 2-D fields:
        nj = -j*Nr

C       record number < 0 : a hack not to write meta files now:
        j = j + 1
        nj = nj-1
        CALL WRITE_REC_3D_RL( fn, prec, 1, pH, nj, myIter, myThid )
        IF (j.LE.listDim) wrFldList(j) = 'DIC_pH2d'

C--------------------------
        nWrFlds = j
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'DIC_WRITE_PICKUP: ',
     &     'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'DIC_WRITE_PICKUP: ',
     &     'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          STOP 'ABNORMAL END: S/R DIC_WRITE_PICKUP (list-size Pb)'
        ENDIF

#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta
C     files
        j  = 1
        nj = ABS(nj)
        IF ( nWrFlds*Nr .EQ. nj ) THEN
          j  = Nr
          nj = nWrFlds
        ENDIF
        glf  = globalFiles
        CALL MDS_WR_METAFILES( fn, prec, glf, .FALSE.,
     &                         0, 0, j, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, myTime,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

c     ENDIF /* DIC_pickup_write_mdsio */

#endif /*  DIC_BIOTIC  */
#endif /*  ALLOW_DIC  */

      RETURN
      END
