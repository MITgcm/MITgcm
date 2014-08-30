C $Header: /u/gcmpack/MITgcm/pkg/dic/dic_write_pickup.F,v 1.12 2014/08/30 00:56:44 jmc Exp $
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
#include "DIC_ATMOS.h"

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

C     !LOCAL VARIABLES:
C     == Local variables ==
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER prec
#ifndef USE_ATMOSCO2
      INTEGER ioUnit
      _RL tmpFld(2)
      _RS dummyRS(1)
#endif
#ifdef DIC_BIOTIC
      LOGICAL glf
      _RL     timList(1)
      INTEGER j, nj
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 2 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
#endif

c     IF ( DIC_pickup_write_mdsio ) THEN
        prec = precFloat64

#ifndef USE_ATMOSCO2
        IF ( dic_int1.EQ.3 ) THEN
          WRITE(fn,'(A,A)') 'pickup_dic_co2atm.',suff
          ioUnit = 0
c#ifdef ALLOW_OPENAD
C-  keep it (commented out) in case these 2 vars become again active
c         tmpFld(1) = total_atmos_carbon%v
c         tmpFld(2) = atpco2%v
c#else /* ALLOW_OPENAD */
          tmpFld(1) = total_atmos_carbon
          tmpFld(2) = atpco2
c#endif /* ALLOW_OPENAD */
#ifdef ALLOW_MDSIO
          CALL MDS_WRITEVEC_LOC(
     I                         fn, prec, ioUnit,
     I                         'RL', 2, tmpFld, dummyRS,
     I                         0, 0, 1, myIter, myThid )
#endif
        ENDIF
#endif /* ndef USE_ATMOSCO2 */

#ifdef DIC_BIOTIC
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
        timList(1) = myTime
        CALL MDS_WR_METAFILES( fn, prec, glf, .FALSE.,
     &                         0, 0, j, ' ',
     &                         nWrFlds, wrFldList,
     &                         1, timList, oneRL,
     &                         nj, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* DIC_BIOTIC  */

c     ENDIF /* DIC_pickup_write_mdsio */

#endif /* ALLOW_DIC  */

      RETURN
      END
