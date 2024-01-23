#include "GMREDI_OPTIONS.h"

CBOP
C !ROUTINE: GMREDI_WRITE_PICKUP

C !INTERFACE: ==========================================================
      SUBROUTINE GMREDI_WRITE_PICKUP( permPickup,
     &     suff, myTime, myIter, myThid )

C !DESCRIPTION:
C     Writes current state of passive tracers to a pickup file

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GMREDI.h"

C !INPUT PARAMETERS: ===================================================
C     permPickup      :: write a permanent pickup
C     suff            :: suffix for pickup file (eg. ckptA or 0000000010)
C     myTime          :: model time
C     myIter          :: time-step number
C     myThid          :: thread number
      LOGICAL permPickup
      CHARACTER*(*) suff
      _RL myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  none

#if ( defined GM_BATES_K3D || defined GM_GEOM_VARIABLE_K )
C !FUNCTIONS: ==========================================================
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C !LOCAL VARIABLES: ====================================================
C     m           :: loop index / field number
C     nm          :: record number
C     fp          :: pickup-file precision
C     glf         :: local flag for "globalFiles"
C     fn          :: character buffer for creating filename
C     nWrFlds     :: number of fields being written
C     listDim     :: dimension of "wrFldList" local array
C     wrFldList   :: list of written fields
C     msgBuf      :: Informational/error message buffer
      INTEGER m, nm, fp, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
#ifdef GM_BATES_K3D
# ifdef GM_GEOM_VARIABLE_K
      PARAMETER( listDim = 6+2*GM_Bates_NModes )
# else
      PARAMETER( listDim = 2+2*GM_Bates_NModes )
# endif
      INTEGER i,j,k,bi,bj,n
      _RL vec(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr,nSx,nSy)
#else /* GM_BATES_K3D */
      PARAMETER( listDim = 4 )
#endif /* GM_BATES_K3D */
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(8) fieldname
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( GM_useBatesK3d .OR. GM_useGEOM ) THEN

       lChar = ILNBLNK(suff)
       IF ( lChar.EQ.0 ) THEN
         WRITE(fn,'(2A)') 'pickup_gmredi'
       ELSE
         WRITE(fn,'(2A)') 'pickup_gmredi.',suff(1:lChar)
       ENDIF
       fp = precFloat64
       m  = 0
       nm = 0

#ifdef GM_BATES_K3D
       IF ( GM_useBatesK3d ) THEN

C     record number < 0 : a hack not to write meta files now:

C     Centre mode
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           vec(i,j,k,bi,bj) = modesC(1,i,j,k,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO
      ENDDO
      m = m + 1
      CALL WRITE_REC_3D_RL( fn, fp, Nr,
     &     vec, -m, myIter, myThid )
      fieldname='mode01C'
      IF (m.LE.listDim) wrFldList(m) = fieldname

C     Western Mode
      DO n=1,GM_Bates_NModes
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vec(i,j,k,bi,bj) = modesW(n,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       m = m + 1
       CALL WRITE_REC_3D_RL( fn, fp, Nr,
     &      vec, -m, myIter, myThid )
       WRITE(fieldname, '(A,I2.2,A)') 'mode',n,'W'
       IF (m.LE.listDim) wrFldList(m) = fieldname
      ENDDO

C     Southern Mode
      DO n=1,GM_Bates_NModes
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1-OLy,sNy+OLy
           DO i=1-OLx,sNx+OLx
            vec(i,j,k,bi,bj) = modesS(n,i,j,k,bi,bj)
           ENDDO
          ENDDO
         ENDDO
        ENDDO
       ENDDO
       m = m + 1
       CALL WRITE_REC_3D_RL( fn, fp, Nr,
     &      vec, -m, myIter, myThid )
       WRITE(fieldname, '(A,I2.2,A)') 'mode',n,'S'
       IF (m.LE.listDim) wrFldList(m) = fieldname
      ENDDO
C--------------------------

C-    switch to 2-D fields:
        nm = -m*Nr

C     The deformation radius (2D field)
      m = m + 1
      nm = nm-1
      CALL WRITE_REC_3D_RL( fn, fp, 1,
     &     Rdef, nm, myIter, myThid )
      fieldname = 'Rdef'
      IF (m.LE.listDim) wrFldList(m) = fieldname

c     if GM_useBatesK3d
       ENDIF
#endif /* GM_BATES_K3D */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef GM_GEOM_VARIABLE_K
       IF (GM_useGEOM ) THEN

        m = m + 1
        nm = nm-1
        CALL WRITE_REC_3D_RL( fn,fp,1,GEOM_ene,nm,myIter,myThid )
        fieldname = 'GEOM_ene'
        IF (m.LE.listDim) wrFldList(m) = fieldname

        m = m + 1
        nm = nm-1
        CALL WRITE_REC_3D_RL( fn,fp,1,ene_rhs_nm1,nm,myIter,myThid )
        fieldname = 'erhs_nm1'
        IF (m.LE.listDim) wrFldList(m) = fieldname

        m = m + 1
        nm = nm-1
        CALL WRITE_REC_3D_RL( fn,fp,1,ene_rhs_nm2,nm,myIter,myThid )
        fieldname = 'erhs_nm2'
        IF (m.LE.listDim) wrFldList(m) = fieldname

C     if GM_useGEOM
       ENDIF
#endif /* GM_GEOM_VARIABLE_K */

       nWrFlds = m
       IF ( nWrFlds.GT.listDim ) THEN
         WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &        'trying to write ',nWrFlds,' fields'
         CALL PRINT_ERROR( msgBuf, myThid )
         WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &        'field-list dimension (listDim=',listDim,') too small'
         CALL PRINT_ERROR( msgBuf, myThid )
         CALL ALL_PROC_DIE( myThid )
         STOP 'ABNORMAL END: S/R GMREDI_WRITE_PICKUP (list-size Pb)'
       ENDIF

#ifdef ALLOW_MDSIO
C     uses this specific S/R to write (with more informations) only meta files
       m  = 1
       nm = ABS(nm)
       IF ( nWrFlds*Nr .EQ. nm ) THEN
         m  = Nr
         nm = nWrFlds
       ENDIF
       glf  = globalFiles
       timList(1) = myTime
       CALL MDS_WR_METAFILES( fn, fp, glf, .FALSE.,
     &       0, 0, m, ' ',
     &       nWrFlds, wrFldList,
     &       1, timList, oneRL,
     &       nm, myIter, myThid )
#endif /* ALLOW_MDSIO */

C     if GM_useBatesK3d or GM_useGEOM
      ENDIF

#endif /* GM_BATES_K3D or GM_GEOM_VARIABLE_K */

      RETURN
      END
