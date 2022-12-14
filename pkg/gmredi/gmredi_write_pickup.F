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

#ifdef GM_BATES_K3D

C     === Functions ====
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
      INTEGER i,j,k,bi,bj,m,n, nm, fp, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 2+2*GM_K3D_NModes )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      _RL vec(1-Olx:sNx+Olx,1-Oly:sNy+Oly,Nr,nSx,nSy)
      CHARACTER*(8) fieldname
CEOP

#endif /* GM_BATES_K3D */

#ifdef GM_GEOM_VARIABLE_K

C     === Functions ====
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      INTEGER m, nm, fp, lChar
      LOGICAL glf
      _RL     timList(1)
      CHARACTER*(MAX_LEN_FNAM) fn
      INTEGER listDim, nWrFlds
      PARAMETER( listDim = 4 )
      CHARACTER*(8) wrFldList(listDim)
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(8) fieldname
CEOP

#endif /* GM_GEOM_VARIABLE_K */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef GM_BATES_K3D

      IF (.NOT. GM_useK3D) RETURN

      lChar = ILNBLNK(suff)
      IF ( lChar.EQ.0 ) THEN
        WRITE(fn,'(2A)') 'pickup_gmredi'
      ELSE
        WRITE(fn,'(2A)') 'pickup_gmredi.',suff(1:lChar)
      ENDIF
      fp = precFloat64
      m  = 0

C     record number < 0 : a hack not to write meta files now:

C     Centre mode
      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
        DO k=1,Nr
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx
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
      DO n=1,GM_K3D_NModes
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
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
      DO n=1,GM_K3D_NModes
       DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
         DO k=1,Nr
          DO j=1-Oly,sNy+Oly
           DO i=1-Olx,sNx+Olx
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
      
      nWrFlds = m
      IF ( nWrFlds.GT.listDim ) THEN
        WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &       'trying to write ',nWrFlds,' fields'
        CALL PRINT_ERROR( msgBuf, myThid )
        WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &       'field-list dimension (listDim=',listDim,') too small'
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
     &     0, 0, m, ' ',
     &     nWrFlds, wrFldList,
     &     1, timList, oneRL,
     &     nm, myIter, myThid )
#endif /* ALLOW_MDSIO */
C--------------------------

#endif /* GM_BATES_K3D */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef GM_GEOM_VARIABLE_K

      IF (GM_useGEOM) THEN
      
C--------------------------
#ifdef ALLOW_MDSIO
      
      IF ( GEOM_pickup_write_mdsio ) THEN

        lChar = ILNBLNK(suff)
        IF ( lChar.EQ.0 ) THEN
          WRITE(fn,'(2A)') 'pickup_gmredi'
        ELSE
          WRITE(fn,'(2A)') 'pickup_gmredi.',suff(1:lChar)
        ENDIF
        fp = precFloat64
        m  = 0
        
        nm = -m*Nr
        m = m + 1
        nm = nm-1
        CALL WRITE_REC_3D_RL( fn,fp,1,GEOM_ene,nm,myIter,myThid )
        fieldname = 'GEOM_ene'
        IF (m.LE.listDim) wrFldList(m) = fieldname
        
        m = m + 1
        nm = nm-1
        CALL WRITE_REC_3D_RL( fn,fp,1,ene_rhs_now,nm,myIter,myThid )
        fieldname = 'erhs_now'
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
        
        
        nWrFlds = m
        IF ( nWrFlds.GT.listDim ) THEN
          WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &         'trying to write ',nWrFlds,' fields'
          CALL PRINT_ERROR( msgBuf, myThid )
          WRITE(msgBuf,'(2A,I5,A)') 'GMREDI_WRITE_PICKUP: ',
     &         'field-list dimension (listDim=',listDim,') too small'
          CALL PRINT_ERROR( msgBuf, myThid )
          CALL ALL_PROC_DIE( myThid )
          STOP 'ABNORMAL END: S/R GMREDI_WRITE_PICKUP (list-size Pb)'
        ENDIF
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
     
        WRITE(*,'(2A)') 'GMREDI_WRITE_PICKUP: ',
     &       'writing GEOM msdio pickup'

      ENDIF
#endif /* ALLOW_MDSIO */
C--------------------------
#ifdef ALLOW_MNC
      IF ( GEOM_pickup_write_mnc ) THEN

        IF ( permPickup ) THEN
          WRITE(fn,'(A)') 'pickup_gmredi'
        ELSE
          WRITE(fn,'(A,A)') 'pickup_gmredi.',suff
        ENDIF
C       First ***define*** the file group name
        CALL MNC_CW_SET_UDIM(fn, 0, myThid)
        IF ( permPickup ) THEN
          CALL MNC_CW_SET_CITER(fn, 3, 3, myIter, 0, myThid)
        ELSE
          CALL MNC_CW_SET_CITER(fn, 2, -1, -1, -1, myThid)
        ENDIF
C       Then set the actual unlimited dimension
        CALL MNC_CW_SET_UDIM(fn, 1, myThid)
        CALL MNC_CW_I_W_S('I',fn,0,0,'iter', myIter, myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'GEOM_ene',   GEOM_ene,   myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'ene_rhs_now',ene_rhs_now,myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'ene_rhs_nm1',ene_rhs_nm1,myThid)
        CALL MNC_CW_RL_W('D',fn,0,0,'ene_rhs_nm2',ene_rhs_nm2,myThid)

        WRITE(*,'(2A)') 'GMREDI_WRITE_PICKUP: ',
     &       'writing GEOM mnc pickup'
      ENDIF
#endif /* ALLOW_MNC */
C--------------------------
      ENDIF
#endif /* GM_GEOM_VARIABLE_K */

      RETURN
      END
