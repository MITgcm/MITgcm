C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_out.F,v 1.4 2004/12/18 02:18:55 edhill Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_OUT

C     !INTERFACE:
      SUBROUTINE  DIAGNOSTICS_OUT(
     I     listnum,
     I     myIter,
     I     myThid )

C     !DESCRIPTION:
C     Write output for diagnostics fields.
      
C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

#ifdef ALLOW_FIZHI
#include "fizhi_SIZE.h"
#else
      INTEGER Nrphys
      PARAMETER (Nrphys=0)
#endif


C     !INPUT PARAMETERS:
C     listnum :: Diagnostics list number being written
C     myIter  :: current iteration number
C     myThid  :: my Thread Id number
      INTEGER listnum, myIter, myThid
CEOP

C     !LOCAL VARIABLES:
      INTEGER i, j, k, m, n, bi, bj
      CHARACTER*8 parms1
      CHARACTER*3 mate_index
      INTEGER mate
      _RL qtmp1(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL qtmp2(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr+Nrphys,nSx,nSy)
      _RL undef, getcon
      EXTERNAL getcon
      INTEGER ILNBLNK
      EXTERNAL ILNBLNK
      INTEGER ilen

      CHARACTER*(MAX_LEN_FNAM) pref
      CHARACTER*(MAX_LEN_MBUF) suff
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(80) fn
      LOGICAL glf
#ifdef ALLOW_MNC
      INTEGER ii
      CHARACTER*(MAX_LEN_FNAM) diag_mnc_bn

      INTEGER CW_DIMS, NLEN
      PARAMETER ( CW_DIMS = 10 )
      PARAMETER ( NLEN    = 80 )
      INTEGER dim(CW_DIMS), ib(CW_DIMS), ie(CW_DIMS)
      CHARACTER*(NLEN) dn(CW_DIMS)
      CHARACTER*(NLEN) dn_blnk
#endif /*  ALLOW_MNC  */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      undef = getcon('UNDEF')
      glf = globalFiles
      WRITE(suff,'(I10.10)') myIter
      pref = fnames(listnum)
      ilen=ILNBLNK( pref )
      WRITE( fn, '(A,A,A)' ) pref(1:ilen),'.',suff(1:10)

#ifdef ALLOW_MNC
      IF (useMNC .AND. diag_mnc) THEN
        DO i = 1,MAX_LEN_FNAM
          diag_mnc_bn(i:i) = ' '
        ENDDO
        DO i = 1,NLEN
          dn_blnk(i:i) = ' '
        ENDDO
c       WRITE( diag_mnc_bn, '(A,A)' ) 'diag.', pref(1:ilen)
        WRITE( diag_mnc_bn, '(A)' ) pref(1:ilen)

C       Update the record dimension by writing the iteration number
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, -1, myThid)
        CALL MNC_CW_I_W_S('I',diag_mnc_bn,0,0,'T',myIter,myThid)
        CALL MNC_CW_SET_UDIM(diag_mnc_bn, 0, myThid)

        dn(1)(1:NLEN) = dn_blnk(1:NLEN)
        WRITE(dn(1),'(a,i6.6)') 'Zd', nlevels(listnum)
        dim(1) = nlevels(listnum)
        ib(1)  = 1
        ie(1)  = nlevels(listnum)

        CALL MNC_CW_ADD_GNAME('diag_levels', 1, 
     &       dim, dn, ib, ie, myThid)
        CALL MNC_CW_ADD_VNAME('level_indicies', 'diag_levels', 
     &       0,0, myThid)
        CALL MNC_CW_ADD_VATTR_TEXT('level_indicies','description',
     &    'Idicies of vertical levels within the data source arrays',
     &       myThid)
        
        CALL MNC_CW_RL_W('I',diag_mnc_bn,0,0,
     &       'level_indicies', levs(1,listnum), myThid)

        CALL MNC_CW_DEL_VNAME('level_indicies', myThid)
        CALL MNC_CW_DEL_GNAME('diag_levels', myThid)
      ENDIF
#endif /*  ALLOW_MNC  */

      DO n = 1,nfields(listnum)
        m = jdiag(n,listnum)
        parms1 = gdiag(m)(1:8)
        IF ( idiag(m).NE.0 .AND. parms1(5:5).NE.'D' ) THEN
C--     Start processing 1 Fld :

          IF ( ndiag(m).EQ.0 ) THEN
C-        Empty diagnostics case :

            _BEGIN_MASTER( myThid )
            WRITE(msgBuf,'(A,I10)')
     &        '- WARNING - from DIAGNOSTICS_OUT at iter=', myIter
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I4,3A,I3,2A)')
     &       '- WARNING -   diag.#',m, ' : ',flds(n,listnum), 
     &       ' (#',n,' ) in outp.Stream: ',fnames(listnum)
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A,I2,A)')
     &       '- WARNING -   has not been filled (ndiag=',ndiag(m),' )'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            WRITE(msgBuf,'(A)')
     &       'WARNING DIAGNOSTICS_OUT  => write ZEROS instead'
            CALL PRINT_MESSAGE( msgBuf, errorMessageUnit, 
     &                          SQUEEZE_RIGHT, myThid)
            _END_MASTER( myThid )
            DO bj = myByLo(myThid), myByHi(myThid)
              DO bi = myBxLo(myThid), myBxHi(myThid)
                DO k = 1,nlevels(listnum)
                  DO j = 1-OLy,sNy+OLy
                    DO i = 1-OLx,sNx+OLx
                      qtmp1(i,j,k,bi,bj) = 0. _d 0
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO
            ENDDO

          ELSE
C-        diagnostics is not empty :

            IF ( myThid.EQ.1 ) 
     &             WRITE(6,2000) m,cdiag(m),ndiag(m),gdiag(m)
            IF ( parms1(5:5).NE.'C' ) THEN

              DO k = 1,nlevels(listnum)
                CALL GETDIAG (levs(k,listnum),m,undef,qtmp1,myThid)
              ENDDO

C             Check for Mate of a Vector Diagnostic
C             -------------------------------------
              IF ( parms1(1:1).EQ.'U' .OR. parms1(1:1).EQ.'V' ) THEN
                mate_index = parms1(6:8)
                READ (mate_index,'(I3)') mate
                IF ( idiag(mate).NE.0 ) THEN
                  IF ( myThid.EQ.1 )
     &                 WRITE(6,2001) cdiag(m),mate,cdiag(mate)
                ELSE
                  IF ( myThid.EQ.1 )
     &                 WRITE(6,2002) cdiag(m),mate,cdiag(mate)
                ENDIF
              ENDIF
                
            ELSE

C             Check for Mate of a Counter Diagnostic
C             --------------------------------------
              mate_index = parms1(6:8)
              READ (mate_index,'(I3)') mate
              IF ( myThid.EQ.1 )
     &                 WRITE(6,2003) cdiag(m),mate,cdiag(mate)
              DO k = 1,nlevels(listnum)
                CALL getdiag2(levs(k,listnum),m,undef,qtmp1,myThid)
                CALL getdiag2(levs(k,listnum),mate,undef,qtmp2,myThid)
                DO bj = myByLo(myThid), myByHi(myThid)
                  DO bi = myBxLo(myThid), myBxHi(myThid)
                    DO j = 1,sNy
                      DO i = 1,sNx
                        IF (qtmp2(i,j,k,bi,bj).NE.0.) THEN
                          qtmp1(i,j,k,bi,bj) = 
     &                         qtmp1(i,j,k,bi,bj) / qtmp2(i,j,k,bi,bj)
                        ELSE
                          qtmp1(i,j,k,bi,bj) = undef
                        ENDIF
                      ENDDO
                    ENDDO
                  ENDDO
                ENDDO
              ENDDO

            ENDIF

C-        end of empty diag / not empty block
          ENDIF

#ifdef ALLOW_MDSIO
C         Prepare for mdsio optionality
          IF (diag_mdsio) THEN
            CALL mdswritefield_new(fn,writeBinaryPrec,glf,'RL',
     &           Nr+Nrphys,nlevels(listnum),qtmp1,n,myIter,myThid)
          ENDIF
#endif /*  ALLOW_MDSIO  */

#ifdef ALLOW_MNC
          IF (useMNC .AND. diag_mnc) THEN

            _BEGIN_MASTER( myThid )

            DO ii = 1,CW_DIMS
              dn(ii)(1:NLEN) = dn_blnk(1:NLEN)
            ENDDO

            dn(1)(1:2) = 'Xd'
            dim(1) = sNx + 2*OLx
            ib(1)  = OLx + 1
            ie(1)  = OLx + sNx
            dn(2)(1:2) = 'Yd'
            dim(2) = sNy + 2*OLy
            ib(2)  = OLy + 1
            ie(2)  = OLy + sNy

C           Z is special since it varies
            WRITE(dn(3),'(a,i6.6)') 'Zd', nlevels(listnum)
            dim(3) = Nr+Nrphys
            ib(3)  = 1
            ie(3)  = nlevels(listnum)

            CALL MNC_CW_ADD_GNAME('diag_cw_temp', 3, 
     &             dim, dn, ib, ie, myThid)
            CALL MNC_CW_ADD_VNAME(cdiag(m), 'diag_cw_temp', 
     &             4,5, myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),'description',
     &             tdiag(m),myThid)
            CALL MNC_CW_ADD_VATTR_TEXT(cdiag(m),'units',
     &             udiag(m),myThid)

            CALL MNC_CW_RL_W('D',diag_mnc_bn,0,0,
     &             cdiag(m), qtmp1, myThid)

            CALL MNC_CW_DEL_VNAME(cdiag(m), myThid)
            CALL MNC_CW_DEL_GNAME('diag_cw_temp', myThid)

            _END_MASTER( myThid )

          ENDIF
#endif /*  ALLOW_MNC  */

C--     end of Processing Fld # n
        ENDIF
      ENDDO

 2000 format(1x,'Computing Diagnostic # ',i3,2x,a8,5x,'Counter: ',
     &     i4,6x,'Parms: ',a16)
 2001 format(1x,'          Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' exists ')
 2002 format(1x,'        Vector  Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8,' not enabled')
 2003 format(1x,'      use Counter Mate for  ',a8,5x,
     &     'Diagnostic # ',i3,2x,a8)
      
      RETURN 
      END
                                                                    
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
