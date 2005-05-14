C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_readparms.F,v 1.9 2005/05/14 20:45:28 jmc Exp $
C $Name:  $

#include "DIAG_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: DIAGNOSTICS_READPARMS

C     !INTERFACE:
      SUBROUTINE DIAGNOSTICS_READPARMS(myThid)

C     !DESCRIPTION:
C     Read Diagnostics Namelists to specify output sequence.

C     !USES:
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DIAGNOSTICS_SIZE.h"
#include "DIAGNOSTICS.h"

C     !INPUT PARAMETERS:
      INTEGER myThid
CEOP

C     !LOCAL VARIABLES:
C     ldimLoc :: Max Number of Lists
C     kdimLoc :: Max Number of Levels
C     fdimLoc :: Max Number of Fields
C     frequency :: Frequency of Output (ouput every "frequency" iteration)
C     levels    :: List Output Levels
C     fields    :: List Output Fields
C     filename  :: List Output Filename
      INTEGER     ldimLoc, kdimLoc, fdimLoc
      PARAMETER ( ldimLoc = 2*numlists )
      PARAMETER ( kdimLoc = 2*numLevels )
      PARAMETER ( fdimLoc = 2*numperlist )
      _RL         frequency(ldimLoc), timePhase(ldimLoc)
      _RL         levels(kdimLoc,ldimLoc)
      CHARACTER*8 fields(fdimLoc,ldimLoc)
      CHARACTER*80 filename(ldimLoc), blkFilName
      CHARACTER*8 fileflags(ldimLoc)
      CHARACTER*8 blk8c
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER ku, stdUnit
      INTEGER k,l,n,m,iL
      _RL undef, getcon
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

      NAMELIST / diagnostics_list /
     &     frequency, timePhase, levels, fields, filename, fileflags,
     &     diag_mnc,
     &     diag_pickup_read,     diag_pickup_write,
     &     diag_pickup_read_mnc, diag_pickup_write_mnc

C     Initialize and Read Diagnostics Namelist
      _BEGIN_MASTER(myThid)

      undef = getcon('UNDEF')
      blk8c  = '        '
      DO k=1,LEN(blkFilName)
        blkFilName(k:k) = ' '
      ENDDO

      DO l = 1,ldimLoc
        frequency(l) = 0.
        timePhase(l) = UNSET_RL
        filename (l) = blkFilName
C       eight spaces:        12345678
        fileflags(l)(1:8) = '        '
        DO k = 1,kdimLoc
          levels (k,l) = undef
        ENDDO
        DO m = 1,fdimLoc
          fields (m,l) = blk8c
        ENDDO
      ENDDO
      diag_mnc = useMNC
      diag_pickup_read      = .FALSE.
      diag_pickup_write     = .FALSE.
      diag_pickup_read_mnc  = .FALSE.
      diag_pickup_write_mnc = .FALSE.

      WRITE(msgBuf,'(A)')
     &     ' DIAGNOSTICS_READPARMS: opening data.diagnostics'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.diagnostics',
     &     'DIAGNOSTICS_READPARMS', ku, myThid )
      READ  (ku,NML=diagnostics_list)
      CLOSE (ku)

C     Initialise diag_choices common block
      nlists = 0
      DO n = 1,numlists
        freq(n) = 0.
        phase(n) = 0.
        nlevels(n) = 0
        nfields(n) = 0
        fnames(n) = blkFilName
        DO k = 1,numLevels
          levs(k,n) = 0
        ENDDO
        DO m = 1,numperlist
          flds(m,n) = blk8c
          jdiag(m,n) = 0
        ENDDO
      ENDDO

C     Fill Diagnostics Common Block with Namelist Info
      diag_mnc = diag_mnc .AND. useMNC
      diag_mdsio = (.NOT. diag_mnc) .OR. outputTypesInclusive
      diag_pickup_read_mnc  = diag_pickup_read_mnc .AND. diag_mnc
      diag_pickup_write_mnc = diag_pickup_write_mnc .AND. diag_mnc
      diag_pickup_read_mdsio  = 
     &     diag_pickup_read .AND. (.NOT. diag_pickup_read_mnc)
      diag_pickup_write_mdsio = diag_pickup_write .AND.
     &     ((.NOT. diag_pickup_write_mnc) .OR. outputTypesInclusive)

      DO l = 1,ldimLoc
       iL = ILNBLNK(filename(l))
       IF ( frequency(l).NE.0. .AND. iL.EQ.0 ) THEN
         WRITE(msgBuf,'(2A,I3,A,I6)') 'DIAGNOSTICS_READPARMS: ',
     &    'Empty File-name ! (list l=', l, ' ), freq:',frequency(l)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
       ENDIF
       IF ( frequency(l).NE.0. .AND. nlists.LT.numlists ) THEN
         n = nlists + 1
         freq(n)    = frequency(l)
         IF ( timePhase(l).NE. UNSET_RL ) THEN
           phase(n) = timePhase(l)
         ELSEIF ( frequency(l) .LT. 0. ) THEN
           phase(n) = -0.5 _d 0 * frequency(l)
         ENDIF
         fnames(n)  = filename (l)
         fflags(n)  = fileflags(l)
         nlevels(n) = 0
         IF ( levels(1,l).NE.undef ) THEN
           DO k=1,kdimLoc
             IF ( levels(k,l).NE.undef .AND.
     &            nlevels(n).LT.numLevels ) THEN
               nlevels(n) = nlevels(n) + 1
               levs(nlevels(n),n) = levels(k,l)
             ELSEIF ( levels(k,l).NE.undef ) THEN
              WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &         'Exceed Max.Num. of Levels numLevels=', numLevels
              CALL PRINT_ERROR( msgBuf , myThid )
              WRITE(msgBuf,'(2A,I3,A,F3.0)') 'DIAGNOSTICS_READPARMS: ',
     &         'when trying to add level(k=', k, ' )=', levels(k,l)
              CALL PRINT_ERROR( msgBuf , myThid )
              WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_READPARMS: ',
     &         ' for list l=', l, ', filename: ', filename(l)
              CALL PRINT_ERROR( msgBuf , myThid )
               STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
             ENDIF
           ENDDO
         ELSE
C-       will set levels later, once the Nb of levels of each diag is known
           nlevels(n) = -1
         ENDIF
         nfields(n) = 0
         DO m=1,fdimLoc
           IF ( fields(m,l).NE.blk8c .AND.
     &          nfields(n).LT.numperlist ) THEN
             nfields(n) = nfields(n) + 1
             flds(nfields(n),n) = fields(m,l)
           ELSEIF ( fields(m,l).NE.blk8c ) THEN
             WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &        'Exceed Max.Num. of Fields/list numperlist=', numperlist
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,3A,I3,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        'when trying to add field (m=', m, ' ): ',fields(m,l)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        ' in list l=', l, ', filename: ', filename(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         nlists = nlists + 1
c        write(6,*) 'list summary:',n,nfields(n),nlevels(n)
       ELSEIF ( frequency(l).NE.0. ) THEN
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &            'Exceed Max.Num. of list numlists=', numlists
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &    'when trying to add list l=', l
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I6,2A)') 'DIAGNOSTICS_READPARMS: ',
     &    ' Frq=', frequency(l), ', filename: ', filename(l)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C     Echo History List Data Structure
      stdUnit = standardMessageUnit
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     &     ' DIAGNOSTICS_READPARMS: active diagnostics summary:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      DO n = 1,nlists
        WRITE(msgBuf,'(2a)') 'Creating Output Stream: ',fnames(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
c       WRITE(msgBuf,*) 'Frequency: ',freq(n)
        WRITE(msgBuf,'(2(A,F17.6))') 'Frequency : ',freq(n),
     &                               ' ; Phase: ', phase(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        IF ( nlevels(n).EQ.-1 ) THEN
          WRITE(msgBuf,'(A,A)') ' Levels:    ','will be set later'
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        ELSE
         DO l=1,nlevels(n),20
          m = MIN(nlevels(n),l+19)
          WRITE(msgBuf,'(A,20F5.0)') ' Levels:    ', (levs(k,n),k=l,m)
          CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
         ENDDO
        ENDIF
        WRITE(msgBuf,*) 'Fields:   ',(' ',flds(l,n),l=1,nfields(n))
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      ENDDO
      WRITE(msgBuf,'(A)')
     & '-----------------------------------------------------'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      WRITE(msgBuf,'(A)')
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)

      _END_MASTER(myThid)

      RETURN
      END
