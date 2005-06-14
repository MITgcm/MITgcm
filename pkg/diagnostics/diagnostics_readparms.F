C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/diagnostics_readparms.F,v 1.11 2005/06/14 22:30:02 molod Exp $
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
C     frequency :: Frequency (in s) of Output (ouput every "frequency" second)
C     timePhase :: phase (in s) within the "frequency" period to write output
C     levels    :: List Output Levels
C     fields    :: List Output Fields
C     filename  :: List Output Filename
C--   per level statistics output:
C     stat_freq   :: Frequency (in s) of statistics output 
C     stat_phase  :: phase (in s) to write statistics output
C     stat_region :: List of statistics output Regions
C     stat_fields :: List of statistics output Fields
C     stat_fname  :: List of statistics output Filename
      INTEGER     ldimLoc, kdimLoc, fdimLoc, rdimLoc
      PARAMETER ( ldimLoc = 2*numlists )
      PARAMETER ( kdimLoc = 2*numLevels )
      PARAMETER ( fdimLoc = 2*numperlist )
      PARAMETER ( rdimLoc = 2*nRegions+1 )
      _RL         frequency(ldimLoc), timePhase(ldimLoc)
      _RL         levels(kdimLoc,ldimLoc)
      _RL         stat_freq(ldimLoc), stat_phase(ldimLoc)
      CHARACTER*8 fields(fdimLoc,ldimLoc)
      CHARACTER*8 stat_fields(fdimLoc,ldimLoc)
      CHARACTER*80 filename(ldimLoc), blkFilName
      CHARACTER*80 stat_fname(ldimLoc)
      CHARACTER*8 fileflags(ldimLoc)
      CHARACTER*8 blk8c
      CHARACTER*(MAX_LEN_MBUF) msgBuf
      INTEGER stat_region(rdimLoc,ldimLoc)
      INTEGER ku, stdUnit
      INTEGER j,k,l,n,m
      INTEGER regionCount
      _RL undef, getcon
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK

C--   full level output:
      NAMELIST / diagnostics_list /
     &     frequency, timePhase, levels, fields, filename, fileflags,
     &     diag_mnc,
     &     diag_pickup_read,     diag_pickup_write,
     &     diag_pickup_read_mnc, diag_pickup_write_mnc

C--   per level statistics output:
      NAMELIST / DIAG_STATIS_PARMS /
     &     stat_freq, stat_phase, stat_region, stat_fields, 
     &     stat_fname, 
     &     diagSt_mnc 

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

      DO l = 1,ldimLoc
        stat_freq(l)  = 0.
        stat_phase(l) = UNSET_RL
        stat_fname(l) = blkFilName
        DO k = 1,rdimLoc
          stat_region(k,l) = UNSET_I
        ENDDO
        DO m = 1,fdimLoc
          stat_fields(m,l) = blk8c
        ENDDO
      ENDDO

      WRITE(msgBuf,'(2A)')
     &     ' DIAGNOSTICS_READPARMS: opening data.diagnostics'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.diagnostics',
     &     'DIAGNOSTICS_READPARMS', ku, myThid )

      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "diagnostics_list": start'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
      READ  (ku,NML=diagnostics_list)
      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "diagnostics_list": OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

C-    set default for statistics output according to the main flag
      diag_mnc = diag_mnc .AND. useMNC
      diagSt_mnc = diag_mnc

      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "DIAG_STATIS_PARMS": start'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)
c     STOP 'before reading namelist: DIAG_STATIS_PARMS'
      READ  (ku,NML=DIAG_STATIS_PARMS)
      WRITE(msgBuf,'(2A)') 'S/R DIAGNOSTICS_READPARMS,',
     &     ' read namelist "DIAG_STATIS_PARMS": OK'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

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

C     useMNC is confusing (can be T at this point & turned off later, whereas
C     for all other pkgs, model stops if use${PKG}= T with #undef ALLOW_${PKG})
#ifndef ALLOW_MNC
C     Fix to avoid running without getting any output:
      diag_mnc   = .FALSE.
      diagSt_mnc = .FALSE.
#endif

C     Fill Diagnostics Common Block with Namelist Info
      diagSt_mnc = diagSt_mnc .AND. useMNC
      diag_mdsio = (.NOT. diag_mnc) .OR. outputTypesInclusive
      diag_pickup_read_mnc  = diag_pickup_read_mnc .AND. diag_mnc
      diag_pickup_write_mnc = diag_pickup_write_mnc .AND. diag_mnc
      diag_pickup_read_mdsio  =
     &     diag_pickup_read .AND. (.NOT. diag_pickup_read_mnc)
      diag_pickup_write_mdsio = diag_pickup_write .AND.
     &     ((.NOT. diag_pickup_write_mnc) .OR. outputTypesInclusive)
      diagSt_ascii = (.NOT. diagSt_mnc) .OR. outputTypesInclusive

      DO l = 1,ldimLoc
       IF ( filename(L).NE.blkFilName .and. nlists.LT.numlists ) THEN
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
       ELSEIF (filename(L).NE.blkFilName) THEN
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &            'Exceed Max.Num. of list numlists=', numlists
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &    'when trying to add list l=', l
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,F17.6,2A)') 'DIAGNOSTICS_READPARMS: ',
     &    ' Frq=', frequency(l), ', filename: ', filename(l)
         CALL PRINT_ERROR( msgBuf , myThid )
         STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
       ENDIF
      ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C     Initialise DIAG_STATIS common block
      diagSt_nbLists = 0
      DO n = 1,numlists
        diagSt_freq(n) = 0.
        diagSt_phase(n) = 0.
        diagSt_nbFlds(n) = 0
        diagSt_ioUnit(n) = 0
        diagSt_Fname(n) = blkFilName
        DO j = 0,nRegions
          diagSt_region(j,n) = 0
        ENDDO
        DO m = 1,numperlist
          diagSt_Flds(m,n) = blk8c
          jSdiag(m,n) = 0
        ENDDO
      ENDDO

C     Fill Diagnostics Common Block with Namelist Info
      diagSt_ascii = (.NOT. diagSt_mnc) .OR. outputTypesInclusive

      DO l = 1,ldimLoc
       IF(stat_fname(L).NE.blkFilName.AND.
     .                            diagSt_nbLists.LT.numlists)THEN
         n = diagSt_nbLists + 1
         diagSt_freq(n) = stat_freq(l)
         IF ( stat_phase(l).NE. UNSET_RL ) THEN
           diagSt_phase(n) = stat_phase(l)
         ELSEIF ( stat_freq(l) .LT. 0. ) THEN
           diagSt_phase(n) = -0.5 _d 0 * stat_freq(l)
         ENDIF
         diagSt_Fname(n)  = stat_fname(l)
         regionCount = 0
         DO k=1,rdimLoc
           j = stat_region(k,l)
           IF ( j.NE.UNSET_I .AND. j.GE.0 .AND. j.LE.nRegions ) THEN
             diagSt_region(j,n) = 1
             regionCount = regionCount + 1
           ELSEIF ( j.NE.UNSET_I ) THEN
             WRITE(msgBuf,'(A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_READPARMS: region=',j,
     &         ' in list l=', l, ', stat_fname: ', stat_fname(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,A,I3,2A)')
     &       'DIAGNOSTICS_READPARMS: ==> exceed Max.Nb of regions',
     &       '(=',nRegions,' )'
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         IF ( regionCount.EQ.0 ) THEN
C-       no region selected => default is Global statistics (region Id: 0)
           diagSt_region(0,n) = 1
         ENDIF
         diagSt_nbFlds(n) = 0
         DO m=1,fdimLoc
           IF ( stat_fields(m,l).NE.blk8c .AND.
     &          diagSt_nbFlds(n).LT.numperlist ) THEN
             diagSt_nbFlds(n) = diagSt_nbFlds(n) + 1
             diagSt_Flds(diagSt_nbFlds(n),n) = stat_fields(m,l)
           ELSEIF ( stat_fields(m,l).NE.blk8c ) THEN
             WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &        'Exceed Max.Num. of Fields/list numperlist=', numperlist
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,3A,I3,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        'when trying to add stat_field (m=', m, 
     &        ' ): ',stat_fields(m,l)
             CALL PRINT_ERROR( msgBuf , myThid )
             WRITE(msgBuf,'(2A,I3,2A)') 'DIAGNOSTICS_READPARMS: ',
     &        ' in list l=', l, ', stat_fname: ', stat_fname(l)
             CALL PRINT_ERROR( msgBuf , myThid )
             STOP 'ABNORMAL END: S/R DIAGNOSTICS_READPARMS'
           ENDIF
         ENDDO
         diagSt_nbLists = diagSt_nbLists + 1
c        write(6,*) 'stat-list summary:',n,diagSt_nbFlds(n),regionCount
       ELSEIF ( stat_fname(L).NE.blkFilName ) THEN
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &            'Exceed Max.Num. of list numlists=', numlists
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,I3)') 'DIAGNOSTICS_READPARMS: ',
     &    'when trying to add stat_list l=', l
         CALL PRINT_ERROR( msgBuf , myThid )
         WRITE(msgBuf,'(2A,F17.6,2A)') 'DIAGNOSTICS_READPARMS: ',
     &    ' Frq=', stat_freq(l), ', stat_fname: ', stat_fname(l)
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
     &     ' DIAGNOSTICS_READPARMS: statistics diags. summary:'
      CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
      DO n = 1,diagSt_nbLists
        WRITE(msgBuf,'(2a)') 'Creating Stats. Output Stream: ',
     &                       diagSt_Fname(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(2(A,F17.6))') 'Frequency : ',diagSt_freq(n),
     &                               ' ; Phase: ', diagSt_phase(n)
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,'(A)') ' Regions : ' 
        l = 12
        DO j=0,nRegions
         IF ( diagSt_region(j,n).GE.1 ) THEN
          IF (l+3.LE.MAX_LEN_MBUF) WRITE(msgBuf,'(A,I3)') msgBuf(1:l),j
          l = l+3
         ENDIF
        ENDDO
        CALL PRINT_MESSAGE( msgBuf, stdUnit,SQUEEZE_RIGHT, myThid)
        WRITE(msgBuf,*) 'Fields:   ',
     &                 (' ',diagSt_Flds(l,n),l=1,diagSt_nbFlds(n))
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
