C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_readparms.F,v 1.19 2014/05/27 23:56:23 jmc Exp $
C $Name:  $

#include "FIZHI_OPTIONS.h"

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
CBOP 0
C     !ROUTINE: FIZHI_MNC_INIT

C     !INTERFACE:
      SUBROUTINE FIZHI_READPARMS( myThid )

C     !DESCRIPTION:
C     Read Fizhi Namelist and Get the Model Date and Time from File

C     !USES:
      implicit none

#include "chronos.h"
#include "SIZE.h"
#include "fizhi_ocean_coms.h"
#include "fizhi_io_comms.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      character*(MAX_LEN_MBUF) msgBuf
      integer ku, ku2
      integer nymdbegin, nhmsbegin
      integer nymdcurrent, nhmscurrent
      real runlength
      integer  nincr
      integer mmdd,hhmmss,nsecf2
      integer nymdend,nhmsend
      logical climsst, climsice

      namelist / fizhi_list /
     &     nymdbegin, nhmsbegin,
     &     fizhi_mnc_write_pickup, fizhi_mnc_read_pickup,
     &     runlength, climsst, climsice

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF ( .NOT.useFizhi ) THEN
C-    pkg FIZHI is not used
        _BEGIN_MASTER(myThid)
C-    Track pkg activation status:
C     print a (weak) warning if data.fizhi is found
         CALL PACKAGES_UNUSED_MSG( 'useFizhi', ' ', ' ' )
        _END_MASTER(myThid)
        RETURN
      ENDIF

C     Set defaults
      fizhi_mdsio_read_pickup = .TRUE.
      fizhi_mdsio_write_pickup = .TRUE.
      fizhi_mnc_write_pickup = .FALSE.
      fizhi_mnc_read_pickup = .FALSE.
      runlength = 0.0
      climsst = .TRUE.
      climsice = .TRUE.

C     Read Fizhi Namelist
      WRITE(msgBuf,'(A)') ' FIZHI_READPARMS: opening data.fizhi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.fizhi', 'FIZHI_READPARMS',
     &     ku,myThid )
      read  (ku,NML=fizhi_list)
      close (ku)

C     Read Supplemental Ascii File with Current Time Info
      CALL MDSFINDUNIT( ku2, myThid )
      open(ku2,file='datetime0',form='formatted')
      read(ku2,1000)nymdcurrent,nhmscurrent
      close (ku2)
 1000 format(i8,2x,i6)

C Change the length of the model run, ie, change ntimesteps
C  if runlength has been set in the fizhi namelist

      if(runlength.gt.0.) then
       mmdd = int(runlength)
       hhmmss = int((runlength - int(runlength))*1.e6)
       if(mmdd.lt.100) then
        nincr = nsecf2(hhmmss,mmdd,nymdcurrent)
       else
        call time2freq2(mmdd,nymdcurrent,nhmscurrent,nincr)
       endif
       nTimeSteps = int(nincr/deltaT)
       nEndIter = nIter0 + nTimeSteps
       endTime = startTime + deltaT*float(nTimeSteps)
       WRITE(msgBuf,'(A,I10)')
     &         ' CHANGING NUMBER OF MODEL TIMESTEPS TO',ntimesteps
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)
       WRITE(msgBuf,'(A,F12.2)') ' CHANGING END TIME TO',endtime
       CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)
      endif

C     Fill chronos Common Block with Namelist and Other File Info
      nymd0 = nymdbegin
      nhms0 = nhmsbegin
      nymd = nymdcurrent
      nhms = nhmscurrent

C     Fill ocean params common block with namelist into
      sstclim = climsst
      siceclim = climsice

C     Echo Date and Time Info
      _BEGIN_MASTER(myThid)
      print *, '  Begin Date ',nymd0,'   Begin Time ',nhms0
      print *, 'Current Date ',nymd,' Current Time ',nhms
      _END_MASTER(myThid)

      return
      end
