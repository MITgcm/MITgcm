C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_readparms.F,v 1.9 2005/05/23 02:15:51 molod Exp $
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
#include "fizhi_io_comms.h"
#include "EEPARAMS.h"
#include "SIZE.h"
#include "PARAMS.h"

C     !INPUT PARAMETERS:
      integer myThid
CEOP

C     !LOCAL VARIABLES:
      character*(MAX_LEN_MBUF) msgBuf
      integer ku
      integer nymdbegin, nhmsbegin
      integer nymdcurrent, nhmscurrent
      real runlength

      namelist / fizhi_list / 
     .     nymdbegin, nhmsbegin,
     .     fizhi_mnc_write_pickup, fizhi_mnc_read_pickup,
     .     runlength

C     Set defaults
      fizhi_mdsio_read_pickup = .TRUE.
      fizhi_mdsio_write_pickup = .TRUE.
      fizhi_mnc_write_pickup = .FALSE.
      fizhi_mnc_read_pickup = .FALSE.
      runlength = 0.0

C     Read Fizhi Namelist
      WRITE(msgBuf,'(A)') ' FIZHI_READPARMS: opening data.fizhi'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,SQUEEZE_RIGHT,1)

      CALL OPEN_COPY_DATA_FILE('data.fizhi', 'FIZHI_READPARMS',
     &     ku,myThid )
      read  (ku,NML=fizhi_list)
      close (ku)

C     Read Supplemental Ascii File with Current Time Info
      open(10,file='datetime0',form='formatted')
      read(10,1000)nymdcurrent,nhmscurrent
      close (10)
 1000 format(i8,2x,i6)

C     Fill chronos Common Block with Namelist and Other File Info
      nymd0 = nymdbegin
      nhms0 = nhmsbegin
      nymd = nymdcurrent
      nhms = nhmscurrent

C     Echo Date and Time Info
      _BEGIN_MASTER(myThid)
      print *, '  Begin Date ',nymd0,'   Begin Time ',nhms0
      print *, 'Current Date ',nymd,' Current Time ',nhms
      _END_MASTER(myThid)

      return
      end

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
