C $Header: /u/gcmpack/MITgcm/pkg/aim/Attic/atparam0.h,v 1.2 2001/02/02 21:36:29 adcroft Exp $
C $Namer: $

      INTEGER ISC
      PARAMETER (ISC=1)

      INTEGER NTRUN, MTRUN, IX
      PARAMETER ( NTRUN=21, MTRUN=21, IX=sNx )   

      INTEGER MX, MX2, IL, NTRUN1, MXP
      PARAMETER (MX=MTRUN+1 , MX2=2*MX)
      PARAMETER (IL=sNy, NTRUN1=NTRUN+1 )
      PARAMETER ( MXP=ISC*MTRUN+1 )
