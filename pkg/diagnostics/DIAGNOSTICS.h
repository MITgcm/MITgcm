C $Header: /u/gcmpack/MITgcm/pkg/diagnostics/DIAGNOSTICS.h,v 1.8 2005/05/20 07:28:49 jmc Exp $
C $Name:  $

C ======================================================================
C  Common blocks for diagnostics package.
C  - diagarrays contains the master list of diagnostics and parameters
C        ndiagt :: total number of available diagnostics
C         cdiag - character names
C         idiag - slot number in large diagnostic array
C         kdiag - number of levels associated with the diagnostic
C         ndiag - counter for number of times diagnostic is added
C         tdiag - description of field in diagnostic
C         gdiag - parser field with characteristics of the diagnostics
C         udiag - physical units of the diagnostic field
C  - diagnostics contains the large array containing diagnostic fields
C         qdiag - diagnostic fields array
C        qSdiag - storage array for diagnostics of (per level) statistics 
C  - diag_choices contains the user-chosen list of fields to store
C         jdiag - short-list (active diag.) to long-list (available diag.)
C                 pointer
C  - diag_statis  contains the user-chosen list of statistics to store
C ======================================================================

C diagarrays common

      integer        ndiagt

      character*8    cdiag(ndiagMax)
      integer        idiag(ndiagMax)
      integer        kdiag(ndiagMax)
      integer        ndiag(ndiagMax)
c     integer        mdiag(ndiagMax)
      character*80   tdiag(ndiagMax)
      character*16   gdiag(ndiagMax)
      character*16   udiag(ndiagMax)

      common /diagarrays/ ndiagt 
      common /diagarrays/ cdiag
      common /diagarrays/ idiag
      common /diagarrays/ kdiag
      common /diagarrays/ ndiag
c     common /diagarrays/ mdiag
      common /diagarrays/ tdiag
      common /diagarrays/ gdiag
      common /diagarrays/ udiag

      CHARACTER*8   CUFLUX
      CHARACTER*8   CVFLUX
      CHARACTER*8   CHFLUX
      CHARACTER*8   CEFLUX
      CHARACTER*8   CQICE
      CHARACTER*8   CRADLWG
      CHARACTER*8   CRADSWG
      CHARACTER*8   CRI
      CHARACTER*8   CCT
      CHARACTER*8   CCU
      CHARACTER*8   CKH
      CHARACTER*8   CKM
      CHARACTER*8   CTURBU
      CHARACTER*8   CTURBV
      CHARACTER*8   CTURBT
      CHARACTER*8   CTURBQ
      CHARACTER*8   CMOISTT
      CHARACTER*8   CMOISTQ
      CHARACTER*8   CRADLW
      CHARACTER*8   CRADSW
      CHARACTER*8   CPREACC
      CHARACTER*8   CPRECON
      CHARACTER*8   CTUFLUX
      CHARACTER*8   CTVFLUX
      CHARACTER*8   CTTFLUX
      CHARACTER*8   CTQFLUX
      CHARACTER*8   CCN
      CHARACTER*8   CWINDS
      CHARACTER*8   CDTSRF
      CHARACTER*8   CTGROUND
      CHARACTER*8   CTS
      CHARACTER*8   CDTG
      CHARACTER*8   CQG
      CHARACTER*8   CQS
      CHARACTER*8   CTGRLW
      CHARACTER*8   CLWGUP
      CHARACTER*8   COLR
      CHARACTER*8   COLRCLR
      CHARACTER*8   CLWGCLR
      CHARACTER*8   CLWCLR
      CHARACTER*8   CTLW
      CHARACTER*8   CSHRAD
      CHARACTER*8   COZLW
      CHARACTER*8   CCLDRAS
      CHARACTER*8   CCLDTOT
      CHARACTER*8   CLWGDOWN
      CHARACTER*8   CGWDT
      CHARACTER*8   CRADSWT
      CHARACTER*8   CTAUCLD
      CHARACTER*8   CTAUCLDC
      CHARACTER*8   CCLDLOW
      CHARACTER*8   CEVAP
      CHARACTER*8   CDPDT
      CHARACTER*8   COMEGA
      CHARACTER*8   CDUDT
      CHARACTER*8   CDVDT
      CHARACTER*8   CDTDT
      CHARACTER*8   CDQDT
      CHARACTER*8   CVORT
      CHARACTER*8   CDTLS  
      CHARACTER*8   CDQLS  
      CHARACTER*8   CUSTAR
      CHARACTER*8   CZ0
      CHARACTER*8   CFRQTRB
      CHARACTER*8   CPBL
      CHARACTER*8   CSWCLR
      CHARACTER*8   COSR
      CHARACTER*8   COSRCLR
      CHARACTER*8   CCLDMAS
      CHARACTER*8   CUWND
      CHARACTER*8   CVWND
      CHARACTER*8   CTMPU
      CHARACTER*8   CSPHU
      CHARACTER*8   CRFT
      CHARACTER*8   CPS  
      CHARACTER*8   CTKE  
      CHARACTER*8   CSWGCLR
      CHARACTER*8   CSDIAG1
      CHARACTER*8   CSDIAG2
      CHARACTER*8   CUDIAG1
      CHARACTER*8   CUDIAG2
      CHARACTER*8   CDIABU
      CHARACTER*8   CDIABV
      CHARACTER*8   CDIABT
      CHARACTER*8   CDIABQ
      CHARACTER*8   CRFU
      CHARACTER*8   CRFV
      CHARACTER*8   CGWDU
      CHARACTER*8   CGWDV
      CHARACTER*8   CGWDUS
      CHARACTER*8   CGWDVS
      CHARACTER*8   CGWDUT
      CHARACTER*8   CGWDVT
      CHARACTER*8   CLZRAD
      CHARACTER*8   CTRBQLIQ
      CHARACTER*8   CTRBFCC 
      CHARACTER*8   CSLP
      CHARACTER*8   CVAVEUQ
      CHARACTER*8   CVAVEVQ
      CHARACTER*8   CVAVEUT
      CHARACTER*8   CVAVEVT
      CHARACTER*8   CCLDFRC
      CHARACTER*8   CTPW 
      CHARACTER*8   CU2M
      CHARACTER*8   CV2M
      CHARACTER*8   CT2M
      CHARACTER*8   CQ2M
      CHARACTER*8   CU10M
      CHARACTER*8   CV10M
      CHARACTER*8   CT10M
      CHARACTER*8   CQ10M
      CHARACTER*8   CDTRAIN
      CHARACTER*8   CQFILL
      CHARACTER*8   CVAVEQFIL
      CHARACTER*8   CSHAPU
      CHARACTER*8   CSHAPV
      CHARACTER*8   CSHAPT
      CHARACTER*8   CSHAPQ
      CHARACTER*8   CSDIAG3
      CHARACTER*8   CSDIAG4
      CHARACTER*8   CSDIAG5
      CHARACTER*8   CSDIAG6
      CHARACTER*8   CSDIAG7
      CHARACTER*8   CSDIAG8
      CHARACTER*8   CSDIAG9
      CHARACTER*8   CSDIAG10
      CHARACTER*8   CUDIAG3
      CHARACTER*8   CUDIAG4
      CHARACTER*8   CUDIAG5
      CHARACTER*8   CUDIAG6
      CHARACTER*8   CUDIAG7
      CHARACTER*8   CUDIAG8
      CHARACTER*8   CUDIAG9
      CHARACTER*8   CUDIAG10
      CHARACTER*8   CCLDMID
      CHARACTER*8   CCLDHI
      CHARACTER*8   CTAULOW
      CHARACTER*8   CTAUMID
      CHARACTER*8   CTAUHI
      CHARACTER*8   CCLDNP
      CHARACTER*8   CCLDPRS
      CHARACTER*8   CCTPCNT
      CHARACTER*8   CCLDTMP
      CHARACTER*8   CCTTCNT
      CHARACTER*8   CTAULOWC
      CHARACTER*8   CTAUMIDC
      CHARACTER*8   CTAUHIC
      CHARACTER*8   CTCANOPY
      CHARACTER*8   CTDEEP
      CHARACTER*8   CQCANOPY
      CHARACTER*8   CSMSHAL
      CHARACTER*8   CSMROOT
      CHARACTER*8   CSMDEEP
      CHARACTER*8   CCAPACITY
      CHARACTER*8   CSNOW
      CHARACTER*8   CRAINCON
      CHARACTER*8   CRAINLSP
      CHARACTER*8   CSNOWFALL
      CHARACTER*8   CRUNOFF
      CHARACTER*8   CFWSOIL
      CHARACTER*8   CGDRAIN
      CHARACTER*8   CSNOWMELT
      CHARACTER*8   CERESV
      CHARACTER*8   CESOIL
      CHARACTER*8   CEVEG
      CHARACTER*8   CESNOW
      CHARACTER*8   CPARDF
      CHARACTER*8   CPARDR
      CHARACTER*8   CLAI
      CHARACTER*8   CGREEN
      CHARACTER*8   CDLWDTC
      CHARACTER*8   CDHDTC
      CHARACTER*8   CDEDTC
      CHARACTER*8   CVDTMOIST
      CHARACTER*8   CVDTTURB
      CHARACTER*8   CVDTRADLW
      CHARACTER*8   CVDTRADSW
      CHARACTER*8   CPSUBCLD
      CHARACTER*8   CPSUBCLDC
      CHARACTER*8   CLCL
      CHARACTER*8   CSDIAGC
      CHARACTER*8   CSDIAGCC
      CHARACTER*8   CEVPOT
      CHARACTER*8   CRHCHECK
      CHARACTER*8   CDHDQA
      CHARACTER*8   CDEDQA
      CHARACTER*8   CDTC
      CHARACTER*8   CDQC
      CHARACTER*8   CTCDTC
      CHARACTER*8   CRADDTC
      CHARACTER*8   CSENSDTC
      CHARACTER*8   CLATDTC
      CHARACTER*8   CTDDTC
      CHARACTER*8   CQCDTC
      CHARACTER*8   CALBEDO
      CHARACTER*8   CALBVISDR
      CHARACTER*8   CALBVISDF
      CHARACTER*8   CALBNIRDR
      CHARACTER*8   CALBNIRDF
      CHARACTER*8   CTAUAVE
      CHARACTER*8   CALBEDOC
      CHARACTER*8   CRHCHECKT
      CHARACTER*8   CRHCHECKQ
      CHARACTER*8   CCLDLSP
      CHARACTER*8   CLZLOW
      CHARACTER*8   CLZMID
      CHARACTER*8   CLZHI
      CHARACTER*8   CLZTOT
      CHARACTER*8   CCAPE
      CHARACTER*8   CHSUBCL
      CHARACTER*8   CHSTAR
      CHARACTER*8   CCONT 
      CHARACTER*8   CCONQ 
      CHARACTER*8   CLST
      CHARACTER*8   CLSQ
      CHARACTER*8   CCONEVPT
      CHARACTER*8   CCONEVPQ
      CHARACTER*8   CLSEVPT
      CHARACTER*8   CLSEVPQ
      CHARACTER*8   CGWCUMID
      CHARACTER*8   CGWCVMID
      CHARACTER*8   CCLDSTD
      CHARACTER*8   CGWCUBAR
      CHARACTER*8   CGWCVBAR
      CHARACTER*8   CGWCUS
      CHARACTER*8   CGWCVS
      CHARACTER*8   CGWCU
      CHARACTER*8   CGWCV

      EQUIVALENCE ( CDIAG( 1) ,  CUFLUX  )
      EQUIVALENCE ( CDIAG( 2) ,  CVFLUX  )
      EQUIVALENCE ( CDIAG( 3) ,  CHFLUX  )
      EQUIVALENCE ( CDIAG( 4) ,  CEFLUX  )
      EQUIVALENCE ( CDIAG( 5) ,  CQICE   )
      EQUIVALENCE ( CDIAG( 6) ,  CRADLWG )
      EQUIVALENCE ( CDIAG( 7) ,  CRADSWG )
      EQUIVALENCE ( CDIAG( 8) ,  CRI     )
      EQUIVALENCE ( CDIAG( 9) ,  CCT     )
      EQUIVALENCE ( CDIAG(10) ,  CCU     )
      EQUIVALENCE ( CDIAG(11) ,  CKH     )
      EQUIVALENCE ( CDIAG(12) ,  CKM     )
      EQUIVALENCE ( CDIAG(13) ,  CTURBU  )
      EQUIVALENCE ( CDIAG(14) ,  CTURBV  )
      EQUIVALENCE ( CDIAG(15) ,  CTURBT  )
      EQUIVALENCE ( CDIAG(16) ,  CTURBQ  )
      EQUIVALENCE ( CDIAG(17) ,  CMOISTT )
      EQUIVALENCE ( CDIAG(18) ,  CMOISTQ )
      EQUIVALENCE ( CDIAG(19) ,  CRADLW  )
      EQUIVALENCE ( CDIAG(20) ,  CRADSW  )
      EQUIVALENCE ( CDIAG(21) ,  CPREACC )
      EQUIVALENCE ( CDIAG(22) ,  CPRECON )
      EQUIVALENCE ( CDIAG(23) ,  CTUFLUX )
      EQUIVALENCE ( CDIAG(24) ,  CTVFLUX )
      EQUIVALENCE ( CDIAG(25) ,  CTTFLUX )
      EQUIVALENCE ( CDIAG(26) ,  CTQFLUX )
      EQUIVALENCE ( CDIAG(27) ,  CCN     )
      EQUIVALENCE ( CDIAG(28) ,  CWINDS  )
      EQUIVALENCE ( CDIAG(29) ,  CDTSRF  )
      EQUIVALENCE ( CDIAG(30) ,  CTGROUND)
      EQUIVALENCE ( CDIAG(31) ,  CTS     )
      EQUIVALENCE ( CDIAG(32) ,  CDTG    )
      EQUIVALENCE ( CDIAG(33) ,  CQG     )
      EQUIVALENCE ( CDIAG(34) ,  CQS     )
      EQUIVALENCE ( CDIAG(35) ,  CTGRLW  )
      EQUIVALENCE ( CDIAG(36) ,  CLWGUP  )
      EQUIVALENCE ( CDIAG(37) ,  COLR    )
      EQUIVALENCE ( CDIAG(38) ,  COLRCLR )
      EQUIVALENCE ( CDIAG(39) ,  CLWGCLR )
      EQUIVALENCE ( CDIAG(40) ,  CLWCLR  )
      EQUIVALENCE ( CDIAG(41) ,  CTLW    )
      EQUIVALENCE ( CDIAG(42) ,  CSHRAD  )
      EQUIVALENCE ( CDIAG(43) ,  COZLW   )
      EQUIVALENCE ( CDIAG(44) ,  CCLDRAS )
      EQUIVALENCE ( CDIAG(45) ,  CCLDTOT )
      EQUIVALENCE ( CDIAG(46) ,  CLWGDOWN)
      EQUIVALENCE ( CDIAG(47) ,  CGWDT   )
      EQUIVALENCE ( CDIAG(48) ,  CRADSWT )
      EQUIVALENCE ( CDIAG(49) ,  CTAUCLD )
      EQUIVALENCE ( CDIAG(50) ,  CTAUCLDC)
      EQUIVALENCE ( CDIAG(51) ,  CCLDLOW )
      EQUIVALENCE ( CDIAG(52) ,  CEVAP   )
      EQUIVALENCE ( CDIAG(53) ,  CDPDT   )
      EQUIVALENCE ( CDIAG(58) ,  COMEGA  )
      EQUIVALENCE ( CDIAG(59) ,  CDUDT   )
      EQUIVALENCE ( CDIAG(60) ,  CDVDT   )
      EQUIVALENCE ( CDIAG(61) ,  CDTDT   )
      EQUIVALENCE ( CDIAG(62) ,  CDQDT   )
      EQUIVALENCE ( CDIAG(63) ,  CVORT   )
      EQUIVALENCE ( CDIAG(65) ,  CDTLS   )
      EQUIVALENCE ( CDIAG(66) ,  CDQLS   )
      EQUIVALENCE ( CDIAG(67) ,  CUSTAR  )
      EQUIVALENCE ( CDIAG(68) ,  CZ0     )
      EQUIVALENCE ( CDIAG(69) ,  CFRQTRB )
      EQUIVALENCE ( CDIAG(70) ,  CPBL    )
      EQUIVALENCE ( CDIAG(71) ,  CSWCLR  )
      EQUIVALENCE ( CDIAG(72) ,  COSR    )
      EQUIVALENCE ( CDIAG(73) ,  COSRCLR )
      EQUIVALENCE ( CDIAG(74) ,  CCLDMAS )
      EQUIVALENCE ( CDIAG(75) ,  CUWND   )
      EQUIVALENCE ( CDIAG(76) ,  CVWND   )
      EQUIVALENCE ( CDIAG(77) ,  CTMPU   )
      EQUIVALENCE ( CDIAG(78) ,  CSPHU   )
      EQUIVALENCE ( CDIAG(79) ,  CRFT    )
      EQUIVALENCE ( CDIAG(80) ,  CPS     )
      EQUIVALENCE ( CDIAG(81) ,  CTKE    )
      EQUIVALENCE ( CDIAG(82) ,  CSWGCLR )
      EQUIVALENCE ( CDIAG(84) ,  CSDIAG1 )
      EQUIVALENCE ( CDIAG(85) ,  CSDIAG2 )
      EQUIVALENCE ( CDIAG(86) ,  CUDIAG1 )
      EQUIVALENCE ( CDIAG(87) ,  CUDIAG2 )
      EQUIVALENCE ( CDIAG(88) ,  CDIABU  )
      EQUIVALENCE ( CDIAG(89) ,  CDIABV  )
      EQUIVALENCE ( CDIAG(90) ,  CDIABT  )
      EQUIVALENCE ( CDIAG(91) ,  CDIABQ  )
      EQUIVALENCE ( CDIAG(92) ,  CRFU    )
      EQUIVALENCE ( CDIAG(93) ,  CRFV    )
      EQUIVALENCE ( CDIAG(94) ,  CGWDU   )
      EQUIVALENCE ( CDIAG(95) ,  CGWDV   )
      EQUIVALENCE ( CDIAG(96) ,  CGWDUS  )
      EQUIVALENCE ( CDIAG(97) ,  CGWDVS  )
      EQUIVALENCE ( CDIAG(98) ,  CGWDUT  )
      EQUIVALENCE ( CDIAG(99) ,  CGWDVT  )
      EQUIVALENCE ( CDIAG(100),  CLZRAD  )
      EQUIVALENCE ( CDIAG(101),  CSLP    )
      EQUIVALENCE ( CDIAG(102),  CVAVEUQ )
      EQUIVALENCE ( CDIAG(103),  CVAVEVQ )
      EQUIVALENCE ( CDIAG(104),  CVAVEUT )
      EQUIVALENCE ( CDIAG(105),  CVAVEVT )
      EQUIVALENCE ( CDIAG(106),  CCLDFRC )
      EQUIVALENCE ( CDIAG(107),  CTPW    )
      EQUIVALENCE ( CDIAG(108),  CU2M    )
      EQUIVALENCE ( CDIAG(109),  CV2M    )
      EQUIVALENCE ( CDIAG(110),  CT2M    )
      EQUIVALENCE ( CDIAG(111),  CQ2M    )
      EQUIVALENCE ( CDIAG(112),  CU10M   )
      EQUIVALENCE ( CDIAG(113),  CV10M   )
      EQUIVALENCE ( CDIAG(114),  CT10M   )
      EQUIVALENCE ( CDIAG(115),  CQ10M   )
      EQUIVALENCE ( CDIAG(116),  CDTRAIN )
      EQUIVALENCE ( CDIAG(117),  CQFILL  )
      EQUIVALENCE ( CDIAG(119),  CVAVEQFIL )
      EQUIVALENCE ( CDIAG(120),  CSHAPU  )
      EQUIVALENCE ( CDIAG(121),  CSHAPV  )
      EQUIVALENCE ( CDIAG(122),  CSHAPT  )
      EQUIVALENCE ( CDIAG(123),  CSHAPQ  )
      EQUIVALENCE ( CDIAG(124),  CSDIAG3 )
      EQUIVALENCE ( CDIAG(125),  CSDIAG4 )
      EQUIVALENCE ( CDIAG(126),  CSDIAG5 )
      EQUIVALENCE ( CDIAG(127),  CSDIAG6 )
      EQUIVALENCE ( CDIAG(128),  CSDIAG7 )
      EQUIVALENCE ( CDIAG(129),  CSDIAG8 )
      EQUIVALENCE ( CDIAG(130),  CSDIAG9 )
      EQUIVALENCE ( CDIAG(131),  CSDIAG10)
      EQUIVALENCE ( CDIAG(132),  CUDIAG3 )
      EQUIVALENCE ( CDIAG(133),  CUDIAG4 )
      EQUIVALENCE ( CDIAG(134),  CUDIAG5 )
      EQUIVALENCE ( CDIAG(135),  CUDIAG6 )
      EQUIVALENCE ( CDIAG(136),  CUDIAG7 )
      EQUIVALENCE ( CDIAG(137),  CUDIAG8 )
      EQUIVALENCE ( CDIAG(138),  CUDIAG9 )
      EQUIVALENCE ( CDIAG(139),  CUDIAG10)
      EQUIVALENCE ( CDIAG(140),  CCLDMID )
      EQUIVALENCE ( CDIAG(141),  CCLDHI  )
      EQUIVALENCE ( CDIAG(142),  CTAULOW )
      EQUIVALENCE ( CDIAG(143),  CTAUMID )
      EQUIVALENCE ( CDIAG(144),  CTAUHI  )
      EQUIVALENCE ( CDIAG(145),  CCLDNP  )
      EQUIVALENCE ( CDIAG(146),  CCLDPRS )
      EQUIVALENCE ( CDIAG(147),  CCTPCNT )
      EQUIVALENCE ( CDIAG(148),  CCLDTMP )
      EQUIVALENCE ( CDIAG(149),  CCTTCNT )
      EQUIVALENCE ( CDIAG(150),  CTAULOWC)
      EQUIVALENCE ( CDIAG(151),  CTAUMIDC)
      EQUIVALENCE ( CDIAG(152),  CTAUHIC )
      EQUIVALENCE ( CDIAG(153),  CTCANOPY  )
      EQUIVALENCE ( CDIAG(154),  CTDEEP    )
      EQUIVALENCE ( CDIAG(155),  CQCANOPY  )
      EQUIVALENCE ( CDIAG(156),  CSMSHAL   )
      EQUIVALENCE ( CDIAG(157),  CSMROOT   )
      EQUIVALENCE ( CDIAG(158),  CSMDEEP   )
      EQUIVALENCE ( CDIAG(159),  CCAPACITY )
      EQUIVALENCE ( CDIAG(160),  CSNOW     )
      EQUIVALENCE ( CDIAG(161),  CRAINCON  )
      EQUIVALENCE ( CDIAG(162),  CRAINLSP  )
      EQUIVALENCE ( CDIAG(163),  CSNOWFALL )
      EQUIVALENCE ( CDIAG(164),  CRUNOFF   )
      EQUIVALENCE ( CDIAG(165),  CFWSOIL   )
      EQUIVALENCE ( CDIAG(166),  CGDRAIN   )
      EQUIVALENCE ( CDIAG(167),  CSNOWMELT )
      EQUIVALENCE ( CDIAG(168),  CERESV    )
      EQUIVALENCE ( CDIAG(169),  CESOIL    )
      EQUIVALENCE ( CDIAG(170),  CEVEG     )
      EQUIVALENCE ( CDIAG(171),  CESNOW    )
      EQUIVALENCE ( CDIAG(172),  CPARDF    )
      EQUIVALENCE ( CDIAG(173),  CPARDR    )
      EQUIVALENCE ( CDIAG(174),  CLAI      )
      EQUIVALENCE ( CDIAG(175),  CGREEN    )
      EQUIVALENCE ( CDIAG(176),  CDLWDTC   )
      EQUIVALENCE ( CDIAG(177),  CDHDTC    )
      EQUIVALENCE ( CDIAG(178),  CDEDTC    )
      EQUIVALENCE ( CDIAG(179),  CVDTMOIST )
      EQUIVALENCE ( CDIAG(180),  CVDTTURB  )
      EQUIVALENCE ( CDIAG(181),  CVDTRADLW )
      EQUIVALENCE ( CDIAG(182),  CVDTRADSW )
      EQUIVALENCE ( CDIAG(184),  CPSUBCLD  )
      EQUIVALENCE ( CDIAG(185),  CPSUBCLDC )
      EQUIVALENCE ( CDIAG(186),  CLCL      )
      EQUIVALENCE ( CDIAG(187),  CSDIAGC   )
      EQUIVALENCE ( CDIAG(188),  CSDIAGCC  )

      EQUIVALENCE ( CDIAG(190),  CEVPOT    )
      EQUIVALENCE ( CDIAG(191),  CRHCHECK  )
      EQUIVALENCE ( CDIAG(192),  CDHDQA    )
      EQUIVALENCE ( CDIAG(193),  CDEDQA    )
      EQUIVALENCE ( CDIAG(194),  CDTC      )
      EQUIVALENCE ( CDIAG(195),  CDQC      )
      EQUIVALENCE ( CDIAG(196),  CTCDTC    )
      EQUIVALENCE ( CDIAG(197),  CRADDTC   )
      EQUIVALENCE ( CDIAG(198),  CSENSDTC  )
      EQUIVALENCE ( CDIAG(199),  CLATDTC   )
      EQUIVALENCE ( CDIAG(200),  CTDDTC    )
      EQUIVALENCE ( CDIAG(201),  CQCDTC    )
      EQUIVALENCE ( CDIAG(202),  CTRBQLIQ  )
      EQUIVALENCE ( CDIAG(203),  CTRBFCC   )
      EQUIVALENCE ( CDIAG(204),  CALBEDO   )
      EQUIVALENCE ( CDIAG(205),  CALBVISDR )
      EQUIVALENCE ( CDIAG(206),  CALBVISDF )
      EQUIVALENCE ( CDIAG(207),  CALBNIRDR )
      EQUIVALENCE ( CDIAG(208),  CALBNIRDF )
      EQUIVALENCE ( CDIAG(209),  CTAUAVE   )
      EQUIVALENCE ( CDIAG(210),  CALBEDOC  )
      EQUIVALENCE ( CDIAG(211),  CRHCHECKT )
      EQUIVALENCE ( CDIAG(212),  CRHCHECKQ )
      EQUIVALENCE ( CDIAG(213),  CCLDLSP   )
      EQUIVALENCE ( CDIAG(214),  CLZLOW    )
      EQUIVALENCE ( CDIAG(215),  CLZMID    )
      EQUIVALENCE ( CDIAG(216),  CLZHI     )
      EQUIVALENCE ( CDIAG(217),  CLZTOT    )
      EQUIVALENCE ( CDIAG(218),  CCAPE     )
      EQUIVALENCE ( CDIAG(219),  CHSUBCL   )
      EQUIVALENCE ( CDIAG(220),  CHSTAR    )
      EQUIVALENCE ( CDIAG(221),  CCONT     )
      EQUIVALENCE ( CDIAG(222),  CCONQ     )
      EQUIVALENCE ( CDIAG(223),  CLST      )
      EQUIVALENCE ( CDIAG(224),  CLSQ      )
      EQUIVALENCE ( CDIAG(225),  CCONEVPT  )
      EQUIVALENCE ( CDIAG(226),  CCONEVPQ  )
      EQUIVALENCE ( CDIAG(227),  CLSEVPT   )
      EQUIVALENCE ( CDIAG(228),  CLSEVPQ   )
      EQUIVALENCE ( CDIAG(229),  CGWCUMID  )
      EQUIVALENCE ( CDIAG(230),  CGWCVMID  )
      EQUIVALENCE ( CDIAG(231),  CCLDSTD   )
      EQUIVALENCE ( CDIAG(232),  CGWCUBAR  )
      EQUIVALENCE ( CDIAG(233),  CGWCVBAR  )
      EQUIVALENCE ( CDIAG(234),  CGWCUS    )
      EQUIVALENCE ( CDIAG(235),  CGWCVS    )
      EQUIVALENCE ( CDIAG(236),  CGWCU     )
      EQUIVALENCE ( CDIAG(237),  CGWCV     )

      integer       iUFLUX   , kUFLUX   , nUFLUX
      integer       iVFLUX   , kVFLUX   , nVFLUX
      integer       iHFLUX   , kHFLUX   , nHFLUX
      integer       iEFLUX   , kEFLUX   , nEFLUX
      integer       iQICE    , kQICE    , nQICE
      integer       iRADLWG  , kRADLWG  , nRADLWG
      integer       iRADSWG  , kRADSWG  , nRADSWG
      integer       iRI      , kRI      , nRI
      integer       iCT      , kCT      , nCT
      integer       iCU      , kCU      , nCU
      integer       iKH      , kKH      , nKH
      integer       iKM      , kKM      , nKM
      integer       iTURBU   , kTURBU   , nTURBU
      integer       iTURBV   , kTURBV   , nTURBV
      integer       iTURBT   , kTURBT   , nTURBT
      integer       iTURBQ   , kTURBQ   , nTURBQ
      integer       iMOISTT  , kMOISTT  , nMOISTT
      integer       iMOISTQ  , kMOISTQ  , nMOISTQ
      integer       iRADLW   , kRADLW   , nRADLW
      integer       iRADSW   , kRADSW   , nRADSW
      integer       iPREACC  , kPREACC  , nPREACC
      integer       iPRECON  , kPRECON  , nPRECON
      integer       iTUFLUX  , kTUFLUX  , nTUFLUX
      integer       iTVFLUX  , kTVFLUX  , nTVFLUX
      integer       iTTFLUX  , kTTFLUX  , nTTFLUX
      integer       iTQFLUX  , kTQFLUX  , nTQFLUX
      integer       iCN      , kCN      , nCN
      integer       iWINDS   , kWINDS   , nWINDS
      integer       iDTSRF   , kDTSRF   , nDTSRF
      integer       iTGROUND , kTGROUND , nTGROUND
      integer       iTS      , kTS      , nTS
      integer       iDTG     , kDTG     , nDTG
      integer       iQG      , kQG      , nQG
      integer       iQS      , kQS      , nQS
      integer       iTGRLW   , kTGRLW   , nTGRLW
      integer       iLWGUP   , kLWGUP   , nLWGUP
      integer       iOLR     , kOLR     , nOLR
      integer       iOLRCLR  , kOLRCLR  , nOLRCLR
      integer       iLWGCLR  , kLWGCLR  , nLWGCLR
      integer       iLWCLR   , kLWCLR   , nLWCLR
      integer       iTLW     , kTLW     , nTLW
      integer       iSHRAD   , kSHRAD   , nSHRAD
      integer       iOZLW    , kOZLW    , nOZLW
      integer       iCLDRAS  , kCLDRAS  , nCLDRAS
      integer       iCLDTOT  , kCLDTOT  , nCLDTOT
      integer       iLWGDOWN , kLWGDOWN , nLWGDOWN
      integer       iGWDT    , kGWDT    , nGWDT
      integer       iRADSWT  , kRADSWT  , nRADSWT
      integer       iTAUCLD  , kTAUCLD  , nTAUCLD
      integer       iTAUCLDC , kTAUCLDC , nTAUCLDC
      integer       iCLDLOW  , kCLDLOW  , nCLDLOW
      integer       iEVAP    , kEVAP    , nEVAP
      integer       iDPDT    , kDPDT    , nDPDT
      integer       iOMEGA   , kOMEGA   , nOMEGA
      integer       iDUDT    , kDUDT    , nDUDT
      integer       iDVDT    , kDVDT    , nDVDT
      integer       iDTDT    , kDTDT    , nDTDT
      integer       iDQDT    , kDQDT    , nDQDT
      integer       iVORT    , kVORT    , nVORT
      integer       iDTLS    , kDTLS    , nDTLS
      integer       iDQLS    , kDQLS    , nDQLS
      integer       iUSTAR   , kUSTAR   , nUSTAR
      integer       iZ0      , kZ0      , nZ0
      integer       iFRQTRB  , kFRQTRB  , nFRQTRB
      integer       iPBL     , kPBL     , nPBL
      integer       iSWCLR   , kSWCLR   , nSWCLR
      integer       iOSR     , kOSR     , nOSR
      integer       iOSRCLR  , kOSRCLR  , nOSRCLR
      integer       iCLDMAS  , kCLDMAS  , nCLDMAS
      integer       iUWND    , kUWND    , nUWND
      integer       iVWND    , kVWND    , nVWND
      integer       iTMPU    , kTMPU    , nTMPU
      integer       iSPHU    , kSPHU    , nSPHU
      integer       iRFT     , kRFT     , nRFT
      integer       iPS      , kPS      , nPS
      integer       iTKE     , kTKE     , nTKE
      integer       iSWGCLR  , kSWGCLR  , nSWGCLR
      integer       iSDIAG1  , kSDIAG1  , nSDIAG1
      integer       iSDIAG2  , kSDIAG2  , nSDIAG2
      integer       iUDIAG1  , kUDIAG1  , nUDIAG1
      integer       iUDIAG2  , kUDIAG2  , nUDIAG2
      integer       iDIABU   , kDIABU   , nDIABU
      integer       iDIABV   , kDIABV   , nDIABV
      integer       iDIABT   , kDIABT   , nDIABT
      integer       iDIABQ   , kDIABQ   , nDIABQ
      integer       iRFU     , kRFU     , nRFU
      integer       iRFV     , kRFV     , nRFV
      integer       iGWDU    , kGWDU    , nGWDU
      integer       iGWDV    , kGWDV    , nGWDV
      integer       iGWDUS   , kGWDUS   , nGWDUS
      integer       iGWDVS   , kGWDVS   , nGWDVS
      integer       iGWDUT   , kGWDUT   , nGWDUT
      integer       iGWDVT   , kGWDVT   , nGWDVT
      integer       iLZRAD   , kLZRAD   , nLZRAD
      integer       iTRBQLIQ , kTRBQLIQ , nTRBQLIQ
      integer       iTRBFCC  , kTRBFCC  , nTRBFCC
      integer       iSLP     , kSLP     , nSLP
      integer       iVAVEUQ  , kVAVEUQ  , nVAVEUQ
      integer       iVAVEVQ  , kVAVEVQ  , nVAVEVQ
      integer       iVAVEUT  , kVAVEUT  , nVAVEUT
      integer       iVAVEVT  , kVAVEVT  , nVAVEVT
      integer       iCLDFRC  , kCLDFRC  , nCLDFRC
      integer       iTPW     , kTPW     , nTPW
      integer       iU2M     , kU2M     , nU2M
      integer       iV2M     , kV2M     , nV2M
      integer       iT2M     , kT2M     , nT2M
      integer       iQ2M     , kQ2M     , nQ2M
      integer       iU10M    , kU10M    , nU10M
      integer       iV10M    , kV10M    , nV10M
      integer       iT10M    , kT10M    , nT10M
      integer       iQ10M    , kQ10M    , nQ10M
      integer       iDTRAIN  , kDTRAIN  , nDTRAIN
      integer       iQFILL   , kQFILL   , nQFILL
      integer       iVAVEQFIL, kVAVEQFIL, nVAVEQFIL
      integer       iSHAPU   , kSHAPU   , nSHAPU
      integer       iSHAPV   , kSHAPV   , nSHAPV
      integer       iSHAPT   , kSHAPT   , nSHAPT
      integer       iSHAPQ   , kSHAPQ   , nSHAPQ
      integer       iSDIAG3  , kSDIAG3  , nSDIAG3
      integer       iSDIAG4  , kSDIAG4  , nSDIAG4
      integer       iSDIAG5  , kSDIAG5  , nSDIAG5
      integer       iSDIAG6  , kSDIAG6  , nSDIAG6
      integer       iSDIAG7  , kSDIAG7  , nSDIAG7
      integer       iSDIAG8  , kSDIAG8  , nSDIAG8
      integer       iSDIAG9  , kSDIAG9  , nSDIAG9
      integer       iSDIAG10 , kSDIAG10 , nSDIAG10
      integer       iUDIAG3  , kUDIAG3  , nUDIAG3
      integer       iUDIAG4  , kUDIAG4  , nUDIAG4
      integer       iUDIAG5  , kUDIAG5  , nUDIAG5
      integer       iUDIAG6  , kUDIAG6  , nUDIAG6
      integer       iUDIAG7  , kUDIAG7  , nUDIAG7
      integer       iUDIAG8  , kUDIAG8  , nUDIAG8
      integer       iUDIAG9  , kUDIAG9  , nUDIAG9
      integer       iUDIAG10 , kUDIAG10 , nUDIAG10
      integer       iCLDMID  , kCLDMID  , nCLDMID
      integer       iCLDHI   , kCLDHI   , nCLDHI
      integer       iTAULOW  , kTAULOW  , nTAULOW
      integer       iTAUMID  , kTAUMID  , nTAUMID
      integer       iTAUHI   , kTAUHI   , nTAUHI
      integer       iCLDNP   , kCLDNP   , nCLDNP
      integer       iCLDPRS  , kCLDPRS  , nCLDPRS
      integer       iCTPCNT  , kCTPCNT  , nCTPCNT
      integer       iCLDTMP  , kCLDTMP  , nCLDTMP
      integer       iCTTCNT  , kCTTCNT  , nCTTCNT
      integer       iTAULOWC , kTAULOWC , nTAULOWC
      integer       iTAUMIDC , kTAUMIDC , nTAUMIDC
      integer       iTAUHIC  , kTAUHIC  , nTAUHIC
      integer       iTCANOPY , kTCANOPY , nTCANOPY
      integer       iTDEEP   , kTDEEP   , nTDEEP
      integer       iQCANOPY , kQCANOPY , nQCANOPY
      integer       iSMSHAL  , kSMSHAL  , nSMSHAL
      integer       iSMROOT  , kSMROOT  , nSMROOT
      integer       iSMDEEP  , kSMDEEP  , nSMDEEP
      integer       iCAPACITY, kCAPACITY, nCAPACITY
      integer       iSNOW    , kSNOW    , nSNOW
      integer       iRAINCON , kRAINCON , nRAINCON
      integer       iRAINLSP , kRAINLSP , nRAINLSP
      integer       iSNOWFALL, kSNOWFALL, nSNOWFALL
      integer       iRUNOFF  , kRUNOFF  , nRUNOFF
      integer       iFWSOIL  , kFWSOIL  , nFWSOIL
      integer       iGDRAIN  , kGDRAIN  , nGDRAIN
      integer       iSNOWMELT, kSNOWMELT, nSNOWMELT
      integer       iERESV   , kERESV   , nERESV
      integer       iESOIL   , kESOIL   , nESOIL
      integer       iEVEG    , kEVEG    , nEVEG
      integer       iESNOW   , kESNOW   , nESNOW
      integer       iPARDF   , kPARDF   , nPARDF
      integer       iPARDR   , kPARDR   , nPARDR
      integer       iLAI     , kLAI     , nLAI
      integer       iGREEN   , kGREEN   , nGREEN
      integer       iDLWDTC  , kDLWDTC  , nDLWDTC
      integer       iDHDTC   , kDHDTC   , nDHDTC
      integer       iDEDTC   , kDEDTC   , nDEDTC
      integer       iVDTMOIST, kVDTMOIST, nVDTMOIST
      integer       iVDTTURB , kVDTTURB , nVDTTURB
      integer       iVDTRADLW, kVDTRADLW, nVDTRADLW
      integer       iVDTRADSW, kVDTRADSW, nVDTRADSW
      integer       iPSUBCLD , kPSUBCLD , nPSUBCLD
      integer       iPSUBCLDC, kPSUBCLDC, nPSUBCLDC
      integer       iLCL     , kLCL     , nLCL
      integer       iSDIAGC  , kSDIAGC  , nSDIAGC
      integer       iSDIAGCC , kSDIAGCC , nSDIAGCC

      integer       iEVPOT   , kEVPOT   , nEVPOT
      integer       iRHCHECK , kRHCHECK , nRHCHECK
      integer       iDHDQA   , kDHDQA   , nDHDQA
      integer       iDEDQA   , kDEDQA   , nDEDQA
      integer       iDTC     , kDTC     , nDTC
      integer       iDQC     , kDQC     , nDQC
      integer       iTCDTC   , kTCDTC   , nTCDTC
      integer       iRADDTC  , kRADDTC  , nRADDTC
      integer       iSENSDTC , kSENSDTC , nSENSDTC
      integer       iLATDTC  , kLATDTC  , nLATDTC
      integer       iTDDTC   , kTDDTC   , nTDDTC
      integer       iQCDTC   , kQCDTC   , nQCDTC
      integer       iALBEDO  , kALBEDO  , nALBEDO
      integer       iALBVISDR, kALBVISDR, nALBVISDR
      integer       iALBVISDF, kALBVISDF, nALBVISDF
      integer       iALBNIRDR, kALBNIRDR, nALBNIRDR
      integer       iALBNIRDF, kALBNIRDF, nALBNIRDF
      integer       iTAUAVE  , kTAUAVE  , nTAUAVE
      integer       iALBEDOC , kALBEDOC , nALBEDOC
      integer       iRHCHECKT, kRHCHECKT, nRHCHECKT
      integer       iRHCHECKQ, kRHCHECKQ, nRHCHECKQ
      integer       iCLDLSP  , kCLDLSP  , nCLDLSP
      integer       iLZLOW   , kLZLOW   , nLZLOW
      integer       iLZMID   , kLZMID   , nLZMID
      integer       iLZHI    , kLZHI    , nLZHI
      integer       iLZTOT   , kLZTOT   , nLZTOT
      integer       iCAPE    , kCAPE    , nCAPE
      integer       iHSUBCL  , kHSUBCL  , nHSUBCL
      integer       iHSTAR   , kHSTAR   , nHSTAR
      integer       iCONT    , kCONT    , nCONT
      integer       iCONQ    , kCONQ    , nCONQ
      integer       iLST     , kLST     , nLST
      integer       iLSQ     , kLSQ     , nLSQ
      integer       iCONEVPT , kCONEVPT , nCONEVPT
      integer       iCONEVPQ , kCONEVPQ , nCONEVPQ
      integer       iLSEVPT  , kLSEVPT  , nLSEVPT
      integer       iLSEVPQ  , kLSEVPQ  , nLSEVPQ
      integer       iGWCUMID , kGWCUMID , nGWCUMID
      integer       iGWCVMID , kGWCVMID , nGWCVMID
      integer       iCLDSTD  , kCLDSTD  , nCLDSTD
      integer       iGWCUBAR , kGWCUBAR , nGWCUBAR
      integer       iGWCVBAR , kGWCVBAR , nGWCVBAR
      integer       iGWCUS   , kGWCUS   , nGWCUS
      integer       iGWCVS   , kGWCVS   , nGWCVS
      integer       iGWCU    , kGWCU    , nGWCU 
      integer       iGWCV    , kGWCV    , nGWCV 


C Diagnostic Pointers
C -------------------
      EQUIVALENCE ( IDIAG( 1) ,  IUFLUX  )
      EQUIVALENCE ( IDIAG( 2) ,  IVFLUX  )
      EQUIVALENCE ( IDIAG( 3) ,  IHFLUX  )
      EQUIVALENCE ( IDIAG( 4) ,  IEFLUX  )
      EQUIVALENCE ( IDIAG( 5) ,  IQICE   )
      EQUIVALENCE ( IDIAG( 6) ,  IRADLWG )
      EQUIVALENCE ( IDIAG( 7) ,  IRADSWG )
      EQUIVALENCE ( IDIAG( 8) ,  IRI     )
      EQUIVALENCE ( IDIAG( 9) ,  ICT     )
      EQUIVALENCE ( IDIAG(10) ,  ICU     )
      EQUIVALENCE ( IDIAG(11) ,  IKH     )
      EQUIVALENCE ( IDIAG(12) ,  IKM     )
      EQUIVALENCE ( IDIAG(13) ,  ITURBU  )
      EQUIVALENCE ( IDIAG(14) ,  ITURBV  )
      EQUIVALENCE ( IDIAG(15) ,  ITURBT  )
      EQUIVALENCE ( IDIAG(16) ,  ITURBQ  )
      EQUIVALENCE ( IDIAG(17) ,  IMOISTT )
      EQUIVALENCE ( IDIAG(18) ,  IMOISTQ )
      EQUIVALENCE ( IDIAG(19) ,  IRADLW  )
      EQUIVALENCE ( IDIAG(20) ,  IRADSW  )
      EQUIVALENCE ( IDIAG(21) ,  IPREACC )
      EQUIVALENCE ( IDIAG(22) ,  IPRECON )
      EQUIVALENCE ( IDIAG(23) ,  ITUFLUX )
      EQUIVALENCE ( IDIAG(24) ,  ITVFLUX )
      EQUIVALENCE ( IDIAG(25) ,  ITTFLUX )
      EQUIVALENCE ( IDIAG(26) ,  ITQFLUX )
      EQUIVALENCE ( IDIAG(27) ,  ICN     )
      EQUIVALENCE ( IDIAG(28) ,  IWINDS  )
      EQUIVALENCE ( IDIAG(29) ,  IDTSRF  )
      EQUIVALENCE ( IDIAG(30) ,  ITGROUND)
      EQUIVALENCE ( IDIAG(31) ,  ITS     )
      EQUIVALENCE ( IDIAG(32) ,  IDTG    )
      EQUIVALENCE ( IDIAG(33) ,  IQG     )
      EQUIVALENCE ( IDIAG(34) ,  IQS     )
      EQUIVALENCE ( IDIAG(35) ,  ITGRLW  )
      EQUIVALENCE ( IDIAG(36) ,  ILWGUP  )
      EQUIVALENCE ( IDIAG(37) ,  IOLR    )
      EQUIVALENCE ( IDIAG(38) ,  IOLRCLR )
      EQUIVALENCE ( IDIAG(39) ,  ILWGCLR )
      EQUIVALENCE ( IDIAG(40) ,  ILWCLR  )
      EQUIVALENCE ( IDIAG(41) ,  ITLW    )
      EQUIVALENCE ( IDIAG(42) ,  ISHRAD  )
      EQUIVALENCE ( IDIAG(43) ,  IOZLW   )
      EQUIVALENCE ( IDIAG(44) ,  ICLDRAS )
      EQUIVALENCE ( IDIAG(45) ,  ICLDTOT )
      EQUIVALENCE ( IDIAG(46) ,  ILWGDOWN)
      EQUIVALENCE ( IDIAG(47) ,  IGWDT   )
      EQUIVALENCE ( IDIAG(48) ,  IRADSWT )
      EQUIVALENCE ( IDIAG(49) ,  ITAUCLD )
      EQUIVALENCE ( IDIAG(50) ,  ITAUCLDC)
      EQUIVALENCE ( IDIAG(51) ,  ICLDLOW )
      EQUIVALENCE ( IDIAG(52) ,  IEVAP   )
      EQUIVALENCE ( IDIAG(53) ,  IDPDT   )
      EQUIVALENCE ( IDIAG(58) ,  IOMEGA  )
      EQUIVALENCE ( IDIAG(59) ,  IDUDT   )
      EQUIVALENCE ( IDIAG(60) ,  IDVDT   )
      EQUIVALENCE ( IDIAG(61) ,  IDTDT   )
      EQUIVALENCE ( IDIAG(62) ,  IDQDT   )
      EQUIVALENCE ( IDIAG(63) ,  IVORT   )
      EQUIVALENCE ( IDIAG(65) ,  IDTLS   )
      EQUIVALENCE ( IDIAG(66) ,  IDQLS   )
      EQUIVALENCE ( IDIAG(67) ,  IUSTAR  )
      EQUIVALENCE ( IDIAG(68) ,  IZ0     )
      EQUIVALENCE ( IDIAG(69) ,  IFRQTRB )
      EQUIVALENCE ( IDIAG(70) ,  IPBL    )
      EQUIVALENCE ( IDIAG(71) ,  ISWCLR  )
      EQUIVALENCE ( IDIAG(72) ,  IOSR    )
      EQUIVALENCE ( IDIAG(73) ,  IOSRCLR )
      EQUIVALENCE ( IDIAG(74) ,  ICLDMAS )
      EQUIVALENCE ( IDIAG(75) ,  IUWND   )
      EQUIVALENCE ( IDIAG(76) ,  IVWND   )
      EQUIVALENCE ( IDIAG(77) ,  ITMPU   )
      EQUIVALENCE ( IDIAG(78) ,  ISPHU   )
      EQUIVALENCE ( IDIAG(79) ,  IRFT    )
      EQUIVALENCE ( IDIAG(80) ,  IPS     )
      EQUIVALENCE ( IDIAG(81) ,  ITKE    )
      EQUIVALENCE ( IDIAG(82) ,  ISWGCLR )
      EQUIVALENCE ( IDIAG(84) ,  ISDIAG1 )
      EQUIVALENCE ( IDIAG(85) ,  ISDIAG2 )
      EQUIVALENCE ( IDIAG(86) ,  IUDIAG1 )
      EQUIVALENCE ( IDIAG(87) ,  IUDIAG2 )
      EQUIVALENCE ( IDIAG(88) ,  IDIABU  )
      EQUIVALENCE ( IDIAG(89) ,  IDIABV  )
      EQUIVALENCE ( IDIAG(90) ,  IDIABT  )
      EQUIVALENCE ( IDIAG(91) ,  IDIABQ  )
      EQUIVALENCE ( IDIAG(92) ,  IRFU    )
      EQUIVALENCE ( IDIAG(93) ,  IRFV    )
      EQUIVALENCE ( IDIAG(94) ,  IGWDU   )
      EQUIVALENCE ( IDIAG(95) ,  IGWDV   )
      EQUIVALENCE ( IDIAG(96) ,  IGWDUS  )
      EQUIVALENCE ( IDIAG(97) ,  IGWDVS  )
      EQUIVALENCE ( IDIAG(98) ,  IGWDUT  )
      EQUIVALENCE ( IDIAG(99) ,  IGWDVT  )
      EQUIVALENCE ( IDIAG(100),  ILZRAD  )
      EQUIVALENCE ( IDIAG(101),  ISLP    )
      EQUIVALENCE ( IDIAG(102),  IVAVEUQ )
      EQUIVALENCE ( IDIAG(103),  IVAVEVQ )
      EQUIVALENCE ( IDIAG(104),  IVAVEUT )
      EQUIVALENCE ( IDIAG(105),  IVAVEVT )
      EQUIVALENCE ( IDIAG(106),  ICLDFRC )
      EQUIVALENCE ( IDIAG(107),  ITPW    )
      EQUIVALENCE ( IDIAG(108),  IU2M    )
      EQUIVALENCE ( IDIAG(109),  IV2M    )
      EQUIVALENCE ( IDIAG(110),  IT2M    )
      EQUIVALENCE ( IDIAG(111),  IQ2M    )
      EQUIVALENCE ( IDIAG(112),  IU10M   )
      EQUIVALENCE ( IDIAG(113),  IV10M   )
      EQUIVALENCE ( IDIAG(114),  IT10M   )
      EQUIVALENCE ( IDIAG(115),  IQ10M   )
      EQUIVALENCE ( IDIAG(116),  IDTRAIN )
      EQUIVALENCE ( IDIAG(117),  IQFILL  )
      EQUIVALENCE ( IDIAG(119),  IVAVEQFIL )
      EQUIVALENCE ( IDIAG(120),  ISHAPU  )
      EQUIVALENCE ( IDIAG(121),  ISHAPV  )
      EQUIVALENCE ( IDIAG(122),  ISHAPT  )
      EQUIVALENCE ( IDIAG(123),  ISHAPQ  )
      EQUIVALENCE ( IDIAG(124),  ISDIAG3 )
      EQUIVALENCE ( IDIAG(125),  ISDIAG4 )
      EQUIVALENCE ( IDIAG(126),  ISDIAG5 )
      EQUIVALENCE ( IDIAG(127),  ISDIAG6 )
      EQUIVALENCE ( IDIAG(128),  ISDIAG7 )
      EQUIVALENCE ( IDIAG(129),  ISDIAG8 )
      EQUIVALENCE ( IDIAG(130),  ISDIAG9 )
      EQUIVALENCE ( IDIAG(131),  ISDIAG10)
      EQUIVALENCE ( IDIAG(132),  IUDIAG3 )
      EQUIVALENCE ( IDIAG(133),  IUDIAG4 )
      EQUIVALENCE ( IDIAG(134),  IUDIAG5 )
      EQUIVALENCE ( IDIAG(135),  IUDIAG6 )
      EQUIVALENCE ( IDIAG(136),  IUDIAG7 )
      EQUIVALENCE ( IDIAG(137),  IUDIAG8 )
      EQUIVALENCE ( IDIAG(138),  IUDIAG9 )
      EQUIVALENCE ( IDIAG(139),  IUDIAG10)
      EQUIVALENCE ( IDIAG(140),  ICLDMID )
      EQUIVALENCE ( IDIAG(141),  ICLDHI  )
      EQUIVALENCE ( IDIAG(142),  ITAULOW )
      EQUIVALENCE ( IDIAG(143),  ITAUMID )
      EQUIVALENCE ( IDIAG(144),  ITAUHI  )
      EQUIVALENCE ( IDIAG(145),  ICLDNP  )
      EQUIVALENCE ( IDIAG(146),  ICLDPRS )
      EQUIVALENCE ( IDIAG(147),  ICTPCNT )
      EQUIVALENCE ( IDIAG(148),  ICLDTMP )
      EQUIVALENCE ( IDIAG(149),  ICTTCNT )
      EQUIVALENCE ( IDIAG(150),  ITAULOWC)
      EQUIVALENCE ( IDIAG(151),  ITAUMIDC)
      EQUIVALENCE ( IDIAG(152),  ITAUHIC )
      EQUIVALENCE ( IDIAG(153),  ITCANOPY  )
      EQUIVALENCE ( IDIAG(154),  ITDEEP    )
      EQUIVALENCE ( IDIAG(155),  IQCANOPY  )
      EQUIVALENCE ( IDIAG(156),  ISMSHAL   )
      EQUIVALENCE ( IDIAG(157),  ISMROOT   )
      EQUIVALENCE ( IDIAG(158),  ISMDEEP   )
      EQUIVALENCE ( IDIAG(159),  ICAPACITY )
      EQUIVALENCE ( IDIAG(160),  ISNOW     )
      EQUIVALENCE ( IDIAG(161),  IRAINCON  )
      EQUIVALENCE ( IDIAG(162),  IRAINLSP  )
      EQUIVALENCE ( IDIAG(163),  ISNOWFALL )
      EQUIVALENCE ( IDIAG(164),  IRUNOFF   )
      EQUIVALENCE ( IDIAG(165),  IFWSOIL   )
      EQUIVALENCE ( IDIAG(166),  IGDRAIN   )
      EQUIVALENCE ( IDIAG(167),  ISNOWMELT )
      EQUIVALENCE ( IDIAG(168),  IERESV    )
      EQUIVALENCE ( IDIAG(169),  IESOIL    )
      EQUIVALENCE ( IDIAG(170),  IEVEG     )
      EQUIVALENCE ( IDIAG(171),  IESNOW    )
      EQUIVALENCE ( IDIAG(172),  IPARDF    )
      EQUIVALENCE ( IDIAG(173),  IPARDR    )
      EQUIVALENCE ( IDIAG(174),  ILAI      )
      EQUIVALENCE ( IDIAG(175),  IGREEN    )
      EQUIVALENCE ( IDIAG(176),  IDLWDTC   )
      EQUIVALENCE ( IDIAG(177),  IDHDTC    )
      EQUIVALENCE ( IDIAG(178),  IDEDTC    )
      EQUIVALENCE ( IDIAG(179),  IVDTMOIST )
      EQUIVALENCE ( IDIAG(180),  IVDTTURB  )
      EQUIVALENCE ( IDIAG(181),  IVDTRADLW )
      EQUIVALENCE ( IDIAG(182),  IVDTRADSW )
      EQUIVALENCE ( IDIAG(184),  IPSUBCLD  )
      EQUIVALENCE ( IDIAG(185),  IPSUBCLDC )
      EQUIVALENCE ( IDIAG(186),  ILCL      )
      EQUIVALENCE ( IDIAG(187),  ISDIAGC   )
      EQUIVALENCE ( IDIAG(188),  ISDIAGCC  )

      EQUIVALENCE ( IDIAG(190),  IEVPOT    )
      EQUIVALENCE ( IDIAG(191),  IRHCHECK  )
      EQUIVALENCE ( IDIAG(192),  IDHDQA    )
      EQUIVALENCE ( IDIAG(193),  IDEDQA    )
      EQUIVALENCE ( IDIAG(194),  IDTC      )
      EQUIVALENCE ( IDIAG(195),  IDQC      )
      EQUIVALENCE ( IDIAG(196),  ITCDTC    )
      EQUIVALENCE ( IDIAG(197),  IRADDTC   )
      EQUIVALENCE ( IDIAG(198),  ISENSDTC  )
      EQUIVALENCE ( IDIAG(199),  ILATDTC   )
      EQUIVALENCE ( IDIAG(200),  ITDDTC    )
      EQUIVALENCE ( IDIAG(201),  IQCDTC    )
      EQUIVALENCE ( IDIAG(202),  ITRBQLIQ  )
      EQUIVALENCE ( IDIAG(203),  ITRBFCC   )
      EQUIVALENCE ( IDIAG(204),  IALBEDO   )
      EQUIVALENCE ( IDIAG(205),  IALBVISDR )
      EQUIVALENCE ( IDIAG(206),  IALBVISDF )
      EQUIVALENCE ( IDIAG(207),  IALBNIRDR )
      EQUIVALENCE ( IDIAG(208),  IALBNIRDF )
      EQUIVALENCE ( IDIAG(209),  ITAUAVE   )
      EQUIVALENCE ( IDIAG(210),  IALBEDOC  )
      EQUIVALENCE ( IDIAG(211),  IRHCHECKT )
      EQUIVALENCE ( IDIAG(212),  IRHCHECKQ )
      EQUIVALENCE ( IDIAG(213),  ICLDLSP   )
      EQUIVALENCE ( IDIAG(214),  ILZLOW    )
      EQUIVALENCE ( IDIAG(215),  ILZMID    )
      EQUIVALENCE ( IDIAG(216),  ILZHI     )
      EQUIVALENCE ( IDIAG(217),  ILZTOT    )
      EQUIVALENCE ( IDIAG(218),  ICAPE     )
      EQUIVALENCE ( IDIAG(219),  IHSUBCL   )
      EQUIVALENCE ( IDIAG(220),  IHSTAR    )
      EQUIVALENCE ( IDIAG(221),  ICONT     )
      EQUIVALENCE ( IDIAG(222),  ICONQ     )
      EQUIVALENCE ( IDIAG(223),  ILST      )
      EQUIVALENCE ( IDIAG(224),  ILSQ      )
      EQUIVALENCE ( IDIAG(225),  ICONEVPT  )
      EQUIVALENCE ( IDIAG(226),  ICONEVPQ  )
      EQUIVALENCE ( IDIAG(227),  ILSEVPT   )
      EQUIVALENCE ( IDIAG(228),  ILSEVPQ   )
      EQUIVALENCE ( IDIAG(229),  IGWCUMID  )
      EQUIVALENCE ( IDIAG(230),  IGWCVMID  )
      EQUIVALENCE ( IDIAG(231),  ICLDSTD   )
      EQUIVALENCE ( IDIAG(232),  IGWCUBAR  )
      EQUIVALENCE ( IDIAG(233),  IGWCVBAR  )
      EQUIVALENCE ( IDIAG(234),  IGWCUS    )
      EQUIVALENCE ( IDIAG(235),  IGWCVS    )
      EQUIVALENCE ( IDIAG(236),  IGWCU     )
      EQUIVALENCE ( IDIAG(237),  IGWCV     )


C Diagnostic Levels
C -----------------
      EQUIVALENCE ( KDIAG( 1) ,  KUFLUX  )
      EQUIVALENCE ( KDIAG( 2) ,  KVFLUX  )
      EQUIVALENCE ( KDIAG( 3) ,  KHFLUX  )
      EQUIVALENCE ( KDIAG( 4) ,  KEFLUX  )
      EQUIVALENCE ( KDIAG( 5) ,  KQICE   )
      EQUIVALENCE ( KDIAG( 6) ,  KRADLWG )
      EQUIVALENCE ( KDIAG( 7) ,  KRADSWG )
      EQUIVALENCE ( KDIAG( 8) ,  KRI     )
      EQUIVALENCE ( KDIAG( 9) ,  KCT     )
      EQUIVALENCE ( KDIAG(10) ,  KCU     )
      EQUIVALENCE ( KDIAG(11) ,  KKH     )
      EQUIVALENCE ( KDIAG(12) ,  KKM     )
      EQUIVALENCE ( KDIAG(13) ,  KTURBU  )
      EQUIVALENCE ( KDIAG(14) ,  KTURBV  )
      EQUIVALENCE ( KDIAG(15) ,  KTURBT  )
      EQUIVALENCE ( KDIAG(16) ,  KTURBQ  )
      EQUIVALENCE ( KDIAG(17) ,  KMOISTT )
      EQUIVALENCE ( KDIAG(18) ,  KMOISTQ )
      EQUIVALENCE ( KDIAG(19) ,  KRADLW  )
      EQUIVALENCE ( KDIAG(20) ,  KRADSW  )
      EQUIVALENCE ( KDIAG(21) ,  KPREACC )
      EQUIVALENCE ( KDIAG(22) ,  KPRECON )
      EQUIVALENCE ( KDIAG(23) ,  KTUFLUX )
      EQUIVALENCE ( KDIAG(24) ,  KTVFLUX )
      EQUIVALENCE ( KDIAG(25) ,  KTTFLUX )
      EQUIVALENCE ( KDIAG(26) ,  KTQFLUX )
      EQUIVALENCE ( KDIAG(27) ,  KCN     )
      EQUIVALENCE ( KDIAG(28) ,  KWINDS  )
      EQUIVALENCE ( KDIAG(29) ,  KDTSRF  )
      EQUIVALENCE ( KDIAG(30) ,  KTGROUND)
      EQUIVALENCE ( KDIAG(31) ,  KTS     )
      EQUIVALENCE ( KDIAG(32) ,  KDTG    )
      EQUIVALENCE ( KDIAG(33) ,  KQG     )
      EQUIVALENCE ( KDIAG(34) ,  KQS     )
      EQUIVALENCE ( KDIAG(35) ,  KTGRLW  )
      EQUIVALENCE ( KDIAG(36) ,  KLWGUP  )
      EQUIVALENCE ( KDIAG(37) ,  KOLR    )
      EQUIVALENCE ( KDIAG(38) ,  KOLRCLR )
      EQUIVALENCE ( KDIAG(39) ,  KLWGCLR )
      EQUIVALENCE ( KDIAG(40) ,  KLWCLR  )
      EQUIVALENCE ( KDIAG(41) ,  KTLW    )
      EQUIVALENCE ( KDIAG(42) ,  KSHRAD  )
      EQUIVALENCE ( KDIAG(43) ,  KOZLW   )
      EQUIVALENCE ( KDIAG(44) ,  KCLDRAS )
      EQUIVALENCE ( KDIAG(45) ,  KCLDTOT )
      EQUIVALENCE ( KDIAG(46) ,  KLWGDOWN)
      EQUIVALENCE ( KDIAG(47) ,  KGWDT   )
      EQUIVALENCE ( KDIAG(48) ,  KRADSWT )
      EQUIVALENCE ( KDIAG(49) ,  KTAUCLD )
      EQUIVALENCE ( KDIAG(50) ,  KTAUCLDC)
      EQUIVALENCE ( KDIAG(51) ,  KCLDLOW )
      EQUIVALENCE ( KDIAG(52) ,  KEVAP   )
      EQUIVALENCE ( KDIAG(53) ,  KDPDT   )
      EQUIVALENCE ( KDIAG(58) ,  KOMEGA  )
      EQUIVALENCE ( KDIAG(59) ,  KDUDT   )
      EQUIVALENCE ( KDIAG(60) ,  KDVDT   )
      EQUIVALENCE ( KDIAG(61) ,  KDTDT   )
      EQUIVALENCE ( KDIAG(62) ,  KDQDT   )
      EQUIVALENCE ( KDIAG(63) ,  KVORT   )
      EQUIVALENCE ( KDIAG(65) ,  KDTLS   )
      EQUIVALENCE ( KDIAG(66) ,  KDQLS   )
      EQUIVALENCE ( KDIAG(67) ,  KUSTAR  )
      EQUIVALENCE ( KDIAG(68) ,  KZ0     )
      EQUIVALENCE ( KDIAG(69) ,  KFRQTRB )
      EQUIVALENCE ( KDIAG(70) ,  KPBL    )
      EQUIVALENCE ( KDIAG(71) ,  KSWCLR  )
      EQUIVALENCE ( KDIAG(72) ,  KOSR    )
      EQUIVALENCE ( KDIAG(73) ,  KOSRCLR )
      EQUIVALENCE ( KDIAG(74) ,  KCLDMAS )
      EQUIVALENCE ( KDIAG(75) ,  KUWND   )
      EQUIVALENCE ( KDIAG(76) ,  KVWND   )
      EQUIVALENCE ( KDIAG(77) ,  KTMPU   )
      EQUIVALENCE ( KDIAG(78) ,  KSPHU   )
      EQUIVALENCE ( KDIAG(79) ,  KRFT    )
      EQUIVALENCE ( KDIAG(80) ,  KPS     )
      EQUIVALENCE ( KDIAG(81) ,  KTKE    )
      EQUIVALENCE ( KDIAG(82) ,  KSWGCLR )
      EQUIVALENCE ( KDIAG(84) ,  KSDIAG1 )
      EQUIVALENCE ( KDIAG(85) ,  KSDIAG2 )
      EQUIVALENCE ( KDIAG(86) ,  KUDIAG1 )
      EQUIVALENCE ( KDIAG(87) ,  KUDIAG2 )
      EQUIVALENCE ( KDIAG(88) ,  KDIABU  )
      EQUIVALENCE ( KDIAG(89) ,  KDIABV  )
      EQUIVALENCE ( KDIAG(90) ,  KDIABT  )
      EQUIVALENCE ( KDIAG(91) ,  KDIABQ  )
      EQUIVALENCE ( KDIAG(92) ,  KRFU    )
      EQUIVALENCE ( KDIAG(93) ,  KRFV    )
      EQUIVALENCE ( KDIAG(94) ,  KGWDU   )
      EQUIVALENCE ( KDIAG(95) ,  KGWDV   )
      EQUIVALENCE ( KDIAG(96) ,  KGWDUS  )
      EQUIVALENCE ( KDIAG(97) ,  KGWDVS  )
      EQUIVALENCE ( KDIAG(98) ,  KGWDUT  )
      EQUIVALENCE ( KDIAG(99) ,  KGWDVT  )
      EQUIVALENCE ( KDIAG(100),  KLZRAD  )
      EQUIVALENCE ( KDIAG(101),  KSLP    )
      EQUIVALENCE ( KDIAG(102),  KVAVEUQ )
      EQUIVALENCE ( KDIAG(103),  KVAVEVQ )
      EQUIVALENCE ( KDIAG(104),  KVAVEUT )
      EQUIVALENCE ( KDIAG(105),  KVAVEVT )
      EQUIVALENCE ( KDIAG(106),  KCLDFRC )
      EQUIVALENCE ( KDIAG(107),  KTPW    )
      EQUIVALENCE ( KDIAG(108),  KU2M    )
      EQUIVALENCE ( KDIAG(109),  KV2M    )
      EQUIVALENCE ( KDIAG(110),  KT2M    )
      EQUIVALENCE ( KDIAG(111),  KQ2M    )
      EQUIVALENCE ( KDIAG(112),  KU10M   )
      EQUIVALENCE ( KDIAG(113),  KV10M   )
      EQUIVALENCE ( KDIAG(114),  KT10M   )
      EQUIVALENCE ( KDIAG(115),  KQ10M   )
      EQUIVALENCE ( KDIAG(116),  KDTRAIN )
      EQUIVALENCE ( KDIAG(117),  KQFILL  )
      EQUIVALENCE ( KDIAG(119),  KVAVEQFIL )
      EQUIVALENCE ( KDIAG(120),  KSHAPU  )
      EQUIVALENCE ( KDIAG(121),  KSHAPV  )
      EQUIVALENCE ( KDIAG(122),  KSHAPT  )
      EQUIVALENCE ( KDIAG(123),  KSHAPQ  )
      EQUIVALENCE ( KDIAG(124),  KSDIAG3 )
      EQUIVALENCE ( KDIAG(125),  KSDIAG4 )
      EQUIVALENCE ( KDIAG(126),  KSDIAG5 )
      EQUIVALENCE ( KDIAG(127),  KSDIAG6 )
      EQUIVALENCE ( KDIAG(128),  KSDIAG7 )
      EQUIVALENCE ( KDIAG(129),  KSDIAG8 )
      EQUIVALENCE ( KDIAG(130),  KSDIAG9 )
      EQUIVALENCE ( KDIAG(131),  KSDIAG10)
      EQUIVALENCE ( KDIAG(132),  KUDIAG3 )
      EQUIVALENCE ( KDIAG(133),  KUDIAG4 )
      EQUIVALENCE ( KDIAG(134),  KUDIAG5 )
      EQUIVALENCE ( KDIAG(135),  KUDIAG6 )
      EQUIVALENCE ( KDIAG(136),  KUDIAG7 )
      EQUIVALENCE ( KDIAG(137),  KUDIAG8 )
      EQUIVALENCE ( KDIAG(138),  KUDIAG9 )
      EQUIVALENCE ( KDIAG(139),  KUDIAG10)
      EQUIVALENCE ( KDIAG(140),  KCLDMID )
      EQUIVALENCE ( KDIAG(141),  KCLDHI  )
      EQUIVALENCE ( KDIAG(142),  KTAULOW )
      EQUIVALENCE ( KDIAG(143),  KTAUMID )
      EQUIVALENCE ( KDIAG(144),  KTAUHI  )
      EQUIVALENCE ( KDIAG(145),  KCLDNP  )
      EQUIVALENCE ( KDIAG(146),  KCLDPRS )
      EQUIVALENCE ( KDIAG(147),  KCTPCNT )
      EQUIVALENCE ( KDIAG(148),  KCLDTMP )
      EQUIVALENCE ( KDIAG(149),  KCTTCNT )
      EQUIVALENCE ( KDIAG(150),  KTAULOWC)
      EQUIVALENCE ( KDIAG(151),  KTAUMIDC)
      EQUIVALENCE ( KDIAG(152),  KTAUHIC )
      EQUIVALENCE ( KDIAG(153),  KTCANOPY  )
      EQUIVALENCE ( KDIAG(154),  KTDEEP    )
      EQUIVALENCE ( KDIAG(155),  KQCANOPY  )
      EQUIVALENCE ( KDIAG(156),  KSMSHAL   )
      EQUIVALENCE ( KDIAG(157),  KSMROOT   )
      EQUIVALENCE ( KDIAG(158),  KSMDEEP   )
      EQUIVALENCE ( KDIAG(159),  KCAPACITY )
      EQUIVALENCE ( KDIAG(160),  KSNOW     )
      EQUIVALENCE ( KDIAG(161),  KRAINCON  )
      EQUIVALENCE ( KDIAG(162),  KRAINLSP  )
      EQUIVALENCE ( KDIAG(163),  KSNOWFALL )
      EQUIVALENCE ( KDIAG(164),  KRUNOFF   )
      EQUIVALENCE ( KDIAG(165),  KFWSOIL   )
      EQUIVALENCE ( KDIAG(166),  KGDRAIN   )
      EQUIVALENCE ( KDIAG(167),  KSNOWMELT )
      EQUIVALENCE ( KDIAG(168),  KERESV    )
      EQUIVALENCE ( KDIAG(169),  KESOIL    )
      EQUIVALENCE ( KDIAG(170),  KEVEG     )
      EQUIVALENCE ( KDIAG(171),  KESNOW    )
      EQUIVALENCE ( KDIAG(172),  KPARDF    )
      EQUIVALENCE ( KDIAG(173),  KPARDR    )
      EQUIVALENCE ( KDIAG(174),  KLAI      )
      EQUIVALENCE ( KDIAG(175),  KGREEN    )
      EQUIVALENCE ( KDIAG(176),  KDLWDTC   )
      EQUIVALENCE ( KDIAG(177),  KDHDTC    )
      EQUIVALENCE ( KDIAG(178),  KDEDTC    )
      EQUIVALENCE ( KDIAG(179),  KVDTMOIST )
      EQUIVALENCE ( KDIAG(180),  KVDTTURB  )
      EQUIVALENCE ( KDIAG(181),  KVDTRADLW )
      EQUIVALENCE ( KDIAG(182),  KVDTRADSW )
      EQUIVALENCE ( KDIAG(184),  KPSUBCLD  )
      EQUIVALENCE ( KDIAG(185),  KPSUBCLDC )
      EQUIVALENCE ( KDIAG(186),  KLCL      )
      EQUIVALENCE ( KDIAG(187),  KSDIAGC   )
      EQUIVALENCE ( KDIAG(188),  KSDIAGCC  )

      EQUIVALENCE ( KDIAG(190),  KEVPOT    )
      EQUIVALENCE ( KDIAG(191),  KRHCHECK  )
      EQUIVALENCE ( KDIAG(192),  KDHDQA    )
      EQUIVALENCE ( KDIAG(193),  KDEDQA    )
      EQUIVALENCE ( KDIAG(194),  KDTC      )
      EQUIVALENCE ( KDIAG(195),  KDQC      )
      EQUIVALENCE ( KDIAG(196),  KTCDTC    )
      EQUIVALENCE ( KDIAG(197),  KRADDTC   )
      EQUIVALENCE ( KDIAG(198),  KSENSDTC  )
      EQUIVALENCE ( KDIAG(199),  KLATDTC   )
      EQUIVALENCE ( KDIAG(200),  KTDDTC    )
      EQUIVALENCE ( KDIAG(201),  KQCDTC    )
      EQUIVALENCE ( KDIAG(202),  KTRBQLIQ  )
      EQUIVALENCE ( KDIAG(203),  KTRBFCC   )
      EQUIVALENCE ( KDIAG(204),  KALBEDO   )
      EQUIVALENCE ( KDIAG(205),  KALBVISDR )
      EQUIVALENCE ( KDIAG(206),  KALBVISDF )
      EQUIVALENCE ( KDIAG(207),  KALBNIRDR )
      EQUIVALENCE ( KDIAG(208),  KALBNIRDF )
      EQUIVALENCE ( KDIAG(209),  KTAUAVE   )
      EQUIVALENCE ( KDIAG(210),  KALBEDOC  )
      EQUIVALENCE ( KDIAG(211),  KRHCHECKT )
      EQUIVALENCE ( KDIAG(212),  KRHCHECKQ )
      EQUIVALENCE ( KDIAG(213),  KCLDLSP   )
      EQUIVALENCE ( KDIAG(214),  KLZLOW    )
      EQUIVALENCE ( KDIAG(215),  KLZMID    )
      EQUIVALENCE ( KDIAG(216),  KLZHI     )
      EQUIVALENCE ( KDIAG(217),  KLZTOT    )
      EQUIVALENCE ( KDIAG(218),  KCAPE     )
      EQUIVALENCE ( KDIAG(219),  KHSUBCL   )
      EQUIVALENCE ( KDIAG(220),  KHSTAR    )
      EQUIVALENCE ( KDIAG(221),  KCONT     )
      EQUIVALENCE ( KDIAG(222),  KCONQ     )
      EQUIVALENCE ( KDIAG(223),  KLST      )
      EQUIVALENCE ( KDIAG(224),  KLSQ      )
      EQUIVALENCE ( KDIAG(225),  KCONEVPT  )
      EQUIVALENCE ( KDIAG(226),  KCONEVPQ  )
      EQUIVALENCE ( KDIAG(227),  KLSEVPT   )
      EQUIVALENCE ( KDIAG(228),  KLSEVPQ   )
      EQUIVALENCE ( KDIAG(229),  KGWCUMID  )
      EQUIVALENCE ( KDIAG(230),  KGWCVMID  )
      EQUIVALENCE ( KDIAG(231),  KCLDSTD   )
      EQUIVALENCE ( KDIAG(232),  KGWCUBAR  )
      EQUIVALENCE ( KDIAG(233),  KGWCVBAR  )
      EQUIVALENCE ( KDIAG(234),  KGWCUS    )
      EQUIVALENCE ( KDIAG(235),  KGWCVS    )
      EQUIVALENCE ( KDIAG(236),  KGWCU     )
      EQUIVALENCE ( KDIAG(237),  KGWCV     )


C Diagnostic Counters
C -------------------
      EQUIVALENCE ( NDIAG( 1) ,  NUFLUX  )
      EQUIVALENCE ( NDIAG( 2) ,  NVFLUX  )
      EQUIVALENCE ( NDIAG( 3) ,  NHFLUX  )
      EQUIVALENCE ( NDIAG( 4) ,  NEFLUX  )
      EQUIVALENCE ( NDIAG( 5) ,  NQICE   )
      EQUIVALENCE ( NDIAG( 6) ,  NRADLWG )
      EQUIVALENCE ( NDIAG( 7) ,  NRADSWG )
      EQUIVALENCE ( NDIAG( 8) ,  NRI     )
      EQUIVALENCE ( NDIAG( 9) ,  NCT     )
      EQUIVALENCE ( NDIAG(10) ,  NCU     )
      EQUIVALENCE ( NDIAG(11) ,  NKH     )
      EQUIVALENCE ( NDIAG(12) ,  NKM     )
      EQUIVALENCE ( NDIAG(13) ,  NTURBU  )
      EQUIVALENCE ( NDIAG(14) ,  NTURBV  )
      EQUIVALENCE ( NDIAG(15) ,  NTURBT  )
      EQUIVALENCE ( NDIAG(16) ,  NTURBQ  )
      EQUIVALENCE ( NDIAG(17) ,  NMOISTT )
      EQUIVALENCE ( NDIAG(18) ,  NMOISTQ )
      EQUIVALENCE ( NDIAG(19) ,  NRADLW  )
      EQUIVALENCE ( NDIAG(20) ,  NRADSW  )
      EQUIVALENCE ( NDIAG(21) ,  NPREACC )
      EQUIVALENCE ( NDIAG(22) ,  NPRECON )
      EQUIVALENCE ( NDIAG(23) ,  NTUFLUX )
      EQUIVALENCE ( NDIAG(24) ,  NTVFLUX )
      EQUIVALENCE ( NDIAG(25) ,  NTTFLUX )
      EQUIVALENCE ( NDIAG(26) ,  NTQFLUX )
      EQUIVALENCE ( NDIAG(27) ,  NCN     )
      EQUIVALENCE ( NDIAG(28) ,  NWINDS  )
      EQUIVALENCE ( NDIAG(29) ,  NDTSRF  )
      EQUIVALENCE ( NDIAG(30) ,  NTGROUND)
      EQUIVALENCE ( NDIAG(31) ,  NTS     )
      EQUIVALENCE ( NDIAG(32) ,  NDTG    )
      EQUIVALENCE ( NDIAG(33) ,  NQG     )
      EQUIVALENCE ( NDIAG(34) ,  NQS     )
      EQUIVALENCE ( NDIAG(35) ,  NTGRLW  )
      EQUIVALENCE ( NDIAG(36) ,  NLWGUP  )
      EQUIVALENCE ( NDIAG(37) ,  NOLR    )
      EQUIVALENCE ( NDIAG(38) ,  NOLRCLR )
      EQUIVALENCE ( NDIAG(39) ,  NLWGCLR )
      EQUIVALENCE ( NDIAG(40) ,  NLWCLR  )
      EQUIVALENCE ( NDIAG(41) ,  NTLW    )
      EQUIVALENCE ( NDIAG(42) ,  NSHRAD  )
      EQUIVALENCE ( NDIAG(43) ,  NOZLW   )
      EQUIVALENCE ( NDIAG(44) ,  NCLDRAS )
      EQUIVALENCE ( NDIAG(45) ,  NCLDTOT )
      EQUIVALENCE ( NDIAG(46) ,  NLWGDOWN)
      EQUIVALENCE ( NDIAG(47) ,  NGWDT   )
      EQUIVALENCE ( NDIAG(48) ,  NRADSWT )
      EQUIVALENCE ( NDIAG(49) ,  NTAUCLD )
      EQUIVALENCE ( NDIAG(50) ,  NTAUCLDC)
      EQUIVALENCE ( NDIAG(51) ,  NCLDLOW )
      EQUIVALENCE ( NDIAG(52) ,  NEVAP   )
      EQUIVALENCE ( NDIAG(53) ,  NDPDT   )
      EQUIVALENCE ( NDIAG(58) ,  NOMEGA  )
      EQUIVALENCE ( NDIAG(59) ,  NDUDT   )
      EQUIVALENCE ( NDIAG(60) ,  NDVDT   )
      EQUIVALENCE ( NDIAG(61) ,  NDTDT   )
      EQUIVALENCE ( NDIAG(62) ,  NDQDT   )
      EQUIVALENCE ( NDIAG(63) ,  NVORT   )
      EQUIVALENCE ( NDIAG(65) ,  NDTLS   )
      EQUIVALENCE ( NDIAG(66) ,  NDQLS   )
      EQUIVALENCE ( NDIAG(67) ,  NUSTAR  )
      EQUIVALENCE ( NDIAG(68) ,  NZ0     )
      EQUIVALENCE ( NDIAG(69) ,  NFRQTRB )
      EQUIVALENCE ( NDIAG(70) ,  NPBL    )
      EQUIVALENCE ( NDIAG(71) ,  NSWCLR  )
      EQUIVALENCE ( NDIAG(72) ,  NOSR    )
      EQUIVALENCE ( NDIAG(73) ,  NOSRCLR )
      EQUIVALENCE ( NDIAG(74) ,  NCLDMAS )
      EQUIVALENCE ( NDIAG(75) ,  NUWND   )
      EQUIVALENCE ( NDIAG(76) ,  NVWND   )
      EQUIVALENCE ( NDIAG(77) ,  NTMPU   )
      EQUIVALENCE ( NDIAG(78) ,  NSPHU   )
      EQUIVALENCE ( NDIAG(79) ,  NRFT    )
      EQUIVALENCE ( NDIAG(80) ,  NPS     )
      EQUIVALENCE ( NDIAG(81) ,  NTKE    )
      EQUIVALENCE ( NDIAG(82) ,  NSWGCLR )
      EQUIVALENCE ( NDIAG(84) ,  NSDIAG1 )
      EQUIVALENCE ( NDIAG(85) ,  NSDIAG2 )
      EQUIVALENCE ( NDIAG(86) ,  NUDIAG1 )
      EQUIVALENCE ( NDIAG(87) ,  NUDIAG2 )
      EQUIVALENCE ( NDIAG(88) ,  NDIABU  )
      EQUIVALENCE ( NDIAG(89) ,  NDIABV  )
      EQUIVALENCE ( NDIAG(90) ,  NDIABT  )
      EQUIVALENCE ( NDIAG(91) ,  NDIABQ  )
      EQUIVALENCE ( NDIAG(92) ,  NRFU    )
      EQUIVALENCE ( NDIAG(93) ,  NRFV    )
      EQUIVALENCE ( NDIAG(94) ,  NGWDU   )
      EQUIVALENCE ( NDIAG(95) ,  NGWDV   )
      EQUIVALENCE ( NDIAG(96) ,  NGWDUS  )
      EQUIVALENCE ( NDIAG(97) ,  NGWDVS  )
      EQUIVALENCE ( NDIAG(98) ,  NGWDUT  )
      EQUIVALENCE ( NDIAG(99) ,  NGWDVT  )
      EQUIVALENCE ( NDIAG(100),  NLZRAD  )
      EQUIVALENCE ( NDIAG(101),  NSLP    )
      EQUIVALENCE ( NDIAG(102),  NVAVEUQ )
      EQUIVALENCE ( NDIAG(103),  NVAVEVQ )
      EQUIVALENCE ( NDIAG(104),  NVAVEUT )
      EQUIVALENCE ( NDIAG(105),  NVAVEVT )
      EQUIVALENCE ( NDIAG(106),  NCLDFRC )
      EQUIVALENCE ( NDIAG(107),  NTPW    )
      EQUIVALENCE ( NDIAG(108),  NU2M    )
      EQUIVALENCE ( NDIAG(109),  NV2M    )
      EQUIVALENCE ( NDIAG(110),  NT2M    )
      EQUIVALENCE ( NDIAG(111),  NQ2M    )
      EQUIVALENCE ( NDIAG(112),  NU10M   )
      EQUIVALENCE ( NDIAG(113),  NV10M   )
      EQUIVALENCE ( NDIAG(114),  NT10M   )
      EQUIVALENCE ( NDIAG(115),  NQ10M   )
      EQUIVALENCE ( NDIAG(116),  NDTRAIN )
      EQUIVALENCE ( NDIAG(117),  NQFILL  )
      EQUIVALENCE ( NDIAG(119),  NVAVEQFIL )
      EQUIVALENCE ( NDIAG(120),  NSHAPU  )
      EQUIVALENCE ( NDIAG(121),  NSHAPV  )
      EQUIVALENCE ( NDIAG(122),  NSHAPT  )
      EQUIVALENCE ( NDIAG(123),  NSHAPQ  )
      EQUIVALENCE ( NDIAG(124),  NSDIAG3 )
      EQUIVALENCE ( NDIAG(125),  NSDIAG4 )
      EQUIVALENCE ( NDIAG(126),  NSDIAG5 )
      EQUIVALENCE ( NDIAG(127),  NSDIAG6 )
      EQUIVALENCE ( NDIAG(128),  NSDIAG7 )
      EQUIVALENCE ( NDIAG(129),  NSDIAG8 )
      EQUIVALENCE ( NDIAG(130),  NSDIAG9 )
      EQUIVALENCE ( NDIAG(131),  NSDIAG10)
      EQUIVALENCE ( NDIAG(132),  NUDIAG3 )
      EQUIVALENCE ( NDIAG(133),  NUDIAG4 )
      EQUIVALENCE ( NDIAG(134),  NUDIAG5 )
      EQUIVALENCE ( NDIAG(135),  NUDIAG6 )
      EQUIVALENCE ( NDIAG(136),  NUDIAG7 )
      EQUIVALENCE ( NDIAG(137),  NUDIAG8 )
      EQUIVALENCE ( NDIAG(138),  NUDIAG9 )
      EQUIVALENCE ( NDIAG(139),  NUDIAG10)
      EQUIVALENCE ( NDIAG(140),  NCLDMID )
      EQUIVALENCE ( NDIAG(141),  NCLDHI  )
      EQUIVALENCE ( NDIAG(142),  NTAULOW )
      EQUIVALENCE ( NDIAG(143),  NTAUMID )
      EQUIVALENCE ( NDIAG(144),  NTAUHI  )
      EQUIVALENCE ( NDIAG(145),  NCLDNP  )
      EQUIVALENCE ( NDIAG(146),  NCLDPRS )
      EQUIVALENCE ( NDIAG(147),  NCTPCNT )
      EQUIVALENCE ( NDIAG(148),  NCLDTMP )
      EQUIVALENCE ( NDIAG(149),  NCTTCNT )
      EQUIVALENCE ( NDIAG(150),  NTAULOWC)
      EQUIVALENCE ( NDIAG(151),  NTAUMIDC)
      EQUIVALENCE ( NDIAG(152),  NTAUHIC )
      EQUIVALENCE ( NDIAG(153),  NTCANOPY  )
      EQUIVALENCE ( NDIAG(154),  NTDEEP    )
      EQUIVALENCE ( NDIAG(155),  NQCANOPY  )
      EQUIVALENCE ( NDIAG(156),  NSMSHAL   )
      EQUIVALENCE ( NDIAG(157),  NSMROOT   )
      EQUIVALENCE ( NDIAG(158),  NSMDEEP   )
      EQUIVALENCE ( NDIAG(159),  NCAPACITY )
      EQUIVALENCE ( NDIAG(160),  NSNOW     )
      EQUIVALENCE ( NDIAG(161),  NRAINCON  )
      EQUIVALENCE ( NDIAG(162),  NRAINLSP  )
      EQUIVALENCE ( NDIAG(163),  NSNOWFALL )
      EQUIVALENCE ( NDIAG(164),  NRUNOFF   )
      EQUIVALENCE ( NDIAG(165),  NFWSOIL   )
      EQUIVALENCE ( NDIAG(166),  NGDRAIN   )
      EQUIVALENCE ( NDIAG(167),  NSNOWMELT )
      EQUIVALENCE ( NDIAG(168),  NERESV    )
      EQUIVALENCE ( NDIAG(169),  NESOIL    )
      EQUIVALENCE ( NDIAG(170),  NEVEG     )
      EQUIVALENCE ( NDIAG(171),  NESNOW    )
      EQUIVALENCE ( NDIAG(172),  NPARDF    )
      EQUIVALENCE ( NDIAG(173),  NPARDR    )
      EQUIVALENCE ( NDIAG(174),  NLAI      )
      EQUIVALENCE ( NDIAG(175),  NGREEN    )
      EQUIVALENCE ( NDIAG(176),  NDLWDTC   )
      EQUIVALENCE ( NDIAG(177),  NDHDTC    )
      EQUIVALENCE ( NDIAG(178),  NDEDTC    )
      EQUIVALENCE ( NDIAG(179),  NVDTMOIST )
      EQUIVALENCE ( NDIAG(180),  NVDTTURB  )
      EQUIVALENCE ( NDIAG(181),  NVDTRADLW )
      EQUIVALENCE ( NDIAG(182),  NVDTRADSW )
      EQUIVALENCE ( NDIAG(184),  NPSUBCLD  )
      EQUIVALENCE ( NDIAG(185),  NPSUBCLDC )
      EQUIVALENCE ( NDIAG(186),  NLCL      )
      EQUIVALENCE ( NDIAG(187),  NSDIAGC   )
      EQUIVALENCE ( NDIAG(188),  NSDIAGCC  )

      EQUIVALENCE ( NDIAG(190),  NEVPOT    )
      EQUIVALENCE ( NDIAG(191),  NRHCHECK  )
      EQUIVALENCE ( NDIAG(192),  NDHDQA    )
      EQUIVALENCE ( NDIAG(193),  NDEDQA    )
      EQUIVALENCE ( NDIAG(194),  NDTC      )
      EQUIVALENCE ( NDIAG(195),  NDQC      )
      EQUIVALENCE ( NDIAG(196),  NTCDTC    )
      EQUIVALENCE ( NDIAG(197),  NRADDTC   )
      EQUIVALENCE ( NDIAG(198),  NSENSDTC  )
      EQUIVALENCE ( NDIAG(199),  NLATDTC   )
      EQUIVALENCE ( NDIAG(200),  NTDDTC    )
      EQUIVALENCE ( NDIAG(201),  NQCDTC    )
      EQUIVALENCE ( NDIAG(202),  NTRBQLIQ  )
      EQUIVALENCE ( NDIAG(203),  NTRBFCC   )
      EQUIVALENCE ( NDIAG(204),  NALBEDO   )
      EQUIVALENCE ( NDIAG(205),  NALBVISDR )
      EQUIVALENCE ( NDIAG(206),  NALBVISDF )
      EQUIVALENCE ( NDIAG(207),  NALBNIRDR )
      EQUIVALENCE ( NDIAG(208),  NALBNIRDF )
      EQUIVALENCE ( NDIAG(209),  NTAUAVE   )
      EQUIVALENCE ( NDIAG(210),  NALBEDOC  )
      EQUIVALENCE ( NDIAG(211),  NRHCHECKT )
      EQUIVALENCE ( NDIAG(212),  NRHCHECKQ )
      EQUIVALENCE ( NDIAG(213),  NCLDLSP   )
      EQUIVALENCE ( NDIAG(214),  NLZLOW    )
      EQUIVALENCE ( NDIAG(215),  NLZMID    )
      EQUIVALENCE ( NDIAG(216),  NLZHI     )
      EQUIVALENCE ( NDIAG(217),  NLZTOT    )
      EQUIVALENCE ( NDIAG(218),  NCAPE     )
      EQUIVALENCE ( NDIAG(219),  NHSUBCL   )
      EQUIVALENCE ( NDIAG(220),  NHSTAR    )
      EQUIVALENCE ( NDIAG(221),  NCONT     )
      EQUIVALENCE ( NDIAG(222),  NCONQ     )
      EQUIVALENCE ( NDIAG(223),  NLST      )
      EQUIVALENCE ( NDIAG(224),  NLSQ      )
      EQUIVALENCE ( NDIAG(225),  NCONEVPT  )
      EQUIVALENCE ( NDIAG(226),  NCONEVPQ  )
      EQUIVALENCE ( NDIAG(227),  NLSEVPT   )
      EQUIVALENCE ( NDIAG(228),  NLSEVPQ   )
      EQUIVALENCE ( NDIAG(229),  NGWCUMID  )
      EQUIVALENCE ( NDIAG(230),  NGWCVMID  )
      EQUIVALENCE ( NDIAG(231),  NCLDSTD   )
      EQUIVALENCE ( NDIAG(232),  NGWCUBAR  )
      EQUIVALENCE ( NDIAG(233),  NGWCVBAR  )
      EQUIVALENCE ( NDIAG(234),  NGWCUS    )
      EQUIVALENCE ( NDIAG(235),  NGWCVS    )
      EQUIVALENCE ( NDIAG(236),  NGWCU     )
      EQUIVALENCE ( NDIAG(237),  NGWCV     )


C diagnostics common
C      qSdiag  - storage array for (per level) statistics 

      _RL qdiag(1-OLx:sNx+Olx,1-Oly:sNy+Oly,numdiags,nSx,nSy)
      _RL qSdiag(0:nStats,0:nRegions,diagSt_size,nSx,nSy)

      common /diagnostics/ qdiag, qSdiag

	
C diag_choices common
C     freq       :: frequency (in s) to write output stream # n
C     phase      :: phase     (in s) to write output stream # n
C     nfields(n) :: number of active diagnostics for output stream # n
C     nActive(n) :: number of active diagnostics (including counters)
C                   for output stream # n
C     fflags(n)  :: character string with per-file flags

      integer nlists

      _RL freq(numlists), phase(numlists)
      integer nlevels(numlists)
      integer nfields(numlists)
      integer nActive(numlists)
      _RL levs (numLevels,numlists)
      integer jdiag(numperlist,numlists)
      character*8 flds (numperlist,numlists)
      character*80 fnames(numlists)
      character*8 fflags(numlists)
      logical
     &     diag_mdsio, diag_mnc,
     &     diag_pickup_read,        diag_pickup_write,
     &     diag_pickup_read_mdsio,  diag_pickup_write_mdsio,
     &     diag_pickup_read_mnc,    diag_pickup_write_mnc

      common /diag_choices/ 
     &     freq, phase, levs, nlevels, 
     &     nfields, nActive, nlists, jdiag,
     &     flds, fnames, fflags,
     &     diag_mdsio, diag_mnc,
     &     diag_pickup_read,        diag_pickup_write,
     &     diag_pickup_read_mdsio,  diag_pickup_write_mdsio,
     &     diag_pickup_read_mnc,    diag_pickup_write_mnc
           
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      _RL       diagSt_freq(numlists), diagSt_phase(numlists)
      CHARACTER*8  diagSt_Flds(numperlist,numlists)
      CHARACTER*80 diagSt_Fname(numlists)
      INTEGER   iSdiag(ndiagMax)
      INTEGER   jSdiag(numperlist,numlists)
      INTEGER   diagSt_region(0:nRegions,numlists)
      INTEGER   diagSt_nbFlds(numlists)
      INTEGER   diagSt_nbActv(numlists)
      INTEGER   diagSt_nbLists
      INTEGER   diagSt_ioUnit(numlists)
      LOGICAL   diagSt_ascii, diagSt_mnc
      COMMON / DIAG_STATIS / 
     &     diagSt_freq, diagSt_phase, 
     &     diagSt_Flds, diagSt_Fname,
     &     iSdiag, jSdiag, diagSt_region,
     &     diagSt_nbFlds, diagSt_nbActv, diagSt_nbLists,
     &     diagSt_ioUnit,
     &     diagSt_Ascii, diagSt_mnc

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
