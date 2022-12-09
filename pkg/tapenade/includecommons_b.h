C debut du gros include des commons
      INTEGER snx
      INTEGER sny
      INTEGER olx
      INTEGER oly
      INTEGER nsx
      INTEGER nsy
      INTEGER npx
      INTEGER npy
      INTEGER nx
      INTEGER ny
      INTEGER nr
      PARAMETER (snx=45, sny=20, olx=2, oly=2, nsx=2, nsy=2, npx=1, npy=
     +   1, nx=snx*nsx*npx, ny=sny*nsy*npy, nr=15)
      INTEGER max_olx
      INTEGER max_oly
      PARAMETER (max_olx=olx, max_oly=oly)
      INTEGER nobcs
      PARAMETER (nobcs=4)
C
      INTEGER max_len_mbuf
      PARAMETER (max_len_mbuf=512)
      INTEGER max_len_fnam
      PARAMETER (max_len_fnam=512)
      INTEGER max_len_prec
      PARAMETER (max_len_prec=200)
CC    MAX_NO_PROCS    :: Maximum number of processes allowed.
CC    MAX_NO_BARRIERS :: Maximum number of distinct thread "barriers"
      INTEGER max_no_threads
      PARAMETER (max_no_threads=4)
      INTEGER cachelinesize
      INTEGER lshare1
      INTEGER lshare4
      INTEGER lshare8
      PARAMETER (cachelinesize=256)
      PARAMETER (lshare1=cachelinesize)
      PARAMETER (lshare4=cachelinesize/4)
      PARAMETER (lshare8=cachelinesize/8)
CC    MAX_VGS  :: Maximum buffer size for Global Vector Sum
      INTEGER precfloat32
      PARAMETER (precfloat32=32)
      INTEGER precfloat64
      PARAMETER (precfloat64=64)
      REAL*8 zerors, oners, twors, halfrs
      PARAMETER (zerors=0.0d0, oners=1.0d0)
      PARAMETER (twors=2.0d0, halfrs=0.5d0)
      REAL*8 zerorl, onerl, tworl, halfrl
      PARAMETER (zerorl=0.0d0, onerl=1.0d0)
      PARAMETER (tworl=2.0d0, halfrl=0.5d0)
      REAL*8 unset_float8
      PARAMETER (unset_float8=1.234567d5)
      REAL*4 unset_float4
      PARAMETER (unset_float4=1.234567e5)
      REAL*8 unset_rl
      PARAMETER (unset_rl=1.234567d5)
      REAL*8 unset_rs
      PARAMETER (unset_rs=1.234567d5)
      INTEGER unset_i
      PARAMETER (unset_i=123456789)
      INTEGER deblevzero
      INTEGER debleva, deblevb, deblevc, deblevd, debleve
      PARAMETER (deblevzero=0)
      PARAMETER (debleva=1)
      PARAMETER (deblevb=2)
      PARAMETER (deblevc=3)
      PARAMETER (deblevd=4)
      PARAMETER (debleve=5)
      CHARACTER*(*) squeeze_right
      PARAMETER (squeeze_right='R')
      CHARACTER*(*) squeeze_left
      PARAMETER (squeeze_left='L')
      CHARACTER*(*) squeeze_both
      PARAMETER (squeeze_both='B')
      CHARACTER*(*) print_map_xy
      PARAMETER (print_map_xy='XY')
      CHARACTER*(*) print_map_xz
      PARAMETER (print_map_xz='XZ')
      CHARACTER*(*) print_map_yz
      PARAMETER (print_map_yz='YZ')
      CHARACTER*(*) commentcharacter
      PARAMETER (commentcharacter='#')
      INTEGER index_i
      INTEGER index_j
      INTEGER index_k
      INTEGER index_none
      PARAMETER (index_i=1, index_j=2, index_k=3, index_none=4)
      INTEGER exch_ignore_corners
      INTEGER exch_update_corners
      PARAMETER (exch_ignore_corners=0, exch_update_corners=1)
      INTEGER forward_simulation
      INTEGER reverse_simulation
      INTEGER tangent_simulation
      PARAMETER (forward_simulation=0, reverse_simulation=1, 
     +   tangent_simulation=2)
      LOGICAL eebooterror
      LOGICAL eeenderror
      LOGICAL fatalerror
      LOGICAL debugmode
      LOGICAL usesinglecpuio
      LOGICAL usesinglecpuinput
      LOGICAL printmapincludeszeros
      LOGICAL usecubedsphereexchange
      LOGICAL usecoupler
      LOGICAL usenest_parent
      LOGICAL usenest_child
      LOGICAL useoasis
      LOGICAL usesetrlstk
      LOGICAL usesigreg
      COMMON /eeparams_l/ eebooterror, eeenderror, fatalerror, debugmode
     +, usesinglecpuio, usesinglecpuinput, printmapincludeszeros, 
     +usecubedsphereexchange, usecoupler, usenest_parent, usenest_child
     +, useoasis, usesetrlstk, usesigreg
      INTEGER errormessageunit
      INTEGER standardmessageunit
      INTEGER maxlengthprt1d
      INTEGER scrunit1
      INTEGER scrunit2
      INTEGER eedataunit
      INTEGER modeldataunit
      INTEGER ioerrorcount(max_no_threads)
      INTEGER mybxlo(max_no_threads)
      INTEGER mybxhi(max_no_threads)
      INTEGER mybylo(max_no_threads)
      INTEGER mybyhi(max_no_threads)
      INTEGER myprocid
      INTEGER mypx
      INTEGER mypy
      INTEGER myxgloballo
      INTEGER myygloballo
      INTEGER nthreads
      INTEGER ntx
      INTEGER nty
      INTEGER numberofprocs
      INTEGER pidio
      COMMON /eeparams_i/ errormessageunit, standardmessageunit, 
     +maxlengthprt1d, scrunit1, scrunit2, eedataunit, modeldataunit, 
     +numberofprocs, pidio, myprocid, mypx, mypy, myxgloballo, 
     +myygloballo, nthreads, mybxlo, mybxhi, mybylo, mybyhi, ntx, nty, 
     +ioerrorcount
C
      REAL*8 pi
      PARAMETER (pi=3.14159265358979323844d0)
      REAL*8 deg2rad
      PARAMETER (deg2rad=2.d0*pi/360.d0)
      CHARACTER*(max_len_fnam) buoyancyrelation
      CHARACTER*6 eostype
      CHARACTER*10 pickupsuff
      CHARACTER*(max_len_fnam) mdsiolocaldir
      CHARACTER*(max_len_fnam) adtapedir
      CHARACTER*(max_len_fnam) treffile
      CHARACTER*(max_len_fnam) sreffile
      CHARACTER*(max_len_fnam) rhoreffile
      CHARACTER*(max_len_fnam) delrfile
      CHARACTER*(max_len_fnam) delrcfile
      CHARACTER*(max_len_fnam) hybsigmfile
      CHARACTER*(max_len_fnam) delxfile
      CHARACTER*(max_len_fnam) delyfile
      CHARACTER*(max_len_fnam) horizgridfile
      CHARACTER*(max_len_fnam) bathyfile, topofile
      CHARACTER*(max_len_fnam) addwwallfile, addswallfile
      CHARACTER*(max_len_fnam) hydrogthetafile, hydrogsaltfile
      CHARACTER*(max_len_fnam) diffkrfile
      CHARACTER*(max_len_fnam) viscahdfile
      CHARACTER*(max_len_fnam) viscahzfile
      CHARACTER*(max_len_fnam) visca4dfile
      CHARACTER*(max_len_fnam) visca4zfile
      CHARACTER*(max_len_fnam) zonalwindfile
      CHARACTER*(max_len_fnam) meridwindfile
      CHARACTER*(max_len_fnam) thetaclimfile
      CHARACTER*(max_len_fnam) saltclimfile
      CHARACTER*(max_len_fnam) surfqfile
      CHARACTER*(max_len_fnam) surfqnetfile
      CHARACTER*(max_len_fnam) surfqswfile
      CHARACTER*(max_len_fnam) empmrfile
      CHARACTER*(max_len_fnam) saltfluxfile
      CHARACTER*(max_len_fnam) uvelinitfile
      CHARACTER*(max_len_fnam) vvelinitfile
      CHARACTER*(max_len_fnam) psurfinitfile
      CHARACTER*(max_len_fnam) ploadfile
      CHARACTER*(max_len_fnam) addmassfile
      CHARACTER*(max_len_fnam) eddypsixfile
      CHARACTER*(max_len_fnam) eddypsiyfile
      CHARACTER*(max_len_fnam) geothermalfile
      CHARACTER*(max_len_fnam) lambdathetafile
      CHARACTER*(max_len_fnam) lambdasaltfile
      CHARACTER*(max_len_prec/2) the_run_name
      COMMON /parm_c/ buoyancyrelation, eostype, pickupsuff, 
     +mdsiolocaldir, adtapedir, treffile, sreffile, rhoreffile, delrfile
     +, delrcfile, hybsigmfile, delxfile, delyfile, horizgridfile, 
     +bathyfile, topofile, addwwallfile, addswallfile, viscahdfile, 
     +viscahzfile, visca4dfile, visca4zfile, hydrogthetafile, 
     +hydrogsaltfile, diffkrfile, zonalwindfile, meridwindfile, 
     +thetaclimfile, saltclimfile, empmrfile, saltfluxfile, surfqfile, 
     +surfqnetfile, surfqswfile, lambdathetafile, lambdasaltfile, 
     +uvelinitfile, vvelinitfile, psurfinitfile, ploadfile, addmassfile
     +, eddypsixfile, eddypsiyfile, geothermalfile, the_run_name
      INTEGER cg2dmaxiters
      INTEGER cg2dchkresfreq
      INTEGER cg2dprecondfreq
      INTEGER cg2duseminressol
      INTEGER cg3dmaxiters
      INTEGER cg3dchkresfreq
      INTEGER printresidualfreq
      INTEGER niter0
      INTEGER ntimesteps
      INTEGER nenditer
      INTEGER writestateprec
      INTEGER writebinaryprec
      INTEGER readbinaryprec
      INTEGER selectcorimap
      INTEGER selectsigmacoord
      INTEGER nonlinfreesurf
      INTEGER select_rstar
      INTEGER selectnhfreesurf
      INTEGER selectaddfluid
      INTEGER momforcingoutab, tracforcingoutab
      INTEGER tempadvscheme, tempvertadvscheme
      INTEGER saltadvscheme, saltvertadvscheme
      INTEGER selectkescheme
      INTEGER selectvortscheme
      INTEGER selectbotdragquadr
      INTEGER monitorselect
      INTEGER debuglevel
      COMMON /parm_i/ cg2dmaxiters, cg2dchkresfreq, cg2dprecondfreq, 
     +cg2duseminressol, cg3dmaxiters, cg3dchkresfreq, printresidualfreq
     +, niter0, ntimesteps, nenditer, writestateprec, writebinaryprec, 
     +readbinaryprec, selectcorimap, selectsigmacoord, nonlinfreesurf, 
     +select_rstar, selectnhfreesurf, selectaddfluid, momforcingoutab, 
     +tracforcingoutab, tempadvscheme, tempvertadvscheme, saltadvscheme
     +, saltvertadvscheme, selectkescheme, selectvortscheme, 
     +selectbotdragquadr, monitorselect, debuglevel
      LOGICAL fluidisair
      LOGICAL fluidiswater
      LOGICAL usingpcoords
      LOGICAL usingzcoords
      LOGICAL usedynp_ineos_zc
      LOGICAL usingcartesiangrid
      LOGICAL usingsphericalpolargrid, rotategrid
      LOGICAL usingcylindricalgrid
      LOGICAL usingcurvilineargrid, haswetcscorners
      LOGICAL deepatmosphere
      LOGICAL setinterfdr
      LOGICAL setcenterdr
      LOGICAL no_slip_sides
      LOGICAL no_slip_bottom
      LOGICAL bottomvisc_pcell
      LOGICAL usesmag3d
      LOGICAL usefullleith
      LOGICAL usestraintensionvisc
      LOGICAL useareavisclength
      LOGICAL momviscosity
      LOGICAL momadvection
      LOGICAL momforcing
      LOGICAL mompressureforcing
      LOGICAL metricterms
      LOGICAL usenhmterms
      LOGICAL usecoriolis
      LOGICAL use3dcoriolis
      LOGICAL usecdscheme
      LOGICAL vectorinvariantmomentum
      LOGICAL useenergyconservingcoriolis
      LOGICAL usejamartwetpoints
      LOGICAL usejamartmomadv
      LOGICAL upwindvorticity
      LOGICAL highordervorticity
      LOGICAL useabsvorticity
      LOGICAL upwindshear
      LOGICAL momstepping
      LOGICAL calc_wvelocity
      LOGICAL tempstepping
      LOGICAL saltstepping
      LOGICAL addfrictionheating
      LOGICAL tempadvection
      LOGICAL tempvertdiff4
      LOGICAL tempisactivetr
      LOGICAL tempforcing
      LOGICAL saltadvection
      LOGICAL saltvertdiff4
      LOGICAL saltisactivetr
      LOGICAL saltforcing
      LOGICAL maskinitemp
      LOGICAL maskinisalt
      LOGICAL checkinitemp
      LOGICAL checkinisalt
      LOGICAL usesrcgsolver
      LOGICAL rigidlid
      LOGICAL implicitfreesurface
      LOGICAL uniformlin_phisurf
      LOGICAL uniformfreesurflev
      LOGICAL exactconserv
      LOGICAL linfsconservetr
      LOGICAL userealfreshwaterflux
      LOGICAL quasihydrostatic
      LOGICAL nonhydrostatic
      LOGICAL use3dsolver
      LOGICAL implicitintgravwave
      LOGICAL staggertimestep
      LOGICAL doresethfactors
      LOGICAL implicitdiffusion
      LOGICAL implicitviscosity
      LOGICAL implbottomfriction
      LOGICAL tempimplvertadv
      LOGICAL saltimplvertadv
      LOGICAL momimplvertadv
      LOGICAL multidimadvection
      LOGICAL usemultidimadvec
      LOGICAL momdissip_in_ab
      LOGICAL doab_ongtgs
      LOGICAL balanceempmr
      LOGICAL balanceqnet
      LOGICAL balanceprintmean
      LOGICAL dothetaclimrelax
      LOGICAL dosaltclimrelax
      LOGICAL balancethetaclimrelax
      LOGICAL balancesaltclimrelax
      LOGICAL allowfreezing
      LOGICAL periodicexternalforcing
      LOGICAL globalfiles
      LOGICAL pickupstrictlymatch
      LOGICAL usepickupbeforec54
      LOGICAL startfrompickupab2
      LOGICAL pickup_read_mdsio, pickup_write_mdsio
      LOGICAL pickup_write_immed, writepickupatend
      LOGICAL timeave_mdsio, snapshot_mdsio, monitor_stdio
      LOGICAL outputtypesinclusive
      LOGICAL dumpinitandlast
      LOGICAL printdomain
      COMMON /parm_l/ fluidisair, fluidiswater, usingpcoords, 
     +usingzcoords, usedynp_ineos_zc, usingcartesiangrid, 
     +usingsphericalpolargrid, rotategrid, usingcylindricalgrid, 
     +usingcurvilineargrid, haswetcscorners, deepatmosphere, setinterfdr
     +, setcenterdr, no_slip_sides, no_slip_bottom, bottomvisc_pcell, 
     +usesmag3d, usefullleith, usestraintensionvisc, useareavisclength, 
     +momviscosity, momadvection, momforcing, mompressureforcing, 
     +metricterms, usenhmterms, usecoriolis, use3dcoriolis, usecdscheme
     +, vectorinvariantmomentum, useenergyconservingcoriolis, 
     +usejamartwetpoints, usejamartmomadv, upwindvorticity, 
     +highordervorticity, useabsvorticity, upwindshear, momstepping, 
     +calc_wvelocity, tempstepping, saltstepping, addfrictionheating, 
     +tempadvection, tempvertdiff4, tempisactivetr, tempforcing, 
     +saltadvection, saltvertdiff4, saltisactivetr, saltforcing, 
     +maskinitemp, maskinisalt, checkinitemp, checkinisalt, 
     +usesrcgsolver, rigidlid, implicitfreesurface, uniformlin_phisurf, 
     +uniformfreesurflev, exactconserv, linfsconservetr, 
     +userealfreshwaterflux, quasihydrostatic, nonhydrostatic, 
     +use3dsolver, implicitintgravwave, staggertimestep, doresethfactors
     +, implicitdiffusion, implicitviscosity, implbottomfriction, 
     +tempimplvertadv, saltimplvertadv, momimplvertadv, 
     +multidimadvection, usemultidimadvec, momdissip_in_ab, doab_ongtgs
     +, balanceempmr, balanceqnet, balanceprintmean, 
     +balancethetaclimrelax, balancesaltclimrelax, dothetaclimrelax, 
     +dosaltclimrelax, allowfreezing, periodicexternalforcing, 
     +globalfiles, pickupstrictlymatch, usepickupbeforec54, 
     +startfrompickupab2, pickup_read_mdsio, pickup_write_mdsio, 
     +pickup_write_immed, writepickupatend, timeave_mdsio, 
     +snapshot_mdsio, monitor_stdio, outputtypesinclusive, 
     +dumpinitandlast, printdomain
      REAL*8 cg2dtargetresidual
      REAL*8 cg2dtargetreswunit
      REAL*8 cg3dtargetresidual
      REAL*8 cg2dpcoffdfac
      REAL*8 delr(nr)
      REAL*8 delrc(nr+1)
      REAL*8 xgorigin
      REAL*8 ygorigin
      REAL*8 deltat
      REAL*8 deltatclock
      REAL*8 deltatmom
      REAL*8 dttracerlev(nr)
      REAL*8 deltatfreesurf
      REAL*8 abeps, alph_ab, beta_ab
      REAL*8 rsphere
      REAL*8 recip_rsphere
      REAL*8 radius_fromhorizgrid
      REAL*8 f0
      REAL*8 beta
      REAL*8 fprime
      REAL*8 omega
      REAL*8 rotationperiod
      REAL*8 freesurffac
      REAL*8 implicsurfpress
      REAL*8 implicdiv2dflow
      REAL*8 implicitnhpress
      REAL*8 hfacmin
      REAL*8 hfacmindz
      REAL*8 hfacmindp
      REAL*8 hfacmindr
      REAL*8 hfacinf
      REAL*8 hfacsup
      REAL*8 viscarnr(nr)
      REAL*8 viscfacadj
      REAL*8 viscah
      REAL*8 viscahw
      REAL*8 viscahd
      REAL*8 viscahz
      REAL*8 smag3d_coeff
      REAL*8 viscahmax
      REAL*8 viscahremax
      REAL*8 viscahgrid, viscahgridmax, viscahgridmin
      REAL*8 viscc2leith
      REAL*8 viscc2leithd
      REAL*8 viscc2smag
      REAL*8 visca4
      REAL*8 visca4w
      REAL*8 visca4d
      REAL*8 visca4z
      REAL*8 visca4max
      REAL*8 visca4remax
      REAL*8 visca4grid, visca4gridmax, visca4gridmin
      REAL*8 viscc4leith
      REAL*8 viscc4leithd
      REAL*8 viscc4smag
      REAL*8 diffkht
      REAL*8 diffk4t
      REAL*8 diffkrnrt(nr)
      REAL*8 diffkr4t(nr)
      REAL*8 diffkhs
      REAL*8 diffk4s
      REAL*8 diffkrnrs(nr)
      REAL*8 diffkr4s(nr)
      REAL*8 diffkrbl79surf
      REAL*8 diffkrbl79deep
      REAL*8 diffkrbl79scl
      REAL*8 diffkrbl79ho
      REAL*8 bl79latvary
      REAL*8 diffkrbleqsurf
      REAL*8 diffkrbleqdeep
      REAL*8 diffkrbleqscl
      REAL*8 diffkrbleqho
      REAL*8 taucd, rcd, epsab_cd
      REAL*8 gravity
      REAL*8 recip_gravity
      REAL*8 gbaro
      REAL*8 rhonil
      REAL*8 rhoconst, recip_rhoconst
      REAL*8 thetaconst
      REAL*8 rhofacc(nr), recip_rhofacc(nr)
      REAL*8 rhofacf(nr+1), recip_rhofacf(nr+1)
      REAL*8 rhoconstfresh
      REAL*8 rho1ref(nr)
      REAL*8 tref(nr)
      REAL*8 sref(nr)
      REAL*8 phiref(2*nr+1)
      REAL*8 dbdrref(nr)
      REAL*8 rvel2wunit(nr+1), wunit2rvel(nr+1)
      REAL*8 mass2runit, runit2mass
      REAL*8 basetime
      REAL*8 starttime
      REAL*8 endtime
      REAL*8 chkptfreq
      REAL*8 pchkptfreq
      REAL*8 dumpfreq
      REAL*8 adjdumpfreq
      REAL*8 diagfreq
      REAL*8 tavefreq
      REAL*8 tave_lastiter
      REAL*8 monitorfreq
      REAL*8 adjmonitorfreq
      REAL*8 affacmom
      REAL*8 vffacmom
      REAL*8 pffacmom
      REAL*8 cffacmom
      REAL*8 fofacmom
      REAL*8 mtfacmom
      REAL*8 cospower
      REAL*8 cadjfreq
      REAL*8 tauthetaclimrelax
      REAL*8 tausaltclimrelax
      REAL*8 latbandclimrelax
      REAL*8 externforcingcycle
      REAL*8 externforcingperiod
      REAL*8 convertfw2salt
      REAL*8 temp_evprrn
      REAL*8 salt_evprrn
      REAL*8 temp_addmass
      REAL*8 salt_addmass
      REAL*8 ivdc_kappa
      REAL*8 hmixcriteria
      REAL*8 drhosmall
      REAL*8 hmixsmooth
      REAL*8 ro_sealevel
      REAL*8 rsigmabnd
      REAL*8 sidedragfactor
      REAL*8 bottomdraglinear
      REAL*8 bottomdragquadratic
      REAL*8 smoothabsfuncrange
      REAL*8 nh_am2
      REAL*8 tcylin, tcylout
      REAL*8 phieuler, thetaeuler, psieuler
C
      COMMON /parm_r/ cg2dtargetresidual, cg2dtargetreswunit, 
     +cg2dpcoffdfac, cg3dtargetresidual, delr, delrc, xgorigin, ygorigin
     +, deltat, deltatmom, dttracerlev, deltatfreesurf, deltatclock, 
     +abeps, alph_ab, beta_ab, rsphere, recip_rsphere, 
     +radius_fromhorizgrid, f0, beta, fprime, omega, rotationperiod, 
     +viscfacadj, viscah, viscahw, smag3d_coeff, viscahmax, viscahgrid, 
     +viscahgridmax, viscahgridmin, viscc2leith, viscc2leithd, 
     +viscc2smag, viscc4smag, viscahd, viscahz, visca4d, visca4z, visca4
     +, visca4w, visca4max, visca4grid, visca4gridmax, visca4gridmin, 
     +viscahremax, visca4remax, viscc4leith, viscc4leithd, viscarnr, 
     +diffkht, diffk4t, diffkrnrt, diffkr4t, diffkhs, diffk4s, diffkrnrs
     +, diffkr4s, diffkrbl79surf, diffkrbl79deep, diffkrbl79scl, 
     +diffkrbl79ho, bl79latvary, diffkrbleqsurf, diffkrbleqdeep, 
     +diffkrbleqscl, diffkrbleqho, taucd, rcd, epsab_cd, freesurffac, 
     +implicsurfpress, implicdiv2dflow, implicitnhpress, hfacmin, 
     +hfacmindz, hfacinf, hfacsup, gravity, recip_gravity, gbaro, rhonil
     +, rhoconst, recip_rhoconst, thetaconst, rhofacc, recip_rhofacc, 
     +rhofacf, recip_rhofacf, rhoconstfresh, rho1ref, tref, sref, phiref
     +, dbdrref, rvel2wunit, wunit2rvel, mass2runit, runit2mass, 
     +basetime, starttime, endtime, chkptfreq, pchkptfreq, dumpfreq, 
     +adjdumpfreq, diagfreq, tavefreq, tave_lastiter, monitorfreq, 
     +adjmonitorfreq, affacmom, vffacmom, pffacmom, cffacmom, fofacmom, 
     +mtfacmom, cospower, cadjfreq, tauthetaclimrelax, tausaltclimrelax
     +, latbandclimrelax, externforcingcycle, externforcingperiod, 
     +convertfw2salt, temp_evprrn, salt_evprrn, temp_addmass, 
     +salt_addmass, hfacmindr, hfacmindp, ivdc_kappa, hmixcriteria, 
     +drhosmall, hmixsmooth, ro_sealevel, rsigmabnd, sidedragfactor, 
     +bottomdraglinear, bottomdragquadratic, nh_am2, smoothabsfuncrange
     +, tcylin, tcylout, phieuler, thetaeuler, psieuler
      REAL*8 heatcapacity_cp
      COMMON /parm_a/ heatcapacity_cp
      REAL*8 celsius2k
      REAL*8 atm_po, atm_cp, atm_rd, atm_kappa, atm_rq
      INTEGER integr_geopot, selectfindrosurf
      COMMON /parm_atm/ celsius2k, atm_cp, atm_rd, atm_kappa, atm_rq, 
     +atm_po, integr_geopot, selectfindrosurf
      LOGICAL usegad
      LOGICAL useobcs
      LOGICAL useshap_filt
      LOGICAL usezonal_filt
      LOGICAL useopps
      LOGICAL usepp81
      LOGICAL usekl10
      LOGICAL usemy82
      LOGICAL useggl90
      LOGICAL usekpp
      LOGICAL usegmredi
      LOGICAL usedown_slope
      LOGICAL usebbl
      LOGICAL usecal
      LOGICAL useexf
      LOGICAL usebulkforce
      LOGICAL useebm
      LOGICAL usecheapaml
      LOGICAL useautodiff
      LOGICAL usegrdchk
      LOGICAL usesmooth
      LOGICAL useprofiles
      LOGICAL useecco
      LOGICAL usectrl
      LOGICAL usesbo
      LOGICAL useflt
      LOGICAL useptracers
      LOGICAL usegchem
      LOGICAL userbcs
      LOGICAL useoffline
      LOGICAL usematrix
      LOGICAL usefrazil
      LOGICAL useseaice
      LOGICAL usesalt_plume
      LOGICAL useshelfice
      LOGICAL usestreamice
      LOGICAL useicefront
      LOGICAL usethsice
      LOGICAL useland
      LOGICAL useatm2d
      LOGICAL useaim
      LOGICAL useatm_phys
      LOGICAL usefizhi
      LOGICAL usegridalt
      LOGICAL usediagnostics
      LOGICAL useregrid
      LOGICAL uselayers
      LOGICAL usemnc
      LOGICAL userunclock
      LOGICAL useembed_files
      LOGICAL usemypackage
      COMMON /parm_packages/ usegad, useobcs, useshap_filt, 
     +usezonal_filt, useopps, usepp81, usekl10, usemy82, useggl90, 
     +usekpp, usegmredi, usebbl, usedown_slope, usecal, useexf, 
     +usebulkforce, useebm, usecheapaml, usegrdchk, usesmooth, 
     +useprofiles, useecco, usectrl, usesbo, useflt, useautodiff, 
     +useptracers, usegchem, userbcs, useoffline, usematrix, usefrazil, 
     +useseaice, usesalt_plume, useshelfice, usestreamice, useicefront, 
     +usethsice, useland, useatm2d, useaim, useatm_phys, usefizhi, 
     +usegridalt, usediagnostics, useregrid, uselayers, usemnc, 
     +userunclock, useembed_files, usemypackage
      REAL*8 cosfacu(1-oly:sny+oly, nsx, nsy)
      REAL*8 cosfacv(1-oly:sny+oly, nsx, nsy)
      REAL*8 sqcosfacu(1-oly:sny+oly, nsx, nsy)
      REAL*8 sqcosfacv(1-oly:sny+oly, nsx, nsy)
      REAL*8 deepfacc(nr)
      REAL*8 deepfac2c(nr)
      REAL*8 deepfacf(nr+1)
      REAL*8 deepfac2f(nr+1)
      REAL*8 recip_deepfacc(nr)
      REAL*8 recip_deepfac2c(nr)
      REAL*8 recip_deepfacf(nr+1)
      REAL*8 recip_deepfac2f(nr+1)
      REAL*8 gravitysign
      REAL*8 rksign
      REAL*8 globalarea
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
      COMMON /grid_rl/ cosfacu, cosfacv, sqcosfacu, sqcosfacv, deepfacc
     +, deepfac2c, recip_deepfacc, recip_deepfac2c, deepfacf, deepfac2f
     +, recip_deepfacf, recip_deepfac2f, gravitysign, rksign, globalarea
      REAL*8 dxc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dxf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dxg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dxv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dyc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dyf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dyg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 dyu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 r_low(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 rloww(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 rlows(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 ro_surf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 rsurfw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 rsurfs(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 hfacc(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 hfacw(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 hfacs(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 recip_dxc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dxf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dxg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dxv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dyc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dyf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dyg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_dyu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_rcol(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_hfacc(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 recip_hfacw(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 recip_hfacs(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 xc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 xg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 yc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 yg(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 ra(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 raw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 ras(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 raz(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_ra(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_raw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_ras(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_raz(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 maskinc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 maskinw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 maskins(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 maskc(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 maskw(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 masks(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 drc(nr+1)
      REAL*8 drf(nr)
      REAL*8 recip_drc(nr+1)
      REAL*8 recip_drf(nr)
      REAL*8 rc(nr)
      REAL*8 rf(nr+1)
      REAL*8 ahybsigmf(nr+1)
      REAL*8 bhybsigmf(nr+1)
      REAL*8 ahybsigmc(nr)
      REAL*8 bhybsigmc(nr)
      REAL*8 dahybsigf(nr)
      REAL*8 dbhybsigf(nr)
      REAL*8 dbhybsigc(nr+1)
      REAL*8 dahybsigc(nr+1)
      REAL*8 tanphiatu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tanphiatv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 anglecosc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 anglesinc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 u2zondir(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 v2zondir(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 fcori(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 fcorig(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 fcoricos(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /grid_rs/ dxc, dxf, dxg, dxv, dyc, dyf, dyg, dyu, r_low, 
     +rloww, rlows, ro_surf, rsurfw, rsurfs, hfacc, hfacw, hfacs, 
     +recip_dxc, recip_dxf, recip_dxg, recip_dxv, recip_dyc, recip_dyf, 
     +recip_dyg, recip_dyu, recip_rcol, recip_hfacc, recip_hfacw, 
     +recip_hfacs, xc, yc, ra, raw, ras, raz, xg, yg, maskinc, maskinw, 
     +maskins, maskc, maskw, masks, recip_ra, recip_raw, recip_ras, 
     +recip_raz, drc, drf, recip_drc, recip_drf, rc, rf, ahybsigmf, 
     +bhybsigmf, ahybsigmc, bhybsigmc, dahybsigf, dbhybsigf, dbhybsigc, 
     +dahybsigc, tanphiatu, tanphiatv, anglecosc, anglesinc, u2zondir, 
     +v2zondir, fcori, fcorig, fcoricos
      INTEGER ksurfc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      INTEGER ksurfw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      INTEGER ksurfs(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      INTEGER klowc(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /grid_i/ ksurfc, ksurfw, ksurfs, klowc
      REAL*8 etan(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 etanb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 uvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 uvelb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vvelb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 wvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 wvelb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 theta(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 thetab(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 salt(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 saltb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gu(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gub(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gv(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gvb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gunm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gunm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gvnm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gvnm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gtnm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gtnm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gsnm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gsnm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /dynvars_r/ etan, uvel, vvel, wvel, theta, salt, gu, gv, 
     +gunm1, gvnm1, gtnm1, gsnm1
      REAL*8 etah(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 etahb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /dynvars_r_2/ etah
      REAL*8 phihydlow(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 totphihyd(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 totphihydb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 rhoinsitu(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 rhoinsitub(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 hmixlayer(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 ivdconvcount(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /dynvars_diag/ phihydlow, totphihyd, rhoinsitu, hmixlayer, 
     +ivdconvcount
      REAL*8 bo_surf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 recip_bo(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 topoz(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 phi0surf(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
C
      COMMON /surf_fixed/ bo_surf, recip_bo, topoz, phi0surf
      REAL*8 tsurfcor
      REAL*8 tsurfcorb
      REAL*8 ssurfcor
      REAL*8 ssurfcorb
      COMMON /surf_correc/ tsurfcor, ssurfcor
      REAL*8 etahnm1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 detahdt(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 detahdtb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 pmepr(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 pmeprb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /exact_eta_local/ etahnm1, detahdt, pmepr
      REAL*8 fu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 fub(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
C
C
C
C
C
C
C
C
C
C
C
      COMMON /ffields_fu/ fu
      REAL*8 fv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 fvb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_fv/ fv
      REAL*8 qnet(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnetb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_qnet/ qnet
      REAL*8 qsw(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_qsw/ qsw
      REAL*8 dqdt(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_dqdt/ dqdt
      REAL*8 empmr(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 empmrb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_empmr/ empmr
      REAL*8 saltflux(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 saltfluxb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_saltflux/ saltflux
      REAL*8 sst(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sstb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_sst/ sst
      REAL*8 sss(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sssb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_sss/ sss
      REAL*8 lambdathetaclimrelax(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy
     +       )
      COMMON /ffields_lambdathetaclimrelax/ lambdathetaclimrelax
      REAL*8 lambdasaltclimrelax(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_lambdasaltclimrelax/ lambdasaltclimrelax
      REAL*8 pload(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_pload/ pload
      REAL*8 siceload(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ffields_siceload/ siceload
      REAL*8 qnetm(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnetmb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /mean_qnet/ qnetm
      INTEGER loadedrec(nsx, nsy)
      COMMON /ffields_i/ loadedrec
      REAL*8 taux0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 taux0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tauy0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tauy0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnet0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnet0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 empmr0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 empmr0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 saltflux0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 saltflux0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sst0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sst0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sss0(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sss0b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 taux1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 taux1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tauy1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tauy1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnet1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 qnet1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 empmr1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 empmr1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 saltflux1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 saltflux1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sst1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sst1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sss1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sss1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /tdfields/ taux0, tauy0, qnet0, empmr0, sst0, sss0, taux1, 
     +tauy1, qnet1, empmr1, sst1, sss1, saltflux0, saltflux1
      REAL*8 surfaceforcingu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingub(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingvb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingt(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingtb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcings(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingsb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingtice(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 surfaceforcingticeb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
C
C
      COMMON /surface_forcing/ surfaceforcingu, surfaceforcingv, 
     +surfaceforcingt, surfaceforcings, surfaceforcingtice
      REAL*8 sitobar, sitodbar
      PARAMETER (sitobar=1.d-05)
      PARAMETER (sitodbar=1.d-04)
      REAL*8 talpha
      REAL*8 sbeta
      CHARACTER*6 equationofstate
      COMMON /parm_eos_lin/ talpha, sbeta, equationofstate
      REAL*8 eosc(9, nr+1), eossig0(nr+1), eosreft(nr+1), eosrefs(nr+1)
      COMMON /parm_eos_nl/ eosc, eossig0, eosreft, eosrefs
      REAL*8 eosjmdcfw(6), eosjmdcsw(9)
      REAL*8 eosjmdckfw(5), eosjmdcksw(7), eosjmdckp(14)
      COMMON /parm_eos_jmd95/ eosjmdcfw, eosjmdcsw, eosjmdckfw, 
     +eosjmdcksw, eosjmdckp
      REAL*8 eosmdjwfnum(0:11), eosmdjwfden(0:12)
      COMMON /parm_eos_mdjwf/ eosmdjwfnum, eosmdjwfden
      REAL*8 teos(48)
      COMMON /parm_teos10/ teos
      INTEGER dumpadrecmn
      INTEGER dumpadrecdy
      INTEGER dumpadrecsi
      COMMON /autodiff_dump_ad_rec/ dumpadrecmn, dumpadrecdy, 
     +dumpadrecsi
      INTEGER ndv3d, ndv2d, nexf1, nexf2, nctrl1, nob, nsi
      PARAMETER (ndv3d=10)
      PARAMETER (ndv2d=23)
      PARAMETER (nexf1=21)
      PARAMETER (nexf2=20)
      PARAMETER (nctrl1=20)
      PARAMETER (nob=20)
      PARAMETER (nsi=19)
      REAL*8 storedynvars3d(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy, 
     +       ndv3d)
      REAL*8 storedynvars3db(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy
     +       , ndv3d)
      REAL*8 storedynvars2d(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, 
     +       ndv2d)
      REAL*8 storedynvars2db(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, 
     +       ndv2d)
      REAL*8 storeexf1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, nexf1)
      REAL*8 storeexf2(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, nexf2)
      REAL*8 storectrls1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, nctrl1)
      REAL*8 storeobcsn(1-olx:snx+olx, nr, nsx, nsy, nob)
      REAL*8 storeobcss(1-olx:snx+olx, nr, nsx, nsy, nob)
      REAL*8 storeobcse(1-oly:sny+oly, nr, nsx, nsy, nob)
      REAL*8 storeobcsw(1-oly:sny+oly, nr, nsx, nsy, nob)
      REAL*8 storeseaice(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy, nsi)
      COMMON /autodiff_store_dyn/ storedynvars3d, storedynvars2d
      COMMON /autodiff_store_exf_flux/ storeexf1
      COMMON /autodiff_store_exf_atmos/ storeexf2
      COMMON /autodiff_store_ctrl/ storectrls1
      COMMON /autodiff_store_obcsn/ storeobcsn
      COMMON /autodiff_store_obcss/ storeobcss
      COMMON /autodiff_store_obcse/ storeobcse
      COMMON /autodiff_store_obcsw/ storeobcsw
      COMMON /autodiff_store_seaice/ storeseaice
C
      INTEGER enum_upwind_1rst
      PARAMETER (enum_upwind_1rst=1)
      INTEGER enum_centered_2nd
      PARAMETER (enum_centered_2nd=2)
      INTEGER enum_upwind_3rd
      PARAMETER (enum_upwind_3rd=3)
      INTEGER enum_centered_4th
      PARAMETER (enum_centered_4th=4)
      INTEGER enum_dst2
      PARAMETER (enum_dst2=20)
      INTEGER enum_flux_limit
      PARAMETER (enum_flux_limit=77)
      INTEGER enum_dst3
      PARAMETER (enum_dst3=30)
      INTEGER enum_dst3_flux_limit
      PARAMETER (enum_dst3_flux_limit=33)
      INTEGER enum_os7mp
      PARAMETER (enum_os7mp=7)
      INTEGER enum_som_prather
      PARAMETER (enum_som_prather=80)
      INTEGER enum_som_limiter
      PARAMETER (enum_som_limiter=81)
      INTEGER gad_scheme_maxnum
      PARAMETER (gad_scheme_maxnum=100)
      INTEGER nsom
      PARAMETER (nsom=3+6)
      REAL*8 onesixth
      PARAMETER (onesixth=1.d0/6.d0)
      INTEGER iminadvr, imaxadvr, jminadvr, jmaxadvr
      PARAMETER (iminadvr=1, imaxadvr=snx)
      PARAMETER (jminadvr=1, jmaxadvr=sny)
C
      INTEGER gad_temperature
      PARAMETER (gad_temperature=1)
      INTEGER gad_salinity
      PARAMETER (gad_salinity=2)
      INTEGER gad_tr1
      PARAMETER (gad_tr1=3)
      CHARACTER*2 somsfx(nsom)
      COMMON /gad_parm_c/ somsfx
      INTEGER gad_olminsize(3)
      COMMON /gad_parm_i/ gad_olminsize
      LOGICAL tempsom_advection
      LOGICAL saltsom_advection
      LOGICAL tempmultidimadvec
      LOGICAL saltmultidimadvec
      LOGICAL adamsbashforthgt
      LOGICAL adamsbashforthgs
      LOGICAL adamsbashforth_t
      LOGICAL adamsbashforth_s
      COMMON /gad_parm_l/ tempsom_advection, saltsom_advection, 
     +tempmultidimadvec, saltmultidimadvec, adamsbashforthgt, 
     +adamsbashforthgs, adamsbashforth_t, adamsbashforth_s
      REAL*8 smolarkiewiczmaxfrac
      COMMON /gad_smol/ smolarkiewiczmaxfrac
      REAL*8 uveld(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 uveldb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vveld(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vveldb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 etanm1(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 etanm1b(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 unm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 unm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vnm1(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vnm1b(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /dynvars_cd/ uveld, vveld, etanm1, unm1, vnm1
C
C
C
C
C
      INTEGER nyears_chkpt
      INTEGER nmonths_chkpt
      INTEGER ndays_chkpt
      INTEGER ngeom_chkpt
      INTEGER ncheck_chkpt
      INTEGER nthreads_chkpt
      PARAMETER (nyears_chkpt=1)
      PARAMETER (nmonths_chkpt=12)
      PARAMETER (ndays_chkpt=31)
      PARAMETER (ngeom_chkpt=nr*nsx*nsy)
      PARAMETER (ncheck_chkpt=6)
      PARAMETER (nthreads_chkpt=1)
      INTEGER nchklev_1
      PARAMETER (nchklev_1=30)
      INTEGER nchklev_2
      PARAMETER (nchklev_2=5)
      INTEGER nchklev_3
      PARAMETER (nchklev_3=5)
      INTEGER ikey_dynamics
      INTEGER ikey_yearly
      INTEGER ikey_daily_1
      INTEGER ikey_daily_2
      INTEGER iloop_daily
C
C
C
      COMMON /tamc_keys_i/ ikey_dynamics, ikey_yearly, ikey_daily_1, 
     +ikey_daily_2, iloop_daily
      INTEGER isbyte
      PARAMETER (isbyte=8)
      INTEGER maximpl
      PARAMETER (maximpl=6)
      INTEGER maxpass
      PARAMETER (maxpass=2)
      INTEGER maxcube
      PARAMETER (maxcube=1)
C
C
C
C
      INTEGER maxcvars
      PARAMETER (maxcvars=60)
      INTEGER ctrlprec
      PARAMETER (ctrlprec=64)
      REAL*8 delzexp
      REAL*8 forcingprecond
      COMMON /controlparams_r/ delzexp, forcingprecond
      LOGICAL ctrlsmoothcorrel2d, ctrlsmoothcorrel3d
      LOGICAL ctrlusegen
      LOGICAL doinitxx
      LOGICAL doadmtlm
      LOGICAL dopackdiag
      LOGICAL dozscaleunpack
      LOGICAL dozscalepack
      LOGICAL domainunpack
      LOGICAL domainpack
      LOGICAL dosingleprectapelev
      LOGICAL doadmtlmbypassad
      COMMON /controlvars_l/ ctrlsmoothcorrel2d, ctrlsmoothcorrel3d, 
     +ctrlusegen, doinitxx, doadmtlm, dopackdiag, dozscaleunpack, 
     +dozscalepack, domainunpack, domainpack, dosingleprectapelev, 
     +doadmtlmbypassad
      INTEGER nvartype
      INTEGER nvarlength
      INTEGER ncvarindex(maxcvars)
      INTEGER ncvarrecs(maxcvars)
      INTEGER ncvarrecstart(maxcvars)
      INTEGER ncvarrecsend(maxcvars)
      INTEGER ncvarxmax(maxcvars)
      INTEGER ncvarymax(maxcvars)
      INTEGER ncvarnrmax(maxcvars)
      INTEGER nwetctile(nsx, nsy, nr)
      INTEGER nwetstile(nsx, nsy, nr)
      INTEGER nwetwtile(nsx, nsy, nr)
      INTEGER nwetvtile(nsx, nsy, nr)
      INTEGER nwetcglobal(nr)
      INTEGER nwetsglobal(nr)
      INTEGER nwetwglobal(nr)
      INTEGER nwetvglobal(nr)
      INTEGER nbuffglobal
      COMMON /controlvars_i/ nvartype, nvarlength, ncvarindex, ncvarrecs
     +, ncvarrecstart, ncvarrecsend, ncvarxmax, ncvarymax, ncvarnrmax, 
     +nwetctile, nwetstile, nwetwtile, nwetvtile, nwetcglobal, 
     +nwetsglobal, nwetwglobal, nwetvglobal, nbuffglobal
      CHARACTER*1 ncvargrd(maxcvars)
      CHARACTER*2 yadprefix
      COMMON /controlvars_c/ ncvargrd, yadprefix
      INTEGER filenvartype
      INTEGER filenvarlength
      INTEGER fileoptimcycle
      INTEGER filencbuffindex
      INTEGER fileig
      INTEGER filejg
      INTEGER filei
      INTEGER filej
      INTEGER filensx
      INTEGER filensy
      INTEGER filek
      INTEGER filenwetcglobal(nr)
      INTEGER filenwetsglobal(nr)
      INTEGER filenwetwglobal(nr)
      INTEGER filenwetvglobal(nr)
      INTEGER filencvarindex(maxcvars)
      INTEGER filencvarrecs(maxcvars)
      INTEGER filencvarxmax(maxcvars)
      INTEGER filencvarymax(maxcvars)
      INTEGER filencvarnrmax(maxcvars)
      COMMON /controlvec_header_i/ filenvartype, filenvarlength, 
     +fileoptimcycle, filencbuffindex, fileig, filejg, filei, filej, 
     +filensx, filensy, filek, filenwetcglobal, filenwetsglobal, 
     +filenwetwglobal, filenwetvglobal, filencvarindex, filencvarrecs, 
     +filencvarxmax, filencvarymax, filencvarnrmax
      REAL*8 filefc
      COMMON /controlvec_header_r/ filefc
      CHARACTER*10 fileyctrlid
      CHARACTER*1 filencvargrd(maxcvars)
      COMMON /controlvec_header_c/ fileyctrlid, filencvargrd
      REAL*8 wunit(nr, nsx, nsy)
      REAL*8 wareaunit(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ctrl_weights_unit_r/ wunit, wareaunit
      CHARACTER*2 yadmark
      CHARACTER*9 ctrlname
      CHARACTER*9 costname
      CHARACTER*9 scalname
      CHARACTER*9 maskname
      CHARACTER*9 metaname
      CHARACTER*10 yctrlid
      CHARACTER*4 yctrlposunpack
      CHARACTER*4 yctrlpospack
      COMMON /packnames_c/ yadmark, ctrlname, costname, scalname, 
     +maskname, metaname, yctrlid, yctrlposunpack, yctrlpospack
      REAL*8 whflux(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wsflux(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wtauu(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wtauv(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 watemp(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 waqh(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wprecip(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wswflux(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wswdown(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wuwind(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wvwind(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wlwflux(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wlwdown(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wevap(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wsnowprecip(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wapressure(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wrunoff(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wsst(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 wsss(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      COMMON /ctrl_weights_atmos_r/ whflux, wsflux, wtauu, wtauv, watemp
     +, waqh, wprecip, wswflux, wswdown, wuwind, wvwind, wlwflux, 
     +wlwdown, wevap, wsnowprecip, wapressure, wrunoff, wsst, wsss
CHFLUXM_CONTROL
CHFLUXM_CONTROL
      CHARACTER*(max_len_fnam) xx_theta_file
      CHARACTER*(max_len_fnam) xx_salt_file
      CHARACTER*(max_len_fnam) xx_hflux_file
      CHARACTER*(max_len_fnam) xx_sflux_file
      CHARACTER*(max_len_fnam) xx_tauu_file
      CHARACTER*(max_len_fnam) xx_tauv_file
      CHARACTER*(max_len_fnam) xx_atemp_file
      CHARACTER*(max_len_fnam) xx_aqh_file
      CHARACTER*(max_len_fnam) xx_precip_file
      CHARACTER*(max_len_fnam) xx_swflux_file
      CHARACTER*(max_len_fnam) xx_swdown_file
      CHARACTER*(max_len_fnam) xx_lwflux_file
      CHARACTER*(max_len_fnam) xx_lwdown_file
      CHARACTER*(max_len_fnam) xx_evap_file
      CHARACTER*(max_len_fnam) xx_snowprecip_file
      CHARACTER*(max_len_fnam) xx_apressure_file
      CHARACTER*(max_len_fnam) xx_runoff_file
      CHARACTER*(max_len_fnam) xx_uwind_file
      CHARACTER*(max_len_fnam) xx_vwind_file
      CHARACTER*(max_len_fnam) xx_atemp_mean_file
      CHARACTER*(max_len_fnam) xx_aqh_mean_file
      CHARACTER*(max_len_fnam) xx_precip_mean_file
      CHARACTER*(max_len_fnam) xx_swdown_mean_file
      CHARACTER*(max_len_fnam) xx_uwind_mean_file
      CHARACTER*(max_len_fnam) xx_vwind_mean_file
      CHARACTER*(max_len_fnam) xx_diffkr_file
      CHARACTER*(max_len_fnam) xx_kapgm_file
      CHARACTER*(max_len_fnam) xx_kapredi_file
      CHARACTER*(max_len_fnam) xx_tr1_file
      CHARACTER*(max_len_fnam) xx_sst_file
      CHARACTER*(max_len_fnam) xx_sss_file
      CHARACTER*(max_len_fnam) xx_depth_file
      CHARACTER*(max_len_fnam) xx_efluxy_file
      CHARACTER*(max_len_fnam) xx_efluxp_file
      CHARACTER*(max_len_fnam) xx_bottomdrag_file
      CHARACTER*(max_len_fnam) xx_edtaux_file
      CHARACTER*(max_len_fnam) xx_edtauy_file
      CHARACTER*(max_len_fnam) xx_uvel_file
      CHARACTER*(max_len_fnam) xx_vvel_file
      CHARACTER*(max_len_fnam) xx_etan_file
      CHARACTER*(max_len_fnam) xx_relaxsst_file
      CHARACTER*(max_len_fnam) xx_relaxsss_file
      CHARACTER*(max_len_fnam) xx_theta_ini_fin_file
      CHARACTER*(max_len_fnam) xx_salt_ini_fin_file
      CHARACTER*(max_len_fnam) xx_siarea_file
      CHARACTER*(max_len_fnam) xx_siheff_file
      CHARACTER*(max_len_fnam) xx_sihsnow_file
      CHARACTER*(max_len_fnam) xx_gen2d_file
      CHARACTER*(max_len_fnam) xx_gen3d_file
CHFLUXM_CONTROL
      CHARACTER*(max_len_fnam) xx_hfluxm_file
CHFLUXM_CONTROL
      CHARACTER*(max_len_fnam) xx_shifwflx_file
C
C
C
      COMMON /controlfiles_c/ xx_theta_file, xx_salt_file, xx_hflux_file
     +, xx_sflux_file, xx_tauu_file, xx_tauv_file, xx_atemp_file, 
     +xx_aqh_file, xx_precip_file, xx_swflux_file, xx_swdown_file, 
     +xx_lwflux_file, xx_lwdown_file, xx_evap_file, xx_snowprecip_file, 
     +xx_apressure_file, xx_runoff_file, xx_uwind_file, xx_vwind_file, 
     +xx_atemp_mean_file, xx_aqh_mean_file, xx_precip_mean_file, 
     +xx_swdown_mean_file, xx_uwind_mean_file, xx_vwind_mean_file, 
     +xx_diffkr_file, xx_kapgm_file, xx_kapredi_file, xx_tr1_file, 
     +xx_sst_file, xx_sss_file, xx_depth_file, xx_efluxy_file, 
     +xx_efluxp_file, xx_bottomdrag_file, xx_edtaux_file, xx_edtauy_file
     +, xx_uvel_file, xx_vvel_file, xx_etan_file, xx_relaxsst_file, 
     +xx_relaxsss_file, xx_theta_ini_fin_file, xx_salt_ini_fin_file, 
     +xx_siarea_file, xx_siheff_file, xx_sihsnow_file, xx_gen2d_file, 
     +xx_gen3d_file, xx_hfluxm_file, xx_shifwflx_file
      REAL*8 xx_hfluxperiod
      REAL*8 xx_sfluxperiod
      REAL*8 xx_tauuperiod
      REAL*8 xx_tauvperiod
      REAL*8 xx_atempperiod
      REAL*8 xx_aqhperiod
      REAL*8 xx_precipperiod
      REAL*8 xx_swfluxperiod
      REAL*8 xx_swdownperiod
      REAL*8 xx_lwfluxperiod
      REAL*8 xx_lwdownperiod
      REAL*8 xx_evapperiod
      REAL*8 xx_snowprecipperiod
      REAL*8 xx_apressureperiod
      REAL*8 xx_runoffperiod
      REAL*8 xx_uwindperiod
      REAL*8 xx_vwindperiod
      REAL*8 xx_sstperiod
      REAL*8 xx_sssperiod
      REAL*8 xx_shifwflxperiod
C
C
      COMMON /controltimes_r/ xx_hfluxperiod, xx_sfluxperiod, 
     +xx_tauuperiod, xx_tauvperiod, xx_atempperiod, xx_aqhperiod, 
     +xx_precipperiod, xx_swfluxperiod, xx_swdownperiod, xx_lwfluxperiod
     +, xx_lwdownperiod, xx_evapperiod, xx_snowprecipperiod, 
     +xx_apressureperiod, xx_runoffperiod, xx_uwindperiod, 
     +xx_vwindperiod, xx_sstperiod, xx_sssperiod, xx_shifwflxperiod
      REAL*8 xx_hflux_remo_intercept, xx_hflux_remo_slope
      REAL*8 xx_sflux_remo_intercept, xx_sflux_remo_slope
      REAL*8 xx_tauu_remo_intercept, xx_tauu_remo_slope
      REAL*8 xx_tauv_remo_intercept, xx_tauv_remo_slope
      REAL*8 xx_atemp_remo_intercept, xx_atemp_remo_slope
      REAL*8 xx_aqh_remo_intercept, xx_aqh_remo_slope
      REAL*8 xx_precip_remo_intercept, xx_precip_remo_slope
      REAL*8 xx_swflux_remo_intercept, xx_swflux_remo_slope
      REAL*8 xx_swdown_remo_intercept, xx_swdown_remo_slope
      REAL*8 xx_lwflux_remo_intercept, xx_lwflux_remo_slope
      REAL*8 xx_lwdown_remo_intercept, xx_lwdown_remo_slope
      REAL*8 xx_evap_remo_intercept, xx_evap_remo_slope
      REAL*8 xx_snowprecip_remo_intercept
      REAL*8 xx_snowprecip_remo_slope
      REAL*8 xx_apressure_remo_intercept
      REAL*8 xx_apressure_remo_slope
      REAL*8 xx_sst_remo_intercept, xx_sst_remo_slope
      REAL*8 xx_sss_remo_intercept, xx_sss_remo_slope
      REAL*8 xx_runoff_remo_intercept, xx_runoff_remo_slope
      REAL*8 xx_uwind_remo_intercept, xx_uwind_remo_slope
      REAL*8 xx_vwind_remo_intercept, xx_vwind_remo_slope
      REAL*8 xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope
      COMMON /ctrl_param_trend_removal/ xx_hflux_remo_intercept, 
     +xx_hflux_remo_slope, xx_sflux_remo_intercept, xx_sflux_remo_slope
     +, xx_tauu_remo_intercept, xx_tauu_remo_slope, 
     +xx_tauv_remo_intercept, xx_tauv_remo_slope, 
     +xx_atemp_remo_intercept, xx_atemp_remo_slope, 
     +xx_aqh_remo_intercept, xx_aqh_remo_slope, xx_precip_remo_intercept
     +, xx_precip_remo_slope, xx_swflux_remo_intercept, 
     +xx_swflux_remo_slope, xx_swdown_remo_intercept, 
     +xx_swdown_remo_slope, xx_lwflux_remo_intercept, 
     +xx_lwflux_remo_slope, xx_lwdown_remo_intercept, 
     +xx_lwdown_remo_slope, xx_evap_remo_intercept, xx_evap_remo_slope, 
     +xx_snowprecip_remo_intercept, xx_snowprecip_remo_slope, 
     +xx_apressure_remo_intercept, xx_apressure_remo_slope, 
     +xx_sst_remo_intercept, xx_sst_remo_slope, xx_sss_remo_intercept, 
     +xx_sss_remo_slope, xx_runoff_remo_intercept, xx_runoff_remo_slope
     +, xx_uwind_remo_intercept, xx_uwind_remo_slope, 
     +xx_vwind_remo_intercept, xx_vwind_remo_slope, 
     +xx_shifwflx_remo_intercept, xx_shifwflx_remo_slope
      INTEGER xx_hfluxstartdate1
      INTEGER xx_hfluxstartdate2
      INTEGER xx_sfluxstartdate1
      INTEGER xx_sfluxstartdate2
      INTEGER xx_tauustartdate1
      INTEGER xx_tauustartdate2
      INTEGER xx_tauvstartdate1
      INTEGER xx_tauvstartdate2
      INTEGER xx_atempstartdate1
      INTEGER xx_atempstartdate2
      INTEGER xx_aqhstartdate1
      INTEGER xx_aqhstartdate2
      INTEGER xx_precipstartdate1
      INTEGER xx_precipstartdate2
      INTEGER xx_swfluxstartdate1
      INTEGER xx_swfluxstartdate2
      INTEGER xx_swdownstartdate1
      INTEGER xx_swdownstartdate2
      INTEGER xx_snowprecipstartdate1
      INTEGER xx_snowprecipstartdate2
      INTEGER xx_lwfluxstartdate1
      INTEGER xx_lwfluxstartdate2
      INTEGER xx_lwdownstartdate1
      INTEGER xx_lwdownstartdate2
      INTEGER xx_evapstartdate1
      INTEGER xx_evapstartdate2
      INTEGER xx_apressurestartdate1
      INTEGER xx_apressurestartdate2
      INTEGER xx_runoffstartdate1
      INTEGER xx_runoffstartdate2
      INTEGER xx_uwindstartdate1
      INTEGER xx_uwindstartdate2
      INTEGER xx_vwindstartdate1
      INTEGER xx_vwindstartdate2
      INTEGER xx_sststartdate1
      INTEGER xx_sststartdate2
      INTEGER xx_sssstartdate1
      INTEGER xx_sssstartdate2
      INTEGER xx_shifwflxstartdate1
      INTEGER xx_shifwflxstartdate2
      INTEGER xx_hfluxstartdate(4)
      INTEGER xx_sfluxstartdate(4)
      INTEGER xx_tauustartdate(4)
      INTEGER xx_tauvstartdate(4)
      INTEGER xx_atempstartdate(4)
      INTEGER xx_aqhstartdate(4)
      INTEGER xx_precipstartdate(4)
      INTEGER xx_swfluxstartdate(4)
      INTEGER xx_swdownstartdate(4)
      INTEGER xx_snowprecipstartdate(4)
      INTEGER xx_lwfluxstartdate(4)
      INTEGER xx_lwdownstartdate(4)
      INTEGER xx_evapstartdate(4)
      INTEGER xx_apressurestartdate(4)
      INTEGER xx_runoffstartdate(4)
      INTEGER xx_uwindstartdate(4)
      INTEGER xx_vwindstartdate(4)
      INTEGER xx_sststartdate(4)
      INTEGER xx_sssstartdate(4)
      INTEGER xx_shifwflxstartdate(4)
      COMMON /controltimes_i/ xx_hfluxstartdate1, xx_hfluxstartdate2, 
     +xx_sfluxstartdate1, xx_sfluxstartdate2, xx_tauustartdate1, 
     +xx_tauustartdate2, xx_tauvstartdate1, xx_tauvstartdate2, 
     +xx_atempstartdate1, xx_atempstartdate2, xx_aqhstartdate1, 
     +xx_aqhstartdate2, xx_precipstartdate1, xx_precipstartdate2, 
     +xx_swfluxstartdate1, xx_swfluxstartdate2, xx_swdownstartdate1, 
     +xx_swdownstartdate2, xx_snowprecipstartdate1, 
     +xx_snowprecipstartdate2, xx_lwfluxstartdate1, xx_lwfluxstartdate2
     +, xx_lwdownstartdate1, xx_lwdownstartdate2, xx_evapstartdate1, 
     +xx_evapstartdate2, xx_apressurestartdate1, xx_apressurestartdate2
     +, xx_runoffstartdate1, xx_runoffstartdate2, xx_uwindstartdate1, 
     +xx_uwindstartdate2, xx_vwindstartdate1, xx_vwindstartdate2, 
     +xx_sststartdate1, xx_sststartdate2, xx_sssstartdate1, 
     +xx_sssstartdate2, xx_hfluxstartdate, xx_sfluxstartdate, 
     +xx_tauustartdate, xx_tauvstartdate, xx_atempstartdate, 
     +xx_aqhstartdate, xx_precipstartdate, xx_swfluxstartdate, 
     +xx_swdownstartdate, xx_uwindstartdate, xx_snowprecipstartdate, 
     +xx_lwfluxstartdate, xx_lwdownstartdate, xx_evapstartdate, 
     +xx_apressurestartdate, xx_runoffstartdate, xx_vwindstartdate, 
     +xx_sststartdate, xx_sssstartdate, xx_shifwflxstartdate1, 
     +xx_shifwflxstartdate2, xx_shifwflxstartdate
CHFLUXM_CONTROL
      REAL*8 xx_theta_dummy
      REAL*8 xx_salt_dummy
      REAL*8 xx_hflux_dummy
      REAL*8 xx_sflux_dummy
      REAL*8 xx_tauu_dummy
      REAL*8 xx_tauv_dummy
      REAL*8 xx_atemp_dummy
      REAL*8 xx_aqh_dummy
      REAL*8 xx_precip_dummy
      REAL*8 xx_swflux_dummy
      REAL*8 xx_swdown_dummy
      REAL*8 xx_snowprecip_dummy
      REAL*8 xx_lwflux_dummy
      REAL*8 xx_lwdown_dummy
      REAL*8 xx_evap_dummy
      REAL*8 xx_apressure_dummy
      REAL*8 xx_runoff_dummy
      REAL*8 xx_uwind_dummy
      REAL*8 xx_vwind_dummy
      REAL*8 xx_diffkr_dummy
      REAL*8 xx_kapgm_dummy
      REAL*8 xx_kapredi_dummy
      REAL*8 xx_tr1_dummy
      REAL*8 xx_sst_dummy
      REAL*8 xx_sss_dummy
      REAL*8 xx_depth_dummy
      REAL*8 xx_efluxy_dummy
      REAL*8 xx_efluxp_dummy
      REAL*8 xx_bottomdrag_dummy
      REAL*8 xx_edtaux_dummy
      REAL*8 xx_edtauy_dummy
      REAL*8 xx_uvel_dummy
      REAL*8 xx_vvel_dummy
      REAL*8 xx_etan_dummy
      REAL*8 xx_siarea_dummy
      REAL*8 xx_siheff_dummy
      REAL*8 xx_sihsnow_dummy
      REAL*8 xx_relaxsst_dummy
      REAL*8 xx_relaxsss_dummy
      REAL*8 xx_gen2d_dummy
      REAL*8 xx_gen3d_dummy
C
      REAL*8 xx_tbar_mean_dummy
      REAL*8 xx_tbar_daily_mean_dummy
      REAL*8 xx_sbar_mean_dummy
      REAL*8 xx_sbar_daily_mean_dummy
      REAL*8 xx_ubar_mean_dummy
      REAL*8 xx_vbar_mean_dummy
      REAL*8 xx_wbar_mean_dummy
      REAL*8 xx_psbar_mean_dummy
      REAL*8 xx_bpbar_mean_dummy
      REAL*8 xx_hflux_mean_dummy
      REAL*8 xx_sflux_mean_dummy
      REAL*8 xx_sstbar_mean_dummy
      REAL*8 xx_sssbar_mean_dummy
      REAL*8 xx_atmfwbar_mean_dummy
      REAL*8 xx_taux_mean_dummy
      REAL*8 xx_tauy_mean_dummy
      REAL*8 xx_atemp_mean_dummy
      REAL*8 xx_aqh_mean_dummy
      REAL*8 xx_precip_mean_dummy
      REAL*8 xx_swflux_mean_dummy
      REAL*8 xx_swdown_mean_dummy
      REAL*8 xx_snowprecip_mean_dummy
      REAL*8 xx_lwflux_mean_dummy
      REAL*8 xx_lwdown_mean_dummy
      REAL*8 xx_evap_mean_dummy
      REAL*8 xx_apressure_mean_dummy
      REAL*8 xx_runoff_mean_dummy
      REAL*8 xx_uwind_mean_dummy
      REAL*8 xx_vwind_mean_dummy
      REAL*8 xx_theta_ini_fin_dummy
      REAL*8 xx_salt_ini_fin_dummy
      REAL*8 xx_smrareabar_mean_dummy
      REAL*8 xx_smrsstbar_mean_dummy
      REAL*8 xx_smrsssbar_mean_dummy
      REAL*8 xx_iestaubar_mean_dummy
CHFLUXM_CONTROL
      REAL*8 xx_hfluxm_dummy(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 xx_hfluxm_dummyb(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
CHFLUXM_CONTROL
      REAL*8 xx_shifwflx_dummy
C
C
      COMMON /ctrl_dummy/ xx_theta_dummy, xx_salt_dummy, xx_hflux_dummy
     +, xx_sflux_dummy, xx_tauu_dummy, xx_tauv_dummy, xx_atemp_dummy, 
     +xx_aqh_dummy, xx_precip_dummy, xx_swflux_dummy, xx_swdown_dummy, 
     +xx_snowprecip_dummy, xx_lwflux_dummy, xx_lwdown_dummy, 
     +xx_evap_dummy, xx_apressure_dummy, xx_runoff_dummy, xx_uwind_dummy
     +, xx_vwind_dummy, xx_diffkr_dummy, xx_kapgm_dummy, 
     +xx_kapredi_dummy, xx_tr1_dummy, xx_sst_dummy, xx_sss_dummy, 
     +xx_depth_dummy, xx_efluxy_dummy, xx_efluxp_dummy, 
     +xx_bottomdrag_dummy, xx_edtaux_dummy, xx_edtauy_dummy, 
     +xx_uvel_dummy, xx_vvel_dummy, xx_etan_dummy, xx_siarea_dummy, 
     +xx_siheff_dummy, xx_sihsnow_dummy, xx_relaxsst_dummy, 
     +xx_relaxsss_dummy, xx_tbar_mean_dummy, xx_tbar_daily_mean_dummy, 
     +xx_sbar_mean_dummy, xx_sbar_daily_mean_dummy, xx_ubar_mean_dummy, 
     +xx_vbar_mean_dummy, xx_wbar_mean_dummy, xx_psbar_mean_dummy, 
     +xx_bpbar_mean_dummy, xx_taux_mean_dummy, xx_tauy_mean_dummy, 
     +xx_hflux_mean_dummy, xx_sflux_mean_dummy, xx_sstbar_mean_dummy, 
     +xx_sssbar_mean_dummy, xx_atmfwbar_mean_dummy, xx_atemp_mean_dummy
     +, xx_aqh_mean_dummy, xx_precip_mean_dummy, xx_swflux_mean_dummy, 
     +xx_swdown_mean_dummy, xx_snowprecip_mean_dummy, 
     +xx_lwflux_mean_dummy, xx_lwdown_mean_dummy, xx_evap_mean_dummy, 
     +xx_apressure_mean_dummy, xx_runoff_mean_dummy, xx_uwind_mean_dummy
     +, xx_vwind_mean_dummy, xx_theta_ini_fin_dummy, 
     +xx_salt_ini_fin_dummy, xx_smrareabar_mean_dummy, 
     +xx_smrsstbar_mean_dummy, xx_smrsssbar_mean_dummy, 
     +xx_iestaubar_mean_dummy, xx_gen2d_dummy, xx_gen3d_dummy, 
     +xx_hfluxm_dummy, xx_shifwflx_dummy
      INTEGER maxctrlarr2d, maxctrlarr3d, maxctrltim2d
      PARAMETER (maxctrlarr2d=1, maxctrlarr3d=1, maxctrltim2d=1)
      REAL*8 xx_genarr2d_dummy(maxctrlarr2d)
      REAL*8 xx_genarr3d_dummy(maxctrlarr3d)
      REAL*8 xx_gentim2d_dummy(maxctrltim2d)
      COMMON /ctrl_dummy_arr/ xx_genarr2d_dummy, xx_genarr3d_dummy, 
     +xx_gentim2d_dummy
C
C
      REAL*8 objf_obcsn(nsx, nsy), objf_obcss(nsx, nsy)
      REAL*8 objf_obcsw(nsx, nsy), objf_obcse(nsx, nsy)
      REAL*8 objf_obcsvol, objf_ageos(nsx, nsy)
      REAL*8 mult_obcsn, mult_obcss
      REAL*8 mult_obcsw, mult_obcse
      REAL*8 mult_obcsvol, mult_ageos
      REAL*8 num_obcsn(nsx, nsy), num_obcss(nsx, nsy)
      REAL*8 num_obcsw(nsx, nsy), num_obcse(nsx, nsy)
      REAL*8 num_obcsvol, num_ageos(nsx, nsy)
      COMMON /ecco_cost_weights_obcs/ objf_obcsn, objf_obcss, objf_obcsw
     +, objf_obcse, objf_obcsvol, objf_ageos, mult_obcsn, mult_obcss, 
     +mult_obcsw, mult_obcse, mult_obcsvol, mult_ageos, num_obcsn, 
     +num_obcss, num_obcsw, num_obcse, num_obcsvol, num_ageos
      REAL*8 modesv(nr, nr, nr)
      COMMON /ih_modes/ modesv
      REAL*8 xx_obcsn_dummy
      REAL*8 xx_obcss_dummy
      REAL*8 xx_obcsw_dummy
      REAL*8 xx_obcse_dummy
      COMMON /ctrl_dummy_obcs/ xx_obcsn_dummy, xx_obcss_dummy, 
     +xx_obcsw_dummy, xx_obcse_dummy
      CHARACTER*(max_len_fnam) xx_obcsn_file
      CHARACTER*(max_len_fnam) xx_obcss_file
      CHARACTER*(max_len_fnam) xx_obcsw_file
      CHARACTER*(max_len_fnam) xx_obcse_file
      COMMON /controlfiles_c_obcs/ xx_obcsn_file, xx_obcss_file, 
     +xx_obcsw_file, xx_obcse_file
      REAL*8 xx_obcsnperiod
      REAL*8 xx_obcssperiod
      REAL*8 xx_obcswperiod
      REAL*8 xx_obcseperiod
      COMMON /controltimes_r_obcs/ xx_obcsnperiod, xx_obcssperiod, 
     +xx_obcswperiod, xx_obcseperiod
      INTEGER xx_obcsnstartdate1
      INTEGER xx_obcsnstartdate2
      INTEGER xx_obcssstartdate1
      INTEGER xx_obcssstartdate2
      INTEGER xx_obcswstartdate1
      INTEGER xx_obcswstartdate2
      INTEGER xx_obcsestartdate1
      INTEGER xx_obcsestartdate2
      INTEGER xx_obcsnstartdate(4)
      INTEGER xx_obcssstartdate(4)
      INTEGER xx_obcswstartdate(4)
      INTEGER xx_obcsestartdate(4)
      COMMON /controltimes_i_obcs/ xx_obcsnstartdate1, 
     +xx_obcsnstartdate2, xx_obcssstartdate1, xx_obcssstartdate2, 
     +xx_obcswstartdate1, xx_obcswstartdate2, xx_obcsestartdate1, 
     +xx_obcsestartdate2, xx_obcsnstartdate, xx_obcssstartdate, 
     +xx_obcswstartdate, xx_obcsestartdate
      INTEGER nwetobcsn(nsx, nsy, nr, nobcs)
      INTEGER nwetobcsnglo(nr, nobcs)
      COMMON /controlvars_i_obcsn/ nwetobcsn, nwetobcsnglo
      INTEGER nwetobcss(nsx, nsy, nr, nobcs)
      INTEGER nwetobcssglo(nr, nobcs)
      COMMON /controlvars_i_obcss/ nwetobcss, nwetobcssglo
      INTEGER nwetobcsw(nsx, nsy, nr, nobcs)
      INTEGER nwetobcswglo(nr, nobcs)
      COMMON /controlvars_i_obcsw/ nwetobcsw, nwetobcswglo
      INTEGER nwetobcse(nsx, nsy, nr, nobcs)
      INTEGER nwetobcseglo(nr, nobcs)
      COMMON /controlvars_i_obcse/ nwetobcse, nwetobcseglo
      REAL*8 fc
      REAL*8 fcb
      REAL*8 glofc
C
C
C
C
C
      COMMON /cost_r/ fc, glofc
      REAL*8 tile_fc(nsx, nsy)
      COMMON /cost_final_r/ tile_fc
CHFLUXM_CONTROL
CHFLUXM_CONTROL
      REAL*8 objf_atl(nsx, nsy)
      REAL*8 objf_test(nsx, nsy)
      REAL*8 objf_tracer(nsx, nsy)
      REAL*8 objf_entropy(nsx, nsy)
      REAL*8 objf_t_misfit(nsx, nsy)
      REAL*8 objf_eflux(nsx, nsy)
CHFLUXM_CONTROL
      REAL*8 objf_hflux_tut(nsx, nsy)
      REAL*8 objf_hflux_tutb(nsx, nsy)
      REAL*8 objf_temp_tut(nsx, nsy)
      REAL*8 objf_temp_tutb(nsx, nsy)
      COMMON /cost_objf/ objf_atl, objf_test, objf_tracer, objf_entropy
     +, objf_t_misfit, objf_eflux, objf_hflux_tut, objf_temp_tut
      REAL*8 lastinterval
CHFLUXM_CONTROL
      COMMON /cost_param_r/ lastinterval
CHFLUXM_CONTROL
CHFLUXM_CONTROL
      REAL*8 mult_atl
      REAL*8 mult_test
      REAL*8 mult_tracer
      REAL*8 mult_entropy
      REAL*8 mult_t_misfit
      REAL*8 mult_eflux
      REAL*8 multtheta
      REAL*8 multsalt
      REAL*8 multuvel
      REAL*8 multvvel
      REAL*8 multetan
CHFLUXM_CONTROL
      REAL*8 mult_hflux_tut
      REAL*8 mult_temp_tut
      COMMON /cost_aux_r/ mult_atl, mult_test, mult_tracer, mult_entropy
     +, mult_t_misfit, mult_eflux, multtheta, multsalt, multuvel, 
     +multvvel, multetan, mult_hflux_tut, mult_temp_tut
      REAL*8 cmeantheta(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 cmeanthetab(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 cmeanuvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 cmeanvvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 cmeanthetauvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 cmeanthetavvel(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
CHFLUXM_CONTROL
      COMMON /cost_mean_r/ cmeantheta, cmeanuvel, cmeanvvel, 
     +cmeanthetauvel, cmeanthetavvel
      INTEGER myiter
C
C
C Added globals for Tapenade Differentiation
C
      REAL*8 kwx(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kwxb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kwy(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kwyb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kwz(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kwzb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /gm_wtensor/ kwx, kwy, kwz
C
      REAL*8 kux(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kuxb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kvy(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kvyb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /gm_hortensor/ kux, kvy
C
      REAL*8 kuz(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kuzb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kvz(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 kvzb(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /gm_uvtensor/ kuz, kvz
C
      REAL*8 gm_timeave(nsx, nsy)
      REAL*8 gm_kwx_t(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gm_kwy_t(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 gm_kwz_t(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      COMMON /gm_tave_vars/ gm_timeave, gm_kwx_t, gm_kwy_t, gm_kwz_t
      REAL*8 ufluxtave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 vfluxtave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tfluxtave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 sfluxtave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 etatave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 uveltave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vveltave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 wveltave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 thetatave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 salttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 phihydlowtave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 uttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 wttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 ustave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vstave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 wstave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 eta2tave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 tttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 uutave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vvtave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 uvtave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 tdiffrtave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 uzetatave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 vzetatave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 phihydtave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy)
      REAL*8 phihydlow2tave(1-olx:snx+olx, 1-oly:sny+oly, nsx, nsy)
      REAL*8 convectcounttave(1-olx:snx+olx, 1-oly:sny+oly, nr, nsx, nsy
     +       )
C
C
C
      COMMON /tave_statevars/ ufluxtave, vfluxtave, tfluxtave, sfluxtave
     +, etatave, uveltave, vveltave, wveltave, thetatave, salttave, 
     +phihydlowtave, uttave, vttave, wttave, ustave, vstave, wstave, 
     +eta2tave, tttave, uutave, vvtave, uvtave, tdiffrtave, uzetatave, 
     +vzetatave, phihydtave, phihydlow2tave, convectcounttave
      EXTERNAL DEBUG_ENTER
      INTRINSIC NINT
      EXTERNAL DEBUG_CALL
      EXTERNAL TIMER_START
      EXTERNAL TIMER_STOP
      INTRINSIC FLOAT
      EXTERNAL BARRIER
      EXTERNAL DEBUG_LEAVE
C
C
C
C
C
      COMMON /gm_uvtensor_b/ kuzb, kvzb
      COMMON /gm_hortensor_b/ kuxb, kvyb
      COMMON /gm_wtensor_b/ kwxb, kwyb, kwzb
      COMMON /cost_mean_r_b/ cmeanthetab
      COMMON /cost_objf_b/ objf_hflux_tutb, objf_temp_tutb
      COMMON /cost_r_b/ fcb
      COMMON /ctrl_dummy_b/ xx_hfluxm_dummyb
      COMMON /dynvars_cd_b/ uveldb, vveldb, etanm1b, unm1b, vnm1b
      COMMON /autodiff_store_dyn_b/ storedynvars3db, storedynvars2db
      COMMON /surface_forcing_b/ surfaceforcingub, surfaceforcingvb, 
     +surfaceforcingtb, surfaceforcingsb, surfaceforcingticeb
      COMMON /tdfields_b/ taux0b, tauy0b, qnet0b, empmr0b, sst0b, sss0b
     +, taux1b, tauy1b, qnet1b, empmr1b, sst1b, sss1b, saltflux0b, 
     +saltflux1b
      COMMON /mean_qnet_b/ qnetmb
      COMMON /ffields_sss_b/ sssb
      COMMON /ffields_sst_b/ sstb
      COMMON /ffields_saltflux_b/ saltfluxb
      COMMON /ffields_empmr_b/ empmrb
      COMMON /ffields_qnet_b/ qnetb
      COMMON /ffields_fv_b/ fvb
      COMMON /ffields_fu_b/ fub
      COMMON /exact_eta_local_b/ detahdtb, pmeprb
      COMMON /surf_correc_b/ tsurfcorb, ssurfcorb
      COMMON /dynvars_diag_b/ totphihydb, rhoinsitub
      COMMON /dynvars_r_2_b/ etahb
      COMMON /dynvars_r_b/ etanb, uvelb, vvelb, wvelb, thetab, saltb, 
     +gub, gvb, gunm1b, gvnm1b, gtnm1b, gsnm1b
C fin du gros include des commons
