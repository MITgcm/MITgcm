.. _sub_phys_pkg_ptracers:

PTRACERS Package
----------------


Introduction
++++++++++++

This is a ''passive'' tracer package. Passive here means that the tracers
don’t affect the density of the water (as opposed to temperature and
salinity) so no not actively affect the physics of the ocean. Tracers
are initialized, advected, diffused and various outputs are taken care
of in this package. For methods to add additional sources and sinks of
tracers use the pkg/gchem (section [sec:pkg:gchem]).

Can use up tp 3843 tracers. But can not use pkg/diagnostics with more
than about 90 tracers. Use utils/matlab/ioLb2num.m and num2ioLb.m to
find correspondence between tracer number and tracer designation in the
code for more than 99 tracers (since tracers only have two digit
designations).

Equations
+++++++++

Key subroutines and parameters
++++++++++++++++++++++++++++++

The only code you should have to modify is: **PTRACERS\_SIZE.h** where
you need to set in the number of tracers to be used in the experiment:
PTRACERS\_num.

Run time parameters set in :code:`data.ptracers`:

- **PTRACERS\_Iter0** which is the integer timestep when the tracer experiment is initialized. If nIter0 :math:`=` PTRACERS\_Iter0 then the tracers are initialized to zero or from initial files. If nIter0 :math:`>` PTRACERS\_Iter0 then tracers (and previous timestep tendency terms) are read in from a the ptracers pickup file. Note that tracers of zeros will be carried around if nIter0 :math:`<` PTRACERS\_Iter0.
- **PTRACERS\_numInUse**: number of tracers to be used in the run (needs to be :math:`<=` PTRACERS\_num set in PTRACERS\_SIZE.h)
- **PTRACERS\_dumpFreq**: defaults to dumpFreq (set in data)
- **PTRACERS\_taveFreq**: defaults to taveFreq (set in data)
- **PTRACERS\_monitorFreq**: defaults to monitorFreq (set in data)
- **PTRACERS\_timeave\_mnc**: needs useMNC, timeave\_mnc, default to false
- **PTRACERS\_snapshot\_mnc**: needs useMNC, snapshot\_mnc, default to false
- **PTRACERS\_monitor\_mnc**: needs useMNC, monitor\_mnc, default to false
- **PTRACERS\_pickup\_write\_mnc**: needs useMNC, pickup\_write\_mnc, default to false
- **PTRACERS\_pickup\_read\_mnc**: needs useMNC, pickup\_read\_mnc, default to false
- **PTRACERS\_useRecords**: defaults to false. If true, will write all tracers in a single file, otherwise each tracer in a seperate file.

The following can be set for each tracer (tracer number iTrc):

- **PTRACERS\_advScheme(iTrc)** will default to saltAdvScheme (set in data). For other options see Table [tab:advectionShemes:sub:`s`\ ummary].
- **PTRACERS\_ImplVertAdv(iTrc)**: implicit vertical advection flag, default to .FALSE.
- **PTRACERS\_diffKh(iTrc)**: horizontal Laplacian Diffusivity, dafaults to diffKhS (set in data).
- **PTRACERS\_diffK4(iTrc)**: Biharmonic Diffusivity, defaults to diffK4S (set in data).
- **PTRACERS\_diffKr(iTrc)**: vertical diffusion, defaults to un-set.
- **PTRACERS\_diffKrNr(k,iTrc)**: level specific vertical diffusion, defaults to diffKrNrS. Will be set to PTRACERS\_diffKr if this is set.
- **PTRACERS\_ref(k,iTrc)**: reference tracer value for each level k, defaults to 0. Currently only used for dilution/concentration of tracers at surface if PTRACERS\_EvPrRn(iTrc) is set and convertFW2Salt (set in data) is set to something other than -1 (note default is convertFW2Salt=35).
- **PTRACERS\_EvPrRn(iTrc)**: tracer concentration in freshwater. Needed for calculation of dilution/concentration in surface layer due to freshwater addition/evaporation. Defaults to un-set in which case no dilution/concentration occurs.
- **PTRACERS\_useGMRedi(iTrc)**: apply GM or not. Defaults to useGMREdi.
- **PTRACERS\_useKPP(iTrc)**: apply KPP or not. Defaults to useKPP.
- **PTRACERS\_initialFile(iTrc)**: file with initial tracer concentration. Will be used if PTRACERS\_Iter0 :math:`=` nIter0. Default is no name, in which case tracer is initialised as zero. If PTRACERS\_Iter0 :math:`<` nIter0, then tracer concentration will come from pickup\_ptracer.
- **PTRACERS\_names(iTrc)**: tracer name. Needed for netcdf. Defaults to nothing.
- **PTRACERS\_long\_names(iTrc)**: optional name in long form of tracer.
- **PTRACERS\_units(iTrc)**: optional units of tracer.

PTRACERS Diagnostics
++++++++++++++++++++

Note that these will only work for 90 or less tracers (some problems
with the numbering/designation over this number)

::


    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    TRAC01  | 15 |SM P    MR      |mol C/m         |Mass-Weighted Dissolved Inorganic Carbon
    UTRAC01 | 15 |UU   171MR      |mol C/m.m/s     |Zonal Mass-Weighted Transp of Dissolved Inorganic Carbon
    VTRAC01 | 15 |VV   170MR      |mol C/m.m/s     |Merid Mass-Weighted Transp of Dissolved Inorganic Carbon
    WTRAC01 | 15 |WM      MR      |mol C/m.m/s     |Vert  Mass-Weighted Transp of Dissolved Inorganic Carbon
    ADVrTr01| 15 |WM      LR      |mol C/m.m^3/s   |Vertical   Advective Flux of Dissolved Inorganic Carbon
    ADVxTr01| 15 |UU   175MR      |mol C/m.m^3/s   |Zonal      Advective Flux of Dissolved Inorganic Carbon
    ADVyTr01| 15 |VV   174MR      |mol C/m.m^3/s   |Meridional Advective Flux of Dissolved Inorganic Carbon
    DFrETr01| 15 |WM      LR      |mol C/m.m^3/s   |Vertical Diffusive Flux of Dissolved Inorganic Carbon (Explicit part)
    DIFxTr01| 15 |UU   178MR      |mol C/m.m^3/s   |Zonal      Diffusive Flux of Dissolved Inorganic Carbon
    DIFyTr01| 15 |VV   177MR      |mol C/m.m^3/s   |Meridional Diffusive Flux of Dissolved Inorganic Carbon
    DFrITr01| 15 |WM      LR      |mol C/m.m^3/s   |Vertical Diffusive Flux of Dissolved Inorganic Carbon (Implicit part)
    TRAC02  | 15 |SM P    MR      |mol eq/         |Mass-Weighted Alkalinity
    UTRAC02 | 15 |UU   182MR      |mol eq/.m/s     |Zonal Mass-Weighted Transp of Alkalinity
    VTRAC02 | 15 |VV   181MR      |mol eq/.m/s     |Merid Mass-Weighted Transp of Alkalinity
    WTRAC02 | 15 |WM      MR      |mol eq/.m/s     |Vert  Mass-Weighted Transp of Alkalinity
    ADVrTr02| 15 |WM      LR      |mol eq/.m^3/s   |Vertical   Advective Flux of Alkalinity
    ADVxTr02| 15 |UU   186MR      |mol eq/.m^3/s   |Zonal      Advective Flux of Alkalinity
    ADVyTr02| 15 |VV   185MR      |mol eq/.m^3/s   |Meridional Advective Flux of Alkalinity
    DFrETr02| 15 |WM      LR      |mol eq/.m^3/s   |Vertical Diffusive Flux of Alkalinity (Explicit part)
    DIFxTr02| 15 |UU   189MR      |mol eq/.m^3/s   |Zonal      Diffusive Flux of Alkalinity
    DIFyTr02| 15 |VV   188MR      |mol eq/.m^3/s   |Meridional Diffusive Flux of Alkalinity
    DFrITr02| 15 |WM      LR      |mol eq/.m^3/s   |Vertical Diffusive Flux of Alkalinity (Implicit part)
    TRAC03  | 15 |SM P    MR      |mol P/m         |Mass-Weighted Phosphate
    UTRAC03 | 15 |UU   193MR      |mol P/m.m/s     |Zonal Mass-Weighted Transp of Phosphate
    VTRAC03 | 15 |VV   192MR      |mol P/m.m/s     |Merid Mass-Weighted Transp of Phosphate
    WTRAC03 | 15 |WM      MR      |mol P/m.m/s     |Vert  Mass-Weighted Transp of Phosphate
    ADVrTr03| 15 |WM      LR      |mol P/m.m^3/s   |Vertical   Advective Flux of Phosphate
    ADVxTr03| 15 |UU   197MR      |mol P/m.m^3/s   |Zonal      Advective Flux of Phosphate
    ADVyTr03| 15 |VV   196MR      |mol P/m.m^3/s   |Meridional Advective Flux of Phosphate
    DFrETr03| 15 |WM      LR      |mol P/m.m^3/s   |Vertical Diffusive Flux of Phosphate (Explicit part)
    DIFxTr03| 15 |UU   200MR      |mol P/m.m^3/s   |Zonal      Diffusive Flux of Phosphate
    ------------------------------------------------------------------------
    <-Name->|Levs|<-parsing code->|<--  Units   -->|<- Tile (max=80c) 
    ------------------------------------------------------------------------
    DIFyTr03| 15 |VV   199MR      |mol P/m.m^3/s   |Meridional Diffusive Flux of Phosphate
    DFrITr03| 15 |WM      LR      |mol P/m.m^3/s   |Vertical Diffusive Flux of Phosphate (Implicit part)
    TRAC04  | 15 |SM P    MR      |mol P/m         |Mass-Weighted Dissolved Organic Phosphorus
    UTRAC04 | 15 |UU   204MR      |mol P/m.m/s     |Zonal Mass-Weighted Transp of Dissolved Organic Phosphorus
    VTRAC04 | 15 |VV   203MR      |mol P/m.m/s     |Merid Mass-Weighted Transp of Dissolved Organic Phosphorus
    WTRAC04 | 15 |WM      MR      |mol P/m.m/s     |Vert  Mass-Weighted Transp of Dissolved Organic Phosphorus
    ADVrTr04| 15 |WM      LR      |mol P/m.m^3/s   |Vertical   Advective Flux of Dissolved Organic Phosphorus
    ADVxTr04| 15 |UU   208MR      |mol P/m.m^3/s   |Zonal      Advective Flux of Dissolved Organic Phosphorus
    ADVyTr04| 15 |VV   207MR      |mol P/m.m^3/s   |Meridional Advective Flux of Dissolved Organic Phosphorus
    DFrETr04| 15 |WM      LR      |mol P/m.m^3/s   |Vertical Diffusive Flux of Dissolved Organic Phosphorus (Explicit part)
    DIFxTr04| 15 |UU   211MR      |mol P/m.m^3/s   |Zonal      Diffusive Flux of Dissolved Organic Phosphorus
    DIFyTr04| 15 |VV   210MR      |mol P/m.m^3/s   |Meridional Diffusive Flux of Dissolved Organic Phosphorus
    DFrITr04| 15 |WM      LR      |mol P/m.m^3/s   |Vertical Diffusive Flux of Dissolved Organic Phosphorus (Implicit part)
    TRAC05  | 15 |SM P    MR      |mol O/m         |Mass-Weighted Dissolved Oxygen
    UTRAC05 | 15 |UU   215MR      |mol O/m.m/s     |Zonal Mass-Weighted Transp of Dissolved Oxygen
    VTRAC05 | 15 |VV   214MR      |mol O/m.m/s     |Merid Mass-Weighted Transp of Dissolved Oxygen
    WTRAC05 | 15 |WM      MR      |mol O/m.m/s     |Vert  Mass-Weighted Transp of Dissolved Oxygen
    ADVrTr05| 15 |WM      LR      |mol O/m.m^3/s   |Vertical   Advective Flux of Dissolved Oxygen
    ADVxTr05| 15 |UU   219MR      |mol O/m.m^3/s   |Zonal      Advective Flux of Dissolved Oxygen
    ADVyTr05| 15 |VV   218MR      |mol O/m.m^3/s   |Meridional Advective Flux of Dissolved Oxygen
    DFrETr05| 15 |WM      LR      |mol O/m.m^3/s   |Vertical Diffusive Flux of Dissolved Oxygen (Explicit part)
    DIFxTr05| 15 |UU   222MR      |mol O/m.m^3/s   |Zonal      Diffusive Flux of Dissolved Oxygen
    DIFyTr05| 15 |VV   221MR      |mol O/m.m^3/s   |Meridional Diffusive Flux of Dissolved Oxygen
    DFrITr05| 15 |WM      LR      |mol O/m.m^3/s   |Vertical Diffusive Flux of Dissolved Oxygen (Implicit part)

Do’s and Don’ts
+++++++++++++++

Reference Material
++++++++++++++++++

