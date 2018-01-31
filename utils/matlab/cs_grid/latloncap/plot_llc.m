% Make some color scale and vector plots for a gmt mesh
% Functions used -
% rdnctiles - to read in mnc generated tile files. these are read into a 
%             structure containing each tile separately.
%
% croptiled - this function takes a rdnctiles structure and crops the
%             data held according to a set of crop specifications
%             given as an argument. it returns an updated rdnctiles style
%             structure containing each tile separately.
%
% calcFacetAngles - calculate cos and sin terms needed to rotate vectors
%                   onto underlying coordinate system.
%
% zmean           - routine for calculating vertical average.
%
% vel2latlon      - rotate velocity vectors according to angle factors
%                   previously calculated.
%
% llc_pcol        - plots a scalar map.
%
% llc_vec         - plots a vector map.

% Set what to get and where to get it from
% gfpat    - where to get grid files
% dfpat    - where to get output files
% flist    - fields to get
% tlev     - timestep number
% klo, khi - restrict indices in vertical to this range
gfpat='../llc_grid_72tiles/*nc'; dfpat='../llc_48month/*nc';
flist={'dyG', 'dxG', 'XG','YG','XC', 'YC', 'Z','Zl','Ttave','uVeltave','vVeltave'};
tlev=172800;
klo=1; khi=11;

% Load tiles
% ( The example below includes a special mod to get rid of tiles in the llc 72 tile mesh. )
% ( The Antarctic grid tiles (69,70,71,72) contain junk, so get rid of them. )
t=rdnctiles({dfpat,gfpat},flist,[tlev],'bytile',[]);
t=t(1:68);

% Useful commands!
% This command gives the max over all separate tiles.
% max(cellfun(@(x)max(x(:)),(arrayfun(@(x)(x.uVeltave),(arrayfun(@(x)(x.var),t)),'UniformOutput',0))))
% This command extracts a great big long vector from all the separate tiles.
% a=cell2mat((cellfun(@(x)(x(:)),(arrayfun(@(x)(x.uVeltave),(arrayfun(@(x)(x.var),t)),'UniformOutput',0)),'UniformOutput',0)));
% max(a(:));

% Crop fields to desired set of points 
%  (cspec should be incorporated into rdnctiles to do cropping on the fly).
% croptiled() is a function that is used to crop tiles datastructures returned by rdnctiles.
cspec.crops(1).ranks=3;
cspec.crops(1).indexlo=klo; cspec.crops(1).indexhi=khi;
cspec.crops(1).fields={'uVeltave', 'vVeltave'};
cspec.crops(2).ranks=3;
cspec.crops(2).indexlo=1; cspec.crops(2).indexhi=1;  
cspec.crops(2).fields={'Ttave'};
t=croptiled(t,cspec);

% 
% Calculate rotation angle to get latlon flow
for tn=1:length(t)
 [t(tn).var.ca,t(tn).var.sa,t(tn).var.ua,t(tn).var.va] = ...
  calcFacetAngles( t(tn).var.YG, t(tn).var.dxG, t(tn).var.dyG );
end

% Calculate vertical means
for tn=1:length(t)
 t(tn).var.ubz=zmean(t(tn).var.uVeltave,t(tn).var.Zl);
 t(tn).var.vbz=zmean(t(tn).var.vVeltave,t(tn).var.Zl);
end

% Calculate lat-lon projection of vertical mean flow
for tn=1:length(t)
  ca=t(tn).var.ca;       sa=t(tn).var.sa;
  uvec=t(tn).var.ubz;    vvec=t(tn).var.vbz;
  [u_ll, v_ll] = vel2latlon(ca, sa, uvec, vvec);
  t(tn).var.ubz_ll=u_ll; t(tn).var.vbz_ll=v_ll;
end

% Plot a scalar
figure(1);clf
hc=llc_pcol(t,'ubz_ll',1,'sphere');

% Plot some vector fields
% == Arctic view
figure(2);clf
fspec.proj='cyl';
fspec.lathi=  90; fspec.latlo=  45;
fspec.lonlo=-180; fspec.lonhi=+180;
fspec.az=+90; fspec.el=+90;
fspec.alx  = 3.8e3; fspec.aly  =-3.8e3; 
fspec.axmag=0.0; fspec.aymag=0.5;
fspec.scal=3.;
hv=llc_vec(t,'ubz_ll','vbz_ll',fspec);

% == North Atlantic sector
figure(3);clf
fspec.proj='cyl';
fspec.lathi=  70; fspec.latlo=  20;
fspec.lonlo=-110; fspec.lonhi= -20;
fspec.az=  0; fspec.el=+90;
fspec.alx  = 4.2e3; fspec.aly  = -5.0e3; 
fspec.axmag=0.5; fspec.aymag=0.;
fspec.scal=3.;
hv=llc_vec(t,'ubz_ll','vbz_ll',fspec);


% == South Atlantic sector
figure(4);clf
fspec.proj='cyl';
fspec.lathi= -25; fspec.latlo= -70;
fspec.lonlo= -80; fspec.lonhi=  40;
fspec.az=+90; fspec.el=-90;
fspec.alx  =1.0e3; fspec.aly  = 2.0e3;
fspec.axmag=0.0; fspec.aymag=0.5;
fspec.scal=3.;
hv=llc_vec(t,'ubz_ll','vbz_ll',fspec);

% == Equatorial Pacific (define in two pieces)
figure(5);clf
fspec.proj='cart';
fspec.lathi= [25 25]; fspec.latlo=   [ -25 -25];
fspec.lonlo= [-180 100]; fspec.lonhi=[ -65 180];
fspec.az=+180; fspec.el=0;
fspec.alx  =130; fspec.aly  = -20;
fspec.axmag=0.5; fspec.aymag=0.;
fspec.scal=3.;
fspec.skip=1;
hv=llc_vec(t,'ubz_ll','vbz_ll',fspec);

% == Antarctic view
figure(6);clf
fspec.proj='cyl';
fspec.lathi= -22; fspec.latlo=   -85;
fspec.lonlo=-180; fspec.lonhi=180;
fspec.az=+90; fspec.el=-90;
fspec.alx  =5.0e3; fspec.aly  = -5.0e3;
fspec.axmag=0.0; fspec.aymag=0.5;
fspec.scal=3.;
hv=llc_vec(t,'ubz_ll','vbz_ll',fspec);
