% generate support data files for tutorial Southern Ocean Reentrant Channel
% hard-coded for eddying resolution: dx=dy=5 km (Cartesian)
% to exactly match the bathymetry and forcing in the coarse-res setup

% grid depths generated Using the hyperbolic tangent method of 
% Stewart et al. (2017) DOI: 10.1016/j.ocemod.2017.03.012
% to design an optimal grid.
% https://github.com/kialstewart/vertical_grid_for_ocean_models

dr =  [  5.48716549,   6.19462098,   6.99291201,   7.89353689, ...
         8.90937723,  10.05483267,  11.34595414,  12.80056778, ...
        14.43837763,  16.28102917,  18.35210877,  20.67704362, ...
        23.28285446,  26.1976981 ,  29.45012046,  33.06792588, ...
        37.07656002,  41.496912  ,  46.34247864,  51.61592052, ...
        57.30518684,  63.37960847,  69.78661289,  76.44996107, ...
        83.27047568,  90.13003112,  96.89898027, 103.44631852, ...
       109.65099217, 115.4122275 , 120.65692923, 125.34295968, ...
       129.45821977, 133.01641219, 136.05088105, 138.60793752, ...
       140.74074276, 142.50436556, 143.95220912, 145.133724  , ...
       146.09317287, 146.86917206, 147.49475454, 147.99774783, ...
       148.40131516, 148.72455653, 148.98310489, 149.18968055, ...
       149.35458582];
nx = 200;
ny = 400; 
nr = length(dr);
rF = -[0 cumsum(dr)];          % z-coordinates of vertical cell faces
z = rF(1:end-1) + diff(rF)/2;  % z-coordinates of vertical cell centers
H = -sum(dr);                  % max depth of vertical grid 

% bathymetry -- flat bottom of depth H (m) with idealized mid-depth ridge
bump_max = 2000;   % peak height of ridge above flat bottom depth
bathy = H .* ones(nx, ny);
bump = zeros(nx, ny);
% sinusoidal bump running N-S through middle of domain
% this is hard-coded for nx=200, ny=400 resolution
r1 = bump_max * repmat(sin(0:pi/79:pi),[ny 1])';
r2 = (0:51)/51;             % create linear ramp for center notch
bump(56:135,:) = r1;
% linearly lower bump height toward center notch
bump(56:135,135:186) = bump(56:135,135:186) .* fliplr(repmat(r2, [80 1]));
bump(56:135,205:256) = bump(56:135,205:256) .* repmat(r2, [80 1]);
bump(56:135,186:205) = 0;   % notch; in these lat bands, contours of f/H are unblocked
bathy = bathy + bump;
bathy(:,1:10) = 0;          % wall at southern boundary, matching coarse-res
fid=fopen('bathy.5km.bin', 'w', 'b');
fwrite(fid, bathy, 'float32');
fclose(fid);

% zonal wind stress file
taux_max = 0.2;
taux=[zeros(5, 1)' taux_max * sin(0:pi/389:pi) zeros(5, 1)'];
taux = repmat(taux, [nx 1]);  % at (XG,YC) points
fid = fopen('zonal_wind.5km.bin', 'w', 'b');
fwrite(fid, taux, 'float32');
fclose(fid);

% 3-D mask for RBCS temperature relaxation
% mask set to zero in surface layer (where core model SST restoring applied)
% note we implement "sponge layer" to match coarse-res dimensions
rbcs_mask = zeros(nx, ny, nr);
rbcs_mask(:,391:end,2:end) = 1.0;
rbcs_mask(:,381:390,2:end) = 0.25;
fid=fopen('T_relax_mask.5km.bin', 'w', 'b');
fwrite(fid, rbcs_mask, 'float32');
fclose(fid);

% 2-D SST field for relaxation, linear ramp between Tmin and Tmax
Tmax = 10.0;
Tmin = -2.0;
sst_relax = repmat(Tmin + (Tmax-Tmin)*(0.05:.1:39.95)/40, [nx 1]); % at (XC,YC) points
fid=fopen('SST_relax.5km.bin', 'w', 'b');
fwrite(fid, sst_relax, 'float32');
fclose(fid);

% 3-D Temperature field for initial conditions and RBCS northern wall profile
h = 500;              % e-folding scale for temperature decrease with depth
T_surf = sst_relax;   % use 2-D SST relaxation field for surface values
zscale = permute(repmat((exp(z/h) - exp(H/h)) / (1-exp(H/h)), [nx 1 ny]), [ 1 3 2]);
T_3D = repmat(T_surf - Tmin, [1 1 nr]) .* zscale + Tmin;
fid=fopen('temperature.5km.bin', 'w', 'b');
fwrite(fid, T_3D, 'float32');
fclose(fid);
