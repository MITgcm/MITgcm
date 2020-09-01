% generate support data files for tutorial Southern Ocean Reentrant Channel
% hard-coded for 50km resolution, 20x40 horizonal resolution

% grid depths generated Using the hyperbolic tangent method of Stewart et al. (2017) DOI: 10.1016/j.ocemod.2017.03.012
% to design an optimal grid. https://github.com/kialstewart/vertical_grid_for_ocean_models

dz=   [  5.48716549,   6.19462098,   6.99291201,   7.89353689, ...
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
   
rF=-[0 cumsum(dz)]; % z-coordinates of vertical cell faces
z=diff(rF)./2 + rF(1:length(dz)); % z-coordinates of vertical cell centers
H=-sum(dz); %max depth of vertical grid 

%bathymetry -- flat bottom with idealized mid-depth ridge
bathy=H.*ones(20,40);
bathy(6:14,:)=bathy(6:14,:)+2000*repmat(sin(0:pi/8:pi),[40 1])'; %ridge running N-S through middle of domain
bathy(6:14,15:18)=H + repmat(2000*sin(0:pi/8:pi),[4 1])'.*repmat((19-(15:18))/5,[9 1]); %sloping notch cutting through ridge
bathy(6:14,19:21)=H;  % in this latitude band, contours of f/H are unblocked
bathy(6:14,22:25)=H + repmat(2000*sin(0:pi/8:pi),[4 1])'.*repmat(((22:25)-21)/5,[9 1]); %sloping notch cutting through ridge
bathy(:,1)=0; %wall at southern boundary
fid=fopen('bathy.50km.bin','w','b');fwrite(fid,bathy,'float32');fclose(fid);

% zonal wind file
taux=repmat(0.2*sin(0:pi/39:pi),[20 1]);
fid=fopen('zonal_wind.50km.bin','w','b');fwrite(fid,taux,'float32');fclose(fid);

% 3-D mask for RBCS temperature relaxation
% note we implement "sponge layer" with full restoring at N boundary row (y=40),
% weaker restoring at row just south of N boundary (y=39)
% mask set to zero in surface layer (where core model SST restoring applied)
mask=zeros(20,40,length(z)); mask(:,40,2:length(z))= 1.0; mask(:,39,2:length(z)) = 0.25;
fid=fopen('T_relax_mask.50km.bin','w','b');fwrite(fid,mask,'float32');fclose(fid);

% 2-D SST field for relaxation
Tmax=10.0; Tmin= -2.0; y=0.5:39.5;
T_surf=repmat(Tmin+(Tmax-Tmin)*y/40,[20 1]);
fid=fopen('SST_relax.50km.bin','w','b');fwrite(fid,T_surf,'float32');fclose(fid);

% 3-D Temperature field for initial conditions and RBCS relaxation
h=500; %e-folding scale for temperature decrease with depth
T_3D=zeros(20,40,length(dz));
for k=1:length(dz)
    T_3D(:,:,k)=(T_surf - Tmin)*(exp(z(k)/h) - exp(H/h))/(1-exp(H/h)) + Tmin;
end
fid=fopen('temperature.50km.bin','w','b');fwrite(fid,T_3D,'float32');fclose(fid);
