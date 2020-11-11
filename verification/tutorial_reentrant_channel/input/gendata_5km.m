% generate support data files for tutorial Southern Ocean Reentrant Channel
% hard-coded for eddy-permitting 5km resolution, 200x400 horizonal resolution
% matches bathy and sponge layer dimensions of coarse res setup

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
z=diff(rF)./2 + rF(1:49); % z-coordinates of vertical cell centers
H=-sum(dz); %max depth of vertical grid 

%bathymetry -- flat bottom with idealized mid-depth ridge
bathy=H*ones(200,400);
bathy(56:135,:)=bathy(56:135,:)+2000*repmat(sin(0:pi/79:pi),[400 1])'; %ridge running N-S through middle of domain
bathy(56:135,136:185)=H + repmat(2000*sin(0:pi/79:pi),[50 1])'.*repmat((186-(136:185))/51,[80 1]); %sloping notch cutting through ridge
bathy(56:135,186:205)=H; % in this latitude band, contours of f/H are unblocked
bathy(56:135,206:255)=H + repmat(2000*sin(0:pi/79:pi),[50 1])'.*repmat(((206:255)-205)/51,[80 1]); %sloping notch cutting through ridge
bathy(:,1:10)=0; %wall at southern boundary,. same width as coarse-res
fid=fopen('bathy.5km.bin','w','b');fwrite(fid,bathy,'float32');fclose(fid);

% zonal wind file
taux=[zeros(5,1)' 0.2*sin(0:pi/389:pi) zeros(5,1)']; taux=repmat(taux,[200 1]);
fid=fopen('zonal_wind.5km.bin','w','b');fwrite(fid,taux,'float32');fclose(fid);

% 3-D mask for RBCS temperature relaxation
% note we implement "sponge layer" to match coarse-res dimensions
mask=zeros(200,400,length(z)); mask(:,391:400,2:length(z))= 1.0; mask(:,381:390,2:length(z)) = 0.25;
fid=fopen('T_relax_mask.5km.bin','w','b');fwrite(fid,mask,'float32');fclose(fid);

% 2-D SST field for relaxation
Tmax=10.0; Tmin= -2.0; y=0.05:.1:39.95;
T_surf=repmat(Tmin+(Tmax-Tmin)*y/40,[200 1]);
fid=fopen('SST_relax.5km.bin','w','b');fwrite(fid,T_surf,'float32');fclose(fid);

% 3-D Temperature field for initial conditions and RBCS relaxation
h=500; %e-folding scale for temperature decrease with depth
T_3D=zeros(200,400,length(z));
for k=1:length(z)
    T_3D(:,:,k)=(T_surf - Tmin)*(exp(z(k)/h) - exp(H/h))/(1-exp(H/h)) + Tmin;
end
fid=fopen('temperature.5km.bin','w','b');fwrite(fid,T_3D,'float32');fclose(fid);
   
        
        
