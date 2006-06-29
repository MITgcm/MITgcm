function HT = calcHeatTransDirect(varargin)

% HT = calcHeatTransDirect(d,g,time,flu,blkFile,[optional]);
% HT = calcHeatTransDirect(d,g,time,flu,blkFile,'grav',9.81);
%
% Input arguements:  
%   The incoming field data (d) and grid data (g) must be in a structured
%   array format (which is the format that comes from rdmnc):
%       d  [Field data]  hUtave,hVtave,uVeltave,vVeltave,Ttave,UTtave,
%                        VTtave,(Stave,UStave,VStave for atm)
%       g  [Grid data ]  drF,dxG,dyG,dxC,dyC,HFacW,HFacS,rA
%   Other input parameters:
%       time     (vec)  Time levels to analyze ([] for all)
%       flu      (str)  'O' or 'A' for ocean or atmosphere
%       blkFile  (str)  Broken line file (eg 'isoLat_cs32_59.mat')
%   Optional parameters:
%       'grav'   (num, default 9.81)  Acceleration due to gravity
%       'LhVap'  (num, default 2501)  Latent heat of vaporization
%       'CpO'    (num, default 3994)  Specific heat capacity of water
%       'RhoO'   (num, default 1035)  Density of sea water
%       'CpA'    (num, default 1004)  Specific heat capacity of water
%       'DiffKh' (num, default 800)   Horizontal diffusivity
%
% Output:
%   HT_Out is a structured array with the following fields:
%       time    ([nt,1])              Time axis
%       ylatHT  ([ny,1])              Heat transport latitude axis
%       ylatSF  ([ny-1,1])            Implied heating latitude axis
%       AreaZon ([ny,1])              Area in zonal bands
%       SenHT   ([ny,nBas,nt,nFld])   Sensible heat transport 
%       SenSF   ([ny-1,nBas,nt,nFld]) Implied heating above
%       LatHT   ([ny,nBas,nt,nFld])   Latent heat transport (atm only)
%       LatSF   ([ny-1,nBas,nt,nFld]) Implied heating from aboce (atm only)
%   Currently, the routine is only configured to handle the global basin,
%   so nBas = 1 for the output.  ny is defined by the broken line file used
%   for the cube calculation.  nFld is the heat transport component:
%       nFld = 1 = Eulerian circulation HT
%       nFld = 2 = HT by mean circulation
%       nFld = 3 = Residual [3=1-2]
%       nFld = 4 = HT by zonal mean circulation
%       nFld = 5 = Residual [5=2-4]
%       nFld = 6 = HT by horizontal diffusion (ocn only)
%
% Description:
%   Calculation heat transport, and to degree possible, decomposition.
%   Heat transport is given in PW and the implied surface heating/flux
%   in W/m^2.  The incoming data arrays are all assumed to be of the
%   dimensions [6*nc,nc,nr,nt].
%
% Original Author:  Jean-Michel Campin
% Modifications:  Daniel Enderton

% Default constants (can be overriden).
LhVap = 2501;
grav = 9.81;
CpO = 3994;
RhoO = 1035;
CpA = 1004;
DiffKh = 800;

% Read input parameters.
d = varargin{1};
g = varargin{2};
time = varargin{3};
flu = varargin{4};
blkFile = varargin{5};
for ii = 6:2:length(varargin)
    temp = varargin{ii+1}; eval([varargin{ii},' = temp;']);
end

nBas = 0;
nlfs = 1;
if isequal(flu,'A'), nout = 5; end
if isequal(flu,'O'), nout = 6; end
if nlfs ~= 1, error('Not ready to handle non-nonlinear free surface!'); end
if nBas ~= 0, error('Not ready to handle multiple basins'); end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare / reform  incoming data                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Determine time indecies.
if isempty(time), time = d.T; i_time = 1:length(time);  
else [dump,i_time] = ismember(time,d.T); end

nc = size(g.HFacC,2);
nr = size(g.HFacC,3);
nt = length(time);

ac  = reshape(g.rA                     ,[6*nc*nc, 1]);
hw  = reshape(g.HFacW(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
hs  = reshape(g.HFacS(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
dxc = reshape(g.dxC(1:6*nc,1:nc)       ,[6*nc*nc, 1]);
dyc = reshape(g.dyC(1:6*nc,1:nc)       ,[6*nc*nc, 1]);
dxg = reshape(g.dxG(1:6*nc,1:nc)       ,[6*nc*nc, 1]);
dyg = reshape(g.dyG(1:6*nc,1:nc)       ,[6*nc*nc, 1]);
drf = reshape(g.drF,[1,length(g.drF)]);

u  = reshape(d.uVeltave(1:6*nc,1:nc,:,i_time),[6*nc*nc,nr,nt]);
v  = reshape(d.vVeltave(1:6*nc,1:nc,:,i_time),[6*nc*nc,nr,nt]);
ut = reshape(d.UTtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
vt = reshape(d.VTtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
if isequal(flu,'A'),
    uq = reshape(d.UStave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
    vq = reshape(d.VStave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
end
hu = reshape(d.hUtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
hv = reshape(d.hVtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
t  = d.Ttave(1:6*nc,1:nc,:,i_time);
if isequal(flu,'A'),
    q  = d.Stave(1:6*nc,1:nc,:,i_time);
end

% Load broken line information.  Compute (tracer point) cell area between
% broken lines for each basin.  There are nbkl broken lines and nbkl+1
% bands between broken lines.  The variable "bkl_Zon" gives the zone number
% (nbkl+1 total) for a given index between 0 and nbkl, that is, nbkl+1
% total zones.  Comments block is for eventual addition of multiple basin
% calculations.
load(blkFile);
ydim=length(bkl_Ylat);
AreaZon=zeros(ydim+1,1+nBas);
for j = 1:ydim+1
    izon = find(bkl_Zon == j-1);
    AreaZon(j,1) = sum(ac(izon));
%     for b = 1:nBas,
%         tmp = ac.*mskBc(:,b);
%         AreaZon(j,1+b) = sum(tmp(izon));
%     end
end

% Latitute plotting information.  Average latitude of a broken line
% (ylatHF) is calculated from a mean value of the y value of all the edges.
% The latitude at the surface flux points is a mean of the broken line mean
% values.
YlatAv=sum(bkl_Ysg,1)./(1+bkl_Npts'); %'
ylatHT = [-90,YlatAv,90];
ylatSF = ( ylatHT(2:ydim+2) + ylatHT(1:ydim+1) )./2;

% The variable "bkl_Flg" is -1/1 if edge (on a given broken) has a u point
% and -2/2 if it has a v point.  Positive/negative values contribute
% positively/negatively to northward heat transport (this depends on the
% oreientation of the cell).  A zero value indicates an end of edges that
% contribute to a broken line.  The u and v information is parced into two
% seperate fields, ufac and vfac (-2/2 are reduced to -1/1 for vfac).
ufac = zeros([size(bkl_Flg),1+nBas]);
vfac = zeros([size(bkl_Flg),1+nBas]);
ufac(:,:,1) = rem(bkl_Flg,2);
vfac(:,:,1) = fix(bkl_Flg/2);
% for jl=1:ydim,
%     ie=bkl_Npts(jl);
%     for b=1:nBas,
%         ufac(1:ie,jl,1+b)=mskBw(bkl_IJuv(1:ie,jl),b).*ufac(1:ie,jl,1);
%         vfac(1:ie,jl,1+b)=mskBs(bkl_IJuv(1:ie,jl),b).*vfac(1:ie,jl,1);
%     end
% end
ufacabs = abs(ufac);
vfacabs = abs(vfac);

% Prepare mask(s).
% ??? I temporarily took out the code to configure the masks beyond this
%     global one.  Does this need to account for a ridge if present?
mskG=ones(ydim+1,1+nBas);

% Area factors.  "ArW_Dif" and "ArS_Dif" the areas for the western and
% southern edge of cells, respectively  The "_Dif" suffix indicates the  
% areas used for the diffusivity because there is an extra "dxc" or "dyc"
% from the gradient (here for computational efficiency reasons).  The  
% division by "dxc" and "dyc" is associates with gradient of temperature.
%
% ??? Why is the land mask (hw and hs) only in the diffusive area term?
ArW = dyg*reshape(drf,[1,length(drf)]);
ArS = dxg*reshape(drf,[1,length(drf)]);
ArW_Dif=hw.*((dyg./dxc)*reshape(drf,[1,length(drf)]));
ArS_Dif=hs.*((dxg./dyc)*reshape(drf,[1,length(drf)]));

% Compute the temperature and its gradient and the velocity points:
%   tbi/tdi = temperature between/difference i points (at u points)
%   tbj/tdj = temperature between/difference j points (at v points)
% The cube is first split and the extra points are added to the files.
% Then the means and differences are taken.  Note that the division of dxc
% and dyc for the gradient is not applied until later for computational
% efficiency purposes.  The arrays are then croped and reshaped to the
% format for the other field variables, [6*nc*nc,nr].  Note that the
% split_C_cub function adds a row/column of tracer points in front of the
% first row/column of the tile matries.  This, when the differences and
% gradients are computed and cropped, the off indecies are selected from
% [2:nc+1] rather than [1:nc].  (This was a bit mystifying to me).
t6bi=zeros(nc,nc+1,nr,nt,6); t6di=t6bi; q6bi=t6bi;
t6bj=zeros(nc+1,nc,nr,nt,6); t6dj=t6bj; q6bj=t6bj;
t6t=split_C_cub(t);
t6bi([1:nc],:,:,:,:) = ( t6t([1:nc],:,:,:,:) + t6t([2:nc+1],:,:,:,:) )./2;
t6bj(:,[1:nc],:,:,:) = ( t6t(:,[1:nc],:,:,:) + t6t(:,[2:nc+1],:,:,:) )./2;
t6di([1:nc],:,:,:,:) = ( t6t([1:nc],:,:,:,:) - t6t([2:nc+1],:,:,:,:) );
t6dj(:,[1:nc],:,:,:) = ( t6t(:,[1:nc],:,:,:) - t6t(:,[2:nc+1],:,:,:) );
tbi = t6bi([1:nc],[2:nc+1],:,:,:);
tbj = t6bj([2:nc+1],[1:nc],:,:,:);
tdi = t6di([1:nc],[2:nc+1],:,:,:);
tdj = t6dj([2:nc+1],[1:nc],:,:,:);
tbi=reshape(permute(tbi,[1,5,2,3,4]),[6*nc*nc,nr,nt]); 
tbj=reshape(permute(tbj,[1,5,2,3,4]),[6*nc*nc,nr,nt]); 
tdi=reshape(permute(tdi,[1,5,2,3,4]),[6*nc*nc,nr,nt]);
tdj=reshape(permute(tdj,[1,5,2,3,4]),[6*nc*nc,nr,nt]);
if isequal(flu,'A')
    q6t=split_C_cub(q);
    q6bi([1:nc],:,:,:,:) = (q6t([1:nc],:,:,:,:)+q6t([2:nc+1],:,:,:,:))./2;
    q6bj(:,[1:nc],:,:,:) = (q6t(:,[1:nc],:,:,:)+q6t(:,[2:nc+1],:,:,:))./2;
    qbi = q6bi([1:nc],[2:nc+1],:,:,:);
    qbj = q6bj([2:nc+1],[1:nc],:,:,:);
    qbi=reshape(permute(qbi,[1,5,2,3,4]),[6*nc*nc,nr,nt]); 
    qbj=reshape(permute(qbj,[1,5,2,3,4]),[6*nc*nc,nr,nt]); 
end

% Prepare output arrays.  "nout" is the number of transport output fields.
% It is currently hard-coded, but could eventually be an input parameters
% to set which output fields are desired if some of then become
% computationally expensive.
%   SenHT = Sensible heat transport
%   SenSF = Sensible implied surface flux
%   IntV  = Integrated volume transport
%   IntT  = Integrated temperature
SenHT  = zeros(ydim+2,1+nBas,nt,nout);
SenSF  = zeros(ydim+1,1+nBas,nt,nout);
IntV   = zeros(ydim,nr,1+nBas,nt);
IntT   = zeros(ydim,nr,1+nBas,nt);
IntM   = zeros(ydim,nr,1+nBas,nt);
if isequal(flu,'A')
    LatHT = zeros(ydim+2,1+nBas,nt,nout);
    Lat = LatHT;
    LatSF = zeros(ydim+1,1+nBas,nt,nout);
    IntQ  = zeros(ydim,nr,1+nBas,nt);
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Make heat transport calculations                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preparation for calculation of zonal average temperature.  The
% tempereature multiplied by the appropriate length scale ("tbi_temp",
% "tbj_temp") is summed up ("IntT" in the next section) and
% divided by the total length ("IntM", composed from summing "hw_temp",
% "hs_temp").
hw_temp = zeros(size(hw));
hs_temp = zeros(size(hs));
for k=1:nr,
    hw_temp(:,k) = dyg.*hw(:,k);
    hs_temp(:,k) = dxg.*hs(:,k);
end

for it = 1:length(time)
    
    %  uz / vz  = Volume transport though cell faces (velocity times area).
    %             Used for zonal mean volume transport (4).
    % utz1/vtz1 = Eulerian sensible heat transport through cell faces (1).
    % utz2/vtz2 = Sensible heat transport through cell faces by Eulerian
    %             mean circulations (2).
    % dtx1/dty1 = Temperatude gradient at cell face times the area (when
    %             multiplied by the diffusion will be the horizontal
    %             diffusion heat transport) (6).
    uz = ArW.*hu(:,:,it);
    vz = ArS.*hv(:,:,it);
    utz1 = sum(ArW.*ut(:,:,it),2);
    vtz1 = sum(ArS.*vt(:,:,it),2);
    utz2 = sum(ArW.*hu(:,:,it).*tbi(:,:,it),2);
    vtz2 = sum(ArS.*hv(:,:,it).*tbj(:,:,it),2);
    if isequal(flu,'A')
        uqz1 = sum(ArW.*uq(:,:,it),2);
        vqz1 = sum(ArS.*vq(:,:,it),2);
        uqz2 = sum(ArW.*hu(:,:,it).*qbi(:,:,it),2);
        vqz2 = sum(ArS.*hv(:,:,it).*qbj(:,:,it),2);
    end
    dtx1 = sum(ArW_Dif.*tdi(:,:,it),2);
    dty1 = sum(ArS_Dif.*tdj(:,:,it),2);
    
    % Preparation for calculation of zonal average temperature.  The
    % tempereature multiplied by the appropriate length scale ("tbi_temp",
    % "tbj_temp") is summed up ("IntT" in the next section) and
    % divided by the total length ("IntM", composed from summing "hw_temp",
    % "hs_temp").
    tbi_temp = hw_temp.*tbi(:,:,it);
    tbj_temp = hs_temp.*tbj(:,:,it);
    if isequal(flu,'A')
        qbi_temp = hw_temp.*qbi(:,:,it);
        qbj_temp = hs_temp.*qbj(:,:,it);
    end

    % Block 1:
    % With the vertical integral of heat transport calculated across cell
    % edges, the zonal integral (along the broken line) is computed to
    % determine the total northward heat transport.  The first for loop is
    % over the individual broken lines (determining the northward HT at the
    % representative latitude).  The second loop is over the basins.  The
    % third loop is over the individual edges along a specific broken line.
    % Note that an individual cell can have two edges (u and v) that have a
    % HT contributions.  Hence the variable containing indecies of cells
    % with edges along the broken lines, "bkl_IJuv", has some repeats.
    % Note that the variable "bkl_Npts" is the number of edges along a
    % given broken line.  Note also that the latitude axis starts at 2
    % because heat transport at extremes (latitute =  -90/90) is zero by
    % definition.  Recall that index (1) is total Eulerian transport, (2)
    % is from the mean circulations, and (3) is from the horizontal
    % diffusion.
    %
    % Block 2:
    % Here zonal average circulation and temperature / moisture is
    % calculated.  The zonal average volume transport v (IntV) and t
    % (IntT/IntM) are computed first and are multiplied together at the
    % end.
    for jl=1:ydim
        ie=bkl_Npts(jl);
        for b=1:1+nBas
	    ij=bkl_IJuv(1:ie,jl);
            % Block 1:
            SenHT(1+jl,b,it,1) = SenHT(1+jl,b,it,1) + ...
                                 sum(ufac(1:ie,jl,b).*utz1(ij) + ...
                                     vfac(1:ie,jl,b).*vtz1(ij));
            SenHT(1+jl,b,it,2) = SenHT(1+jl,b,it,2) + ...
                                 sum(ufac(1:ie,jl,b).*utz2(ij) + ...
                                     vfac(1:ie,jl,b).*vtz2(ij));
            if isequal(flu,'A')
               LatHT(1+jl,b,it,1) = LatHT(1+jl,b,it,1) + ...
                                    sum(ufac(1:ie,jl,b).*uqz1(ij) + ...
                                        vfac(1:ie,jl,b).*vqz1(ij));
               LatHT(1+jl,b,it,2) = LatHT(1+jl,b,it,2) + ...
                                    sum(ufac(1:ie,jl,b).*uqz2(ij) + ...
                                        vfac(1:ie,jl,b).*vqz2(ij));
            end
            if isequal(flu,'O')
               SenHT(1+jl,b,it,6) = SenHT(1+jl,b,it,6) + ...
                                    sum(ufac(1:ie,jl,b).*dtx1(ij) + ...
                                        vfac(1:ie,jl,b).*dty1(ij));
            end
            % Block 2:
            IntV(jl,:,b,it) = IntV(jl,:,b,it) ...
                                + ufac(1:ie,jl,b)'*uz(ij,:) ...
                                + vfac(1:ie,jl,b)'*vz(ij,:) ;
            IntT(jl,:,b,it) = IntT(jl,:,b,it) ...
                                + ufacabs(1:ie,jl,b)'*tbi_temp(ij,:) ...
                                + vfacabs(1:ie,jl,b)'*tbj_temp(ij,:);
            IntM(jl,:,b,it) = IntM(jl,:,b,it) ...
                                + ufacabs(1:ie,jl,b)'*hw_temp(ij,:) ...
                                + vfacabs(1:ie,jl,b)'*hs_temp(ij,:);
            if isequal(flu,'A')
               IntQ(jl,:,b,it) = IntQ(jl,:,b,it) ...
                                + ufacabs(1:ie,jl,b)'*qbi_temp(ij,:) ...
                                + vfacabs(1:ie,jl,b)'*qbj_temp(ij,:);
            end
        end
    end

    % Prepare HT output:  Calculate HT by zonal flows and tabulate
    % residuals.  Also, the multipliative constants (Cp,rho,grav,LhVap) are
    % applied here to put moisture and potential temperature fluxes in
    % terms of heat transports.
    SenHT(2:ydim+1,:,it,4) = sum(    IntV(:,:,:,it) ...
                                  .* IntT(:,:,:,it) ...
                                  ./ IntM(:,:,:,it),2);
    SenHT(:,:,it,3) = SenHT(:,:,it,1) - SenHT(:,:,it,2);
    SenHT(:,:,it,5) = SenHT(:,:,it,2) - SenHT(:,:,it,4);
    if isequal(flu,'O')
        SenHT(:,:,it,6) = DiffKh.*SenHT(:,:,it,6);
        SenHT(:,:,it,:) = (CpO*RhoO)*SenHT(:,:,it,:);
    elseif isequal(flu,'A')
        SenHT(:,:,it,:) = (CpA./grav).*SenHT(:,:,it,:);
        LatHT(2:ydim+1,:,it,4) = sum(    IntV(:,:,:,it) ...
                                      .* IntQ(:,:,:,it) ...
                                      ./ IntM(:,:,:,it),2);
        LatHT(:,:,it,3) = LatHT(:,:,it,1) - LatHT(:,:,it,2);
        LatHT(:,:,it,5) = LatHT(:,:,it,2) - LatHT(:,:,it,4);
        LatHT(:,:,it,:) = (LhVap./grav).*LatHT(:,:,it,:);
    end

    % Implied surface heat flux from heat transports (implied heating).
    % Tabulated as the difference in heat transports between two broken
    % lines divided by the zonal band area.
    mskG = reshape(mskG,(ydim+1)*(1+nBas),1);
    I = find(mskG==0);
    mskG = reshape(mskG,ydim+1,1+nBas);
    var = zeros(ydim+1,1+nBas); 
    for n=1:min(nout,6),
        varT =   SenHT([2:ydim+2],:,it,n) ...
               - SenHT([1:ydim+1],:,it,n);
        varT = reshape(varT,(ydim+1)*(1+nBas),1); varT(I)=NaN; 
        varT = reshape(varT,ydim+1,1+nBas);
        SenSF([1:ydim+1],:,it,n) = varT./AreaZon;
        if isequal(flu,'A')
            for n=1:min(nout,6)
                varQ =   LatHT([2:ydim+2],:,it,n) ...
                       - LatHT([1:ydim+1],:,it,n);
                varQ = reshape(varQ,(ydim+1)*(1+nBas),1); varQ(I)=NaN; 
                varQ = reshape(varQ,ydim+1,1+nBas);
                LatSF([1:ydim+1],:,it,n) = varQ./AreaZon;
            end
        end
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                    Assign outputs, put in units of PW                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SenHT = SenHT*1e-15;

HT.time = time;
HT.SenHT = SenHT;
HT.SenSF = SenSF;
HT.ylatHT = ylatHT;
HT.ylatSF = ylatSF;
HT.AreaZon = AreaZon;

if isequal(flu,'A')
    LatHT = LatHT*1e-15;
    HT.LatHT = LatHT;
    HT.LatSF = LatSF;
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       Stuff that might need to be added back in later (for basins)      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Block for constructing mskG for different basins:
% if nBas > 0,
%     mskBc=rdda([Rac,'maskC_bas.bin'],[6*nc*nc 3],1,'real*4','b');
%     mskBw=rdda([Rac,'maskW_bas.bin'],[6*nc*nc 3],1,'real*4','b');
%     mskBs=rdda([Rac,'maskS_bas.bin'],[6*nc*nc 3],1,'real*4','b');
%     if nBas==2,
%         mskBc(:,2)=mskBc(:,2)+mskBc(:,3);
%         mskBw(:,2)=mskBw(:,2)+mskBw(:,3);
%         mskBs(:,2)=mskBs(:,2)+mskBs(:,3);
%         mskBc=min(1,mskBc); mskBw=min(1,mskBw); mskBs=min(1,mskBs);
%     end
%     %- load: np_Sep, ij_Sep, tp_Sep:
%     sep_lineF=[Rac,'sepBas_cs32_60'];
%     load(sep_lineF);
%     fprintf([' + bassin mask & Sep.line:',sep_lineF,' \n']);
%     %- compute mask for each bassin: -----------
%     kMsep=1;
%     if kMsep,
%         mskW=1+min(1,ceil(hw(:,1)));
%         mskS=1+min(1,ceil(hs(:,1)));
%         for b=1:nBas,
%             bs=b; b1=1+bs; b2=2+rem(bs,nBas);
%             if nBas == 2, bs=b+b-1; b1=2; b2=3 ; end
%             for j=1:ydim+1,
%                 for i=1:np_Sep(bs,j),
%                     ij=ij_Sep(bs,j,i); typ=abs(tp_Sep(bs,j,i));
%                     if typ == 1,
%                         mskG(j,b1)=mskG(j,b1)*mskW(ij);
%                         mskG(j,b2)=mskG(j,b2)*mskW(ij);
%                     elseif typ == 2,
%                         mskG(j,b1)=mskG(j,b1)*mskS(ij);
%                         mskG(j,b2)=mskG(j,b2)*mskS(ij);
%                     end
%                 end
%             end
%         end
%         mskG=2-min(2,mskG); 
%     end
% end

% % Masking for different basin.
% mskZ = ones(ydim+2,1+nBas);
% mskZ([2:ydim+1],:) = mskG([1:ydim],:) + mskG([2:ydim+1],:);
% mskZ = reshape(min(mskZ,1),(ydim+2)*(1+nBas),1);
% I = find(mskZ == 0);
% mskZ = reshape(mskZ,(ydim+2),1+nBas);
% for n=1:min(6,nout)
%     var=SenHT(:,:,n); 
%     var=reshape(var,(ydim+2)*(1+nBas),1);  var(I) = NaN;
%     var=reshape(var,(ydim+2),1+nBas);  SenHT(:,:,n) = var;
% end

% Makes sure that there are no zeros in area AreaZon:
% - set AreaZon to 1 if = zero
% AreaZon=reshape(AreaZon,(ydim+1)*(1+nBas),1);
% [I]=find(AreaZon==0); AreaZon(I)=1; 
% AreaZon=reshape(AreaZon,ydim+1,1+nBas);