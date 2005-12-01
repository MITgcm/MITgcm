function HT_Out = ...
	calcHeatTransDirect(d,g,time,HeatCst,DiffKh,blkFile,nBas,nlfs,nout,flu)
      
% Output:
%   HT(latitude,basin,field)
%       latitude:   Latitude index of heat transport
%       basin:      Basin (1 = global)
%       field:      Field (1 = Eulerian circulation HT       )
%                         (2 = HT by mean circulation        )
%                         (3 = Residual [3=1-2]              )
%                         (4 = HT by zonal mean circulation  )
%                         (5 = Residual [5=2-4]              )
%                         (6 = HT by horizontal diffusion    )
%                         (7 = Integrated surface heat flux  )
%                         (8-14 = Implied heating from fields)
%
% Description:
%   Calculation heat transport, and to degree possible, decomposition.
%   Heat transport is given in Watts and the implied surface heating/flux
%   in W/m^2.  The incoming data arrays are all assumed to be of the
%   dimensions [6*nc,nc,nr].

% Error checking
if nlfs ~= 1, error('Not ready to handle non-nonlinear free surface!'); end
if nBas ~= 0, error('Not ready to handle multiple basins'); end

MakePlots = 0;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare / reform  incoming data                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Determine time indecies.
if isempty(time), time = d.T; i_time = 1:length(time);  
else [dump,i_time] = ismember(time,d.T); end

% i_time = 1;

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
uq = reshape(d.UStave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
vq = reshape(d.VStave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
hu = reshape(d.hUtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
hv = reshape(d.hVtave(1:6*nc,1:nc,:,i_time)  ,[6*nc*nc,nr,nt]);
t  = d.Ttave(1:6*nc,1:nc,:,i_time);
q  = d.Stave(1:6*nc,1:nc,:,i_time);
if nout > 6, tf = reshape(d.tFluxtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]); end

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
YlatAv=sum(bkl_Ysg,1)./(1+bkl_Npts');
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
%   HT    = Heat transport
%   SF    = Implied surface flux
%   divFx = Integrated surface heat flux
%   IntV  = Integrated volume transport
%   IntT  = Integrated temperature
%
% ??? utz0/vtz0 What do you use this for?
SenHT  = zeros(ydim+2,1+nBas,nt,nout);
SenSF  = zeros(ydim+1,1+nBas,nt,nout);
divFx  = zeros(ydim+1,1+nBas,nt,nout);
IntV   = zeros(ydim,nr,1+nBas,nt);
IntT   = zeros(ydim,nr,1+nBas,nt);
IntM   = zeros(ydim,nr,1+nBas,nt);
utz0   = zeros(6*nc*nc,nt);
vtz0   = zeros(6*nc*nc,nt);
divHfx = zeros(6*nc,nc,nt);
if isequal(flu,'A')
    LatHT  = zeros(ydim+2,1+nBas,nt,nout);
    LatSF  = zeros(ydim+1,1+nBas,nt,nout);
    IntQ   = zeros(ydim,nr,1+nBas,nt);
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
    dtx1 = sum(ArW_Dif.*tdi(:,:,it),2);
    dty1 = sum(ArS_Dif.*tdj(:,:,it),2);
    if isequal(flu,'A')
        uqz1 = sum(ArW.*uq(:,:,it),2);
        vqz1 = sum(ArS.*vq(:,:,it),2);
        uqz2 = sum(ArW.*hu(:,:,it).*qbi(:,:,it),2);
        vqz2 = sum(ArS.*hv(:,:,it).*qbj(:,:,it),2);
    end
    
    
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
    % determine the total northward heat transport.  The first for loop is over
    % the individual broken lines (determining the northward HT at the
    % representative latitude).  The second loop is over the basins.  The third
    % loop is over the individual edges along a specific broken line.  Note
    % that an individual cell can have two edges (u and v) that have a HT
    % contributions.  Hence the variable containing indecies of cells with
    % edges along the broken lines, "bkl_IJuv", has some repeats.  Note that
    % the variable "bkl_Npts" is the number of edges along a given broken line.
    % Note also that the latitude axis starts at 2 because heat transport at
    % extremes (latitute =  -90/90) is zero by definition.  Recall that index
    % (1) is total Eulerian transport, (2) is from the mean circulations, and
    % (3) is from the horizontal diffusion.
    %
    % Block 2:
    % Here the heat transport associated mean zonal average circulation is
    % calculated.  The zonal average volume transport v (IntV) and t
    % (IntT/IntM) are computed first and are multiplied together at the
    % end.
    for jl=1:ydim
        ie=bkl_Npts(jl);
        for b=1:1+nBas
            for ii=1:ie
                ij=bkl_IJuv(ii,jl);
                SenHT(1+jl,b,it,1) = SenHT(1+jl,b,it,1) ...
                                     + ufac(ii,jl,b)*utz1(ij) ...
                                     + vfac(ii,jl,b)*vtz1(ij);
                SenHT(1+jl,b,it,2) = SenHT(1+jl,b,it,2) ...
                                     + ufac(ii,jl,b)*utz2(ij) ...
                                     + vfac(ii,jl,b)*vtz2(ij);
                if isequal(flu,'A')
                	LatHT(1+jl,b,it,1) = LatHT(1+jl,b,it,1) ...
                                         + ufac(ii,jl,b)*uqz1(ij) ...
                                         + vfac(ii,jl,b)*vqz1(ij);
                    LatHT(1+jl,b,it,2) = LatHT(1+jl,b,it,2) ...
                                         + ufac(ii,jl,b)*uqz2(ij) ...
                                         + vfac(ii,jl,b)*vqz2(ij);
                end
                if nout >= 6
                    SenHT(1+jl,b,it,6) = SenHT(1+jl,b,it,6) ...
                                      + ufac(ii,jl,b)*dtx1(ij) ...
                                      + vfac(ii,jl,b)*dty1(ij);
                end
                for k=1:nr
                    IntV(jl,k,b,it) = IntV(jl,k,b,it) ...
                                      + ufac(ii,jl,b).*uz(ij,k) ...
                                      + vfac(ii,jl,b).*vz(ij,k); 
                    IntT(jl,k,b,it) = IntT(jl,k,b,it) ...
                                      + ufacabs(ii,jl,b).*tbi_temp(ij,k) ...
                                      + vfacabs(ii,jl,b).*tbj_temp(ij,k); 
                    IntM(jl,k,b,it) = IntM(jl,k,b,it) ...
                                      + ufacabs(ii,jl,b).*hw_temp(ij,k) ...
                                      + vfacabs(ii,jl,b).*hs_temp(ij,k);
                    if isequal(flu,'A')
                        IntQ(jl,k,b,it) = IntQ(jl,k,b,it) ...
                                          + ufacabs(ii,jl,b).*qbi_temp(ij,k) ...
                                          + vfacabs(ii,jl,b).*qbj_temp(ij,k);                         
                    end
                end
            end
        end
    end

    % Prepare HT output, including muliplication of HeatCst.
    SenHT(2:ydim+1,:,it,4) = sum(    IntV(:,:,:,it) ...
                               .* IntT(:,:,:,it) ...
                               ./ IntM(:,:,:,it),2);
    SenHT(:,:,it,3) = SenHT(:,:,it,1) - SenHT(:,:,it,2);
    SenHT(:,:,it,5) = SenHT(:,:,it,2) - SenHT(:,:,it,4);
    if nout >= 6, SenHT(:,:,it,6) = DiffKh.*SenHT(:,:,it,6); end
    SenHT(:,:,it,:)=HeatCst*SenHT(:,:,it,:);
    if isequal(flu,'A')
        LatHT(2:ydim+1,:,it,4) = sum(    IntV(:,:,:,it) ...
                                      .* IntT(:,:,:,it) ...
                                      ./ IntM(:,:,:,it),2);
        LatHT(:,:,it,3) = LatHT(:,:,it,1) - LatHT(:,:,it,2);
        LatHT(:,:,it,5) = LatHT(:,:,it,2) - LatHT(:,:,it,4);
        LatHT(:,:,it,:) = (2501./9.81)*LatHT(:,:,it,:);
    end
    
    % Integrated heat flux in zones calculated from tFlux.
    if nout > 6
        for j = 1:ydim+1
            I = find(bkl_Zon == j-1);
            var = ac.*tf(:,it);
            divFx(j,1,it,nout) = sum(var(I));
            % for b=1:nBas
            %     tmp=var.*mskBc(:,b);
            %     divFx(j,1+b,nout)=sum(tmp(I));
            % end
        end
        SenSF(:,:,it,nout) = divFx(:,:,it,nout)./AreaZon;
        SenHT([2:(ydim+2)],:,it,nout) = cumsum(divFx(:,:,it,nout));
    end

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

    % Horizontal divergence of vertically integrated heat transport.  Recall
    % that "utz1" and "vtz1" are the vertically integrated Eulerian sensible
    % heat transport through edges.
    uT = reshape(utz1,6*nc,nc);
    vT = reshape(vtz1,6*nc,nc);
    [u6t,v6t] = split_UV_cub(uT,vT);
    temp = u6t([2:nc+1],:,:) - u6t([1:nc],:,:) + ...
           v6t(:,[2:nc+1],:) - v6t(:,[1:nc],:);
    divHfx(:,:,it) =    reshape(permute(temp,[1 3 2]),[6*nc,nc]) ...
                     ./ reshape(ac,[6*nc,nc]);
    % ??? What exactly are you measuring with this metric?  Is this not
    %     supposed to give the implied surface heat flux?  The result looks
    %     slightly off; both for the scale and for the corners.
    % merccube(xg,yg,divHfx); colorbar;

    % Implied surface heat flux from heat transports (implied heating).
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
    end
    if isequal(flu,'A')
        for n=1:min(nout,6),
            varQ =   LatHT([2:ydim+2],:,it,n) ...
                   - LatHT([1:ydim+1],:,it,n);
            varQ = reshape(varQ,(ydim+1)*(1+nBas),1); varQ(I)=NaN; 
            varQ = reshape(varQ,ydim+1,1+nBas);
            LatSF([1:ydim+1],:,it,n) = varQ./AreaZon;
        end
    end
    
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Assign outputs                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SenHT = SenHT*1e-15;

HT_Out.time = time;
HT_Out.SenHT = SenHT;
HT_Out.SenSF = SenSF;
HT_Out.ylatHT = ylatHT;
HT_Out.ylatSF = ylatSF;
HT_Out.AreaZon = AreaZon;

try HT_Out.SenHTbyEulerCirc                = SenHT(:,:,:,1); catch, end
try HT_Out.SenHTbyMeanCirc                 = SenHT(:,:,:,2); catch, end
try HT_Out.SenHTbyEulerCircMinusMeanCirc   = SenHT(:,:,:,3); catch, end
try HT_Out.SenHTbyZonMeanCirc              = SenHT(:,:,:,4); catch, end
try HT_Out.SenHTbyMeanCircMinusZonMeanCirc = SenHT(:,:,:,5); catch, end
try HT_Out.SenHTbyHorizDiff                = SenHT(:,:,:,6); catch, end
% HT_Out.SenHTbyIntegSurfHeatFlux = SenHT(:,:,7);

try HT_Out.SenSFbyEulerCirc                = SenSF(:,:,:,1); catch, end
try HT_Out.SenSFbyMeanCirc                 = SenSF(:,:,:,2); catch, end
try HT_Out.SenSFbyEulerCircMinusMeanCirc   = SenSF(:,:,:,3); catch, end
try HT_Out.SenSFbyZonMeanCirc              = SenSF(:,:,:,4); catch, end
try HT_Out.SenSFbyMeanCircMinusZonMeanCirc = SenSF(:,:,:,5); catch, end
try HT_Out.SenSFbyHorizDiff                = SenSF(:,:,:,6); catch, end
% HT_Out.SFbyIntegSurfHeatFlux = SenSF(:,7);

if isequal(flu,'A')
    LatHT = LatHT*1e-15;
    HT_Out.LatHT = LatHT;
    HT_Out.LatSF = LatSF;
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

% Makes sure that there are no zeros in area AreaZon:
% - set AreaZon to 1 if = zero
% AreaZon=reshape(AreaZon,(ydim+1)*(1+nBas),1);
% [I]=find(AreaZon==0); AreaZon(I)=1; 
% AreaZon=reshape(AreaZon,ydim+1,1+nBas);

% if kgr >= 3 & nBas > 1
%     figure(nf+2);clf;
%     subplot(211); yax=ylat2;
%     plot(yax,mskZ(:,1),'r-',yax,mskZ(:,nb),'b-'); 
%     % hold on; plot(yax,mskZ(:,3),'k-'); hold off
%     grid; axis([-90 90 -.2 1.2]);
%     legend('Global',['bas=',int2str(nb)]);
%     title('Mask for Tranp.Heat')
%     subplot(212); yax=ylat1;
%     plot(yax,mskG(:,1),'r-',yax,mskG(:,nb),'b-');
%     % hold on; plot(yax,mskG(:,3),'k-'); hold off
%     grid; axis([-90 90 -.2 1.2]);
%     legend('Global',['bas=',int2str(nb)]);
%     title('Mask for Implied Heat Forcing')
% end

% if kgr >= 2
%     figure(nf+1);clf;
%     psi=zeros(ydim,nr+1);
%     for k=nr:-1:1, psi(:,k)=psi(:,k+1)-IntV(:,k);end
%     subplot(211); imagesc(ylat,[1:nr+1],psi');colorbar
%     subplot(212); plot(ylat,psi(:,1),'k-'); grid;
% end