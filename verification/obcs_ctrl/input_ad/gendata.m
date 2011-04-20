clear all
close all
ieee='b';accuracy='single';

dz = [500 500 500 500 500 500 500 500 ]
zf=-cumsum([0 dz]);
zc=(zf(1:end-1)+zf(2:end))/2;

Ho=-zf(length(zf));
nx=64;ny=64;nz = 8;

if 1
    % set up input for model
    % Flat bottom at z=-Ho
    h=-Ho*ones(nx,ny);
    %h(end,:)=0;%h(:,end)=0;%WALLS
    fid=fopen('topog.box','w',ieee); fwrite(fid,h,accuracy); fclose(fid);
    
    T = [20.,16.,12.,10., 9., 8., 7., 6.]    
    T2D = single(ones(64,1)*T);       
%    plot((T2D(1,:)),zc,'-ro','linewidth',2)
    T3D = zeros(nx,ny,nz,'single');
    for i = 1:64;   T3D(i,:,:)= T2D;  end
  
  % T OBSERVED
    fid = fopen('FinalThetaObs.bin','w',ieee);
       fwrite(fid,T3D,accuracy);
    fclose(fid)

  %VPROFILE
    x = linspace(-1,1,ny)';%x = [-1 linspace(-1,1,ny-2) 1]
    V = single(1-(x.^2));
    V2D =single(V*ones(1,nz))./10;
  %V2D = zeros(64,nz,'single');
  for i = 1:64
    V3D(i,:,:) = V2D;
  end

% Lets make a zonal jet solution 
  %INITIAL CONDITION 
  fid = fopen('Vini.bin','w',ieee);
  fwrite(fid,V3D.*0,accuracy);
  fclose(fid)
  fid = fopen('Uini.bin','w',ieee);
  fwrite(fid,V3D,accuracy);
  fclose(fid)
  
  %NORTHERN BOUNDARY
  fid = fopen('Unbc.bin','w',ieee);
  fwrite(fid,zeros(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Vnbc.bin','w',ieee);
  fwrite(fid,zeros(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Snbc.bin','w',ieee);
  fwrite(fid,35.*ones(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Tnbc.bin','w',ieee);
  fwrite(fid,T2D,accuracy);
  fclose(fid) 

  %SOUTHERN BOUNDARY
  fid = fopen('Usbc.bin','w',ieee);
  fwrite(fid,zeros(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Vsbc.bin','w',ieee);
  fwrite(fid,zeros(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Ssbc.bin','w',ieee);
  fwrite(fid,35.*ones(nx,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Tsbc.bin','w',ieee);
  fwrite(fid,T2D,accuracy);
  fclose(fid)

  %WESTERN BOUNDARY
  fid = fopen('Uwbc.bin','w',ieee);
  fwrite(fid,V2D,accuracy);
  fclose(fid)
  fid = fopen('Vwbc.bin','w',ieee);
  fwrite(fid,zeros(ny,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Swbc.bin','w',ieee);
  fwrite(fid,35.*ones(ny,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Twbc.bin','w',ieee);
  fwrite(fid,T2D,accuracy);
  fclose(fid)


  %EASTERN BOUNDARY
  fid = fopen('Uebc.bin','w',ieee);
  fwrite(fid,V2D,accuracy);
  fclose(fid)
  fid = fopen('Vebc.bin','w',ieee);
  fwrite(fid,zeros(ny,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Sebc.bin','w',ieee);
  fwrite(fid,35.*ones(ny,nz,accuracy),accuracy);
  fclose(fid)
  fid = fopen('Tebc.bin','w',ieee);
  fwrite(fid,T2D,accuracy);
  fclose(fid)
  % end of model setup if-then
end


if 0
  % get vertical modes for obcs decomp
  rhonil = 1035;g = 9.81;
  RC = squeeze(rdmds('../GRID/RC'));
  RF = squeeze(rdmds('../GRID/RF'));  
  DRF = squeeze(rdmds('../GRID/DRF'));  
  mask = squeeze(rdmds('../GRID/maskInC'));
  RLOW=RF(end);
  zmid = [0; (RC(1:end-1)+RC(2:end))/2;RLOW];
  NZ = length(RC);
  
  % all the dRhdz* come down to Nz
  dRhodz_bar = rdmds('../run_fwd/dRhodz_5');
  dRhodz_bar(mask==0)=0;dRhodz_bar(dRhodz_bar==0) = nan;
  dRhodz_bar = squeeze(nanmean(nanmean(dRhodz_bar(32:33,32:33,:),1),2));
  dRhodz_bar(1)=dRhodz_bar(2);dRhodz_bar(NZ+1)=dRhodz_bar(NZ);
  Nz = (-g./rhonil.*dRhodz_bar).^0.5;
  
  modesv = zeros(NZ,NZ,NZ);
  % when only one depth, just have 1 for the mode
  modesv(1,1,1) = 1.0;
  % for more than 1 depth, solve eigenvalue problem
  for k = 2:NZ;
    % iz = vector of depth levels
    iz = 1:k;
    % print out a progress number
    NZ-k,
    % regularly spaced depths to the bottom of layer k;
    % leave out the surface: VERT_FSFB2.m will supply it.
    % 5 m spacing was selected as being small enough
    zreg=-5:-5:RF(k+1);
    Nzreg = interp1(zmid,Nz.^2,zreg,'linear',0);
    [c2, F, G, N2, Pmid] = VERT_FSFB2(Nzreg,zreg);
    %VERT_FSFB2.m adds a surface point
    zreg = [0,zreg];
    % sort from largest to smallest phase speed
    [c2s indx] = sort(abs(c2),'descend');
    % now interp back to our grid
    for mds = 1:k
      YI = interp1(zreg,F(:,indx(mds)),RC(iz),'linear',0);
      modesv(iz,mds,k) = YI;
    end
    % have to weight by dz!  Make a vector of fractional dz's
    zwt = DRF(iz)/sum(DRF(iz),1);
    %ensure first mode is barotropic (constant in depth)
    avm1=sum(modesv(iz,1,k).*zwt,1);
    modesv(iz,1,k)=avm1;
    %make all modes orthogonal weighted by delta z
    % use gram-schmidt leaving first one the same
    for mds = 1:k-1
        R = sum((modesv(iz,mds,k).^2).*zwt,1);
        R2 = (modesv(iz,mds,k).*zwt)'*modesv(iz,mds+1:k,k)/R;
        modesv(iz,mds+1:k,k) = modesv(iz,mds+1:k,k) - ...
            modesv(iz,mds,k)*R2;
    end
    %All now orthognal, now normalize
    for mds = 1:k
       R = sqrt(sum((modesv(iz,mds,k).^2).*zwt,1));
       if R < 1e-8
         fprintf('Small R!! for mds = %2i and k = %2i\n',mds,k);
         R = inf;
       end
       modesv(iz,mds,k) = modesv(iz,mds,k)./R;
     end
  end;% end of loop over depth level k
  fid = fopen('baro_invmodes.bin','w','b');
  fwrite(fid,modesv,'double');
  fclose(fid)
  if 1 %plot first 5 modes for deepest case
    figure
    clf;plot(modesv(:,[1:5],NZ),RC(:));
    title('output modes at deepest point')
  end
  if 1  % test orthogonality
    % do whole matrix (need to include dz!)
    k = NZ;
    cmm = (squeeze(modesv(iz,iz,k)).*repmat(zwt,[1 k]))'* ...
          squeeze(modesv(iz,iz,k));
    figure;imagesc(cmm);colorbar
    title([num2str(k) ' mode orthogonality min, max diag: ' ...
           num2str(min(diag(cmm))) ', ' ...
           num2str(max(diag(cmm)))])
  end
  save baro_invmodes modesv
end

