%function interpicks(dirin,dirout,varargin)
% function interpicks(dirin,dirout,snap)
%
% This function interpolates the data in 
% a set of mnc pickup files and grid files from the MITgcm 
% given in dirin/pickup.*.nc and in dirin/grid.*.nc
% (this can be a global file or a collection of per-tile pickup files)
% to the pickup files dirout/pickup.*.nc and the grid files dirout/grid.*.nc
% (this, too, can be a global file or a collection of per-tile pickup files)
%
% Extrapolation takes place near the domain edges if domain size
% of pickout is larger than that of pickin.
%
% The number of vertical levels must be the same in the two sets of files.
%
% Snap is an optional argument if there is more than one timestep
% in the file.  The default is 1.

%if nargin==2
  snap=1
%else
%  snap=varargin{1}
%endif

if (strcmp(dirin,dirout))
  error('dir','You cant use the same input and output directories!')
end

pickin=dir([dirin '/pickup.*.nc'])
gridin=dir([dirin '/grid.*.nc'])
if length(pickin)~=length(gridin)
  error('in','Incompatible number of input pickups and gridfiles')
end

pickout=dir([dirout '/pickup.*.nc'])
gridout=dir([dirout '/grid.*.nc'])
if length(pickout)~=length(gridout)
  error('out','Incompatible number of output pickups and gridfiles')
end 

%%%%%%%%%%%%INPUT SANITY

fin=netcdf([dirin '/' pickin(1).name],'nowrite');
gin=netcdf([dirin '/' gridin(1).name],'nowrite');
Zcomp=fin{'Z'}(:);
gZcomp=gin{'Z'}(:);
if (sum(Zcomp~=gZcomp)>0)
  error('in','Incompatible Z-axis input pickup and gridfile: 1')
end

Xin=fin{'X'}(:);
gXin=gin{'X'}(:);

if (sum(gXin~=gXin)>0)
  error('in','Incompatible x-axis input pickups and gridfile: 1')
end

Yin=fin{'Y'}(:);
gYin=gin{'Y'}(:);
if (sum(gYin~=gYin)>0)
  error('in','Incompatible y-axis input pickups and gridfile: 1')
end

Xp1in=fin{'Xp1'}(:);
gXp1in=gin{'Xp1'}(:);
if (sum(gXp1in~=gXp1in)>0)
  error('in','Incompatible x-axis input pickups and gridfile: 1')
end

Yp1in=fin{'Yp1'}(:);
gYp1in=gin{'Yp1'}(:);
if (sum(gYp1in~=gYp1in)>0)
  error('in','Incompatible y-axis input pickups and gridfile: 1')
end

close(fin)
close(gin)

for i=2:length(pickin)
  fin=netcdf([dirin '/' pickin(i).name],'nowrite');
  Z=fin{'Z'}(:);
  if (sum(Zcomp~=Z)>0)
    error('Z','Incompatible vertical axes in input pickups:',num2str(i))
  end

  gin=netcdf([dirin '/' pickin(i).name],'nowrite');
  Z=gin{'Z'}(:);
  if (sum(Zcomp~=Z)>0)
    error('Z','Incompatible vertical axes in input gridfiles:',num2str(i))
  end

  Xin=sort([Xin;fin{'X'}(:)]);
  Xp1in=sort([Xp1in;fin{'Xp1'}(:)]);

  gXin=sort([gXin;gin{'X'}(:)]);
  gXp1in=sort([gXp1in;gin{'Xp1'}(:)]);

  if (sum(gXin~=Xin)>0)
    error('X','Incompatible x-axes in input files:',num2str(i))
  end

  Yin=sort([Yin;fin{'Y'}(:)]);
  Yp1in=sort([Yp1in;fin{'Yp1'}(:)]);

  gYin=sort([gYin;fin{'Y'}(:)]);
  gYp1in=sort([gYp1in;fin{'Yp1'}(:)]);

  if (sum(gYin~=Yin)>0)
    error('Y','Incompatible y-axes in input files:',num2str(i))
  end

  close(fin);
  close(gin);
end

store=[Xin(1)];
for i=2:length(Xin)
  if Xin(i-1)~=Xin(i)
    store(end+1)=Xin(i);
  end
end
Xin=store';
clear gXin

store=[Xp1in(1)];
for i=2:length(Xp1in)
  if Xp1in(i-1)~=Xp1in(i)
    store(end+1)=Xp1in(i);
  end
end
Xp1in=store';
clear gXp1in

store=[Yin(1)];
for i=2:length(Yin)
  if Yin(i-1)~=Yin(i)
    store(end+1)=Yin(i);
  end
end
Yin=store';
clear gYin

store=[Yp1in(1)];
for i=2:length(Yp1in)
  if Yp1in(i-1)~=Yp1in(i)
    store(end+1)=Yp1in(i);
  end
end
Yp1in=store';
clear gYp1in

%%%%%%%%%%%%%%% OUTPUT SANITY
fout=netcdf([dirout '/' pickout(1).name],'nowrite');
gout=netcdf([dirout '/' gridout(1).name],'nowrite');
Zcomp=fout{'Z'}(:);
gZcomp=gout{'Z'}(:);
if (sum(Zcomp~=gZcomp)>0)
  error('out','Incompatible Z-axis output pickup and gridfile: 1')
end

Xout=fout{'X'}(:);
gXout=gout{'X'}(:);

if (sum(gXout~=gXout)>0)
  error('out','Incompatible x-axis output pickups and gridfile: 1')
end

Yout=fout{'Y'}(:);
gYout=gout{'Y'}(:);
if (sum(gYout~=gYout)>0)
  error('out','Incompatible y-axis output pickups and gridfile: 1')
end

Xp1out=fout{'Xp1'}(:);
gXp1out=gout{'Xp1'}(:);
if (sum(gXp1out~=gXp1out)>0)
  error('out','Incompatible x-axis output pickups and gridfile: 1')
end

Yp1out=fout{'Yp1'}(:);
gYp1out=gout{'Yp1'}(:);
if (sum(gYp1out~=gYp1out)>0)
  error('out','Incompatible y-axis output pickups and gridfile: 1')
end

close(fout)
close(gout)

for i=2:length(pickout)
  fout=netcdf([dirout '/' pickout(i).name],'nowrite');
  Z=fout{'Z'}(:);
  if (sum(Zcomp~=Z)>0)
    error('Z','Incompatible vertical axes in output pickups:',num2str(i))
  end

  gout=netcdf([dirout '/' pickout(i).name],'nowrite');
  Z=gout{'Z'}(:);
  if (sum(Zcomp~=Z)>0)
    error('Z','Incompatible vertical axes in output gridfiles:',num2str(i))
  end

  Xout=sort([Xout;fout{'X'}(:)]);
  Xp1out=sort([Xp1out;fout{'Xp1'}(:)]);

  gXout=sort([gXout;gout{'X'}(:)]);
  gXp1out=sort([gXp1out;gout{'Xp1'}(:)]);

  if (sum(gXout~=Xout)>0)
    error('X','Incompatible x-axes in output files:',num2str(i))
  end

  Yout=sort([Yout;fout{'Y'}(:)]);
  Yp1out=sort([Yp1out;fout{'Yp1'}(:)]);

  gYout=sort([gYout;fout{'Y'}(:)]);
  gYp1out=sort([gYp1out;fout{'Yp1'}(:)]);

  if (sum(gYout~=Yout)>0)
    error('Y','Incompatible y-axes in output files:',num2str(i))
  end

  close(fout);
  close(gout);
end

store=[Xout(1)];
for i=2:length(Xout)
  if Xout(i-1)~=Xout(i)
    store(end+1)=Xout(i);
  end
end
Xout=store';
clear gXout

store=[Xp1out(1)];
for i=2:length(Xp1out)
  if Xp1out(i-1)~=Xp1out(i)
    store(end+1)=Xp1out(i);
  end
end
Xp1out=store';
clear gXp1out

store=[Yout(1)];
for i=2:length(Yout)
  if Yout(i-1)~=Yout(i)
    store(end+1)=Yout(i);
  end
end
Yout=store';
clear gYout

store=[Yp1out(1)];
for i=2:length(Yp1out)
  if Yp1out(i-1)~=Yp1out(i)
    store(end+1)=Yp1out(i);
  end
end
Yp1out=store';
clear gYp1out


% First, do the Centered variables

% We assume periodicity as MITgcm does
Xinext=[2*Xin(1)-Xin(2);Xin;2*Xin(end)-Xin(end-1)];
Yinext=[2*Yin(1)-Yin(2);Yin;2*Yin(end)-Yin(end-1)];

[xcin,ycin]=meshgrid(Xinext,Yinext);
[xcout,ycout]=meshgrid(Xout,Yout);

%%%%%%%%%%%%% HFacCoutk

HFacoutk=ones([length(Zcomp) size(xcout)]);

disp(['Calculating HFacC...'])
  for j=1:length(gridout)
    gout=netcdf([dirout '/' gridout(j).name],'nowrite');
    Xhere=gout{'X'}(:);
    Yhere=gout{'Y'}(:);
    HFacouthere=gout{'HFacC'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcout).*(Yhere(jj)==ycout));
        HFacoutk(:,jjj,iii)=HFacouthere(:,jj,ii);
      end
    end
    close(gout)
  end
clear HFacouthere
disp(['Done.'])

%%%%%%%%%%%%% ETA

Fieldin=NaN*ones(size(xcin));
Fieldout=NaN*ones(size(xcout));

  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'Eta'}(:,:);
    HFacinhere=squeeze(gin{'HFacC'}(1,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating Eta ...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing Eta within topography'])
  Fieldout=Fieldout.*squeeze(HFacoutk(1,:,:));
  disp('Done.')

  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'Eta'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end

%%%%%%%%%%%%% EtaH

Fieldin=NaN*ones(size(xcin));
Fieldout=NaN*ones(size(xcout));

  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'EtaH'}(:,:);
    HFacinhere=squeeze(gin{'HFacC'}(1,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating EtaH ...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing EtaH within topography'])
  Fieldout=Fieldout.*squeeze(HFacoutk(1,:,:));
  disp('Done.')

  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'EtaH'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end

%%%%%%%%%%%%% EtaHdt

Fieldin=NaN*ones(size(xcin));
Fieldout=NaN*ones(size(xcout));

  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'dEtaHdt'}(:,:);
    HFacinhere=squeeze(gin{'HFacC'}(1,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating dEtaHdt ...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing dEtaHdt within topography'])
  Fieldout=Fieldout.*squeeze(HFacoutk(1,:,:));
  disp('Done.')

  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'dEtaHdt'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end

%S,gSnm1,Temp,gTnm1,phi_nh are on Xc,Rc

%%%%%%%%%%%%%% S

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  clear Fieldinhere
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'S'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating S:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing S:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'S'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% gSnm1

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'gSnm1'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating gSnm1:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing gSnm1:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'gSnm1'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% Temp

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'Temp'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating Temp:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing Temp:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'Temp'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% gTnm1

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'gTnm1'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating gTnm1:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing gTnm1:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'gTnm1'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% phi_nh
fin=netcdf([dirin '/' pickin(1).name],'nowrite');
status=fin{'phi_nh'};
if ~isempty(status)

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'phi_nh'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating phi_nh:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing phi_nh:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'phi_nh'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

end %isempty(status)

%%%%%%%%%%%%%%%%%% gW 
%BFK Not sure this is right, since HFacC isn't on the right grid for gW

fin=netcdf([dirin '/' pickin(1).name],'nowrite');
status=fin{'gW'};

if ~isempty(status)

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'gW'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacC'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating gW:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing gW:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'gW'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end
end %isempty(status)

%U,gUnm1 is on XU,Rc
%Note that there is already a periodic repeat in Uin...
Xp1inext=[2*Xp1in(1)-Xp1in(2);Xp1in;2*Xp1in(end)-Xp1in(end-1)];

[xcin,ycin]=meshgrid(Xp1inext,Yinext);
[xcout,ycout]=meshgrid(Xp1out,Yout);

%%%%%%%%%%%%% HFacWoutk

HFacout=ones(size(xcout));
HFacoutk=ones([length(Zcomp) size(xcout)]);

disp(['Calculating HFacW...'])
  for j=1:length(gridout)
    gout=netcdf([dirout '/' gridout(j).name],'nowrite');
    Xhere=gout{'Xp1'}(:);
    Yhere=gout{'Y'}(:);
    HFacouthere=gout{'HFacW'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcout).*(Yhere(jj)==ycout));
        HFacoutk(:,jjj,iii)=HFacouthere(:,jj,ii);
      end
    end
    close(gout)
  end
clear HFacouthere
disp(['Done.'])

%%%%%%%%%%%%%% U

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'Xp1'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'U'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacW'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  %Note that there is already a periodic repeat in U...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating U:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing U:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'Xp1'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'U'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% gUnm1

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'Xp1'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=squeeze(fin{'gUnm1'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacW'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  %Note that there is already a periodic repeat in gUnm1...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  Fieldin(1,:)=Fieldin(end-1,:);
  Fieldin(end,:)=Fieldin(2,:);

  disp(['Interpolating gUnm1:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing gUnm1:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'Xp1'}(:);
  Yhere=fout{'Y'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'gUnm1'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%V,gVnm1 is on XV,Rc

%Note that there is already a periodic repeat in Vin...
Yp1inext=[2*Yp1in(1)-Yp1in(2);Yp1in;2*Yp1in(end)-Yp1in(end-1)];

[xcin,ycin]=meshgrid(Xinext,Yp1inext);
[xcout,ycout]=meshgrid(Xout,Yp1out);

%%%%%%%%%%%%% HFacSoutk

HFacout=ones(size(xcout));
HFacoutk=ones([length(Zcomp) size(xcout)]);

disp(['Calculating HFacS...'])
  for j=1:length(gridout)
    gout=netcdf([dirout '/' gridout(j).name],'nowrite');
    Xhere=gout{'X'}(:);
    Yhere=gout{'Yp1'}(:);
    HFacouthere=gout{'HFacS'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcout).*(Yhere(jj)==ycout));
        HFacoutk(:,jjj,iii)=HFacouthere(:,jj,ii);
      end
    end
    close(gout)
  end
clear HFacouthere
disp(['Done.'])

%%%%%%%%%%%%%% V

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Yp1'}(:);
    Fieldinhere=squeeze(fin{'V'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacS'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  %Note that there is already a periodic repeat in V...
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-2,:);
  Fieldin(end,:)=Fieldin(3,:);

  disp(['Interpolating V:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing V:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Yp1'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'V'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end

%%%%%%%%%%%%%% gVnm1

 Fieldoutk=zeros([length(Zcomp) size(xcout)]);
 for k=1:length(Zcomp)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));

  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    gin=netcdf([dirin '/' gridin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Yp1'}(:);
    Fieldinhere=squeeze(fin{'gVnm1'}(snap,k,:,:));
    HFacinhere=squeeze(gin{'HFacS'}(k,:,:));
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        [jjj,iii]=find((Xhere(ii)==xcin).*(Yhere(jj)==ycin));
        Fieldin(jjj,iii)=Fieldinhere(jj,ii);
        if (HFacinhere(jj,ii)==0)
          Fieldin(jjj,iii)=NaN;
        end
      end
    end
    close(fin)
    close(gin)
  end
  % This utilizes periodic geometry
  %Note that there is already a periodic repeat in gVnm1...
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  Fieldin(1,:)=Fieldin(end-2,:);
  Fieldin(end,:)=Fieldin(3,:);

  disp(['Interpolating gVnm1:',num2str(k),'...'])
  Field0=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  Fieldout=inpaint_nans(Field0,0);
  disp('Done.')

  disp(['Zeroing gVnm1:',num2str(k),' within topography'])
  Fieldoutk(k,:,:)=Fieldout.*squeeze(HFacoutk(k,:,:));
  disp('Done.')
end

for j=1:length(pickout)
  fout=netcdf([dirout '/' pickout(j).name],'write');
  Xhere=fout{'X'}(:);
  Yhere=fout{'Yp1'}(:);
  [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
  fout{'gVnm1'}(:,:,:)=Fieldoutk(:,min(jj):max(jj),min(ii):max(ii));
  close(fout)
end


