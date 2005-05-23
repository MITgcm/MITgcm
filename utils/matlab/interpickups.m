function interpickups(dirin,dirout)
%function interpickups(dirin,dirout)
%This function interpolates the data in 
%a set of mnc pickup files from the MITgcm
%given in dirin/pickup.*.nc
%(this can be a global file or a 
% collection of per-processor pickup files)
%to the pickup files dirout/pickup.*.nc
%(this, too, can be a global file or a 
% collection of per-processor pickup files)
%Extrapolation takes place near the domain edges if domain size
%of pickout is larger than that of pickin.
%The number of vertical levels must be the same in the two sets of files.

pickin=dir([dirin '/pickup.*.nc'])
pickout=dir([dirout '/pickup.*.nc'])

fin=netcdf([dirin '/' pickin(1).name],'nowrite');
Zcomp=fin{'Z'}(:);
Xin=fin{'X'}(:);
Yin=fin{'Y'}(:);
Xp1in=fin{'Xp1'}(:);
Yp1in=fin{'Yp1'}(:);
close(fin)

for i=2:length(pickin)
  fin=netcdf([dirin '/' pickin(i).name],'nowrite');
  Z=fin{'Z'}(:);
  if sum(Zcomp~=Z)
    error('Z','Incompatible vertical axes in input gridfiles')
  end
  Xin=sort([Xin;fin{'X'}(:)]);
  Xp1in=sort([Xp1in;fin{'Xp1'}(:)]);
  Yin=sort([Yin;fin{'Y'}(:)]);
  Yp1in=sort([Yp1in;fin{'Yp1'}(:)]);
  close(fin);
end

store=[Xin(1)];
for i=2:length(Xin)
  if Xin(i-1)~=Xin(i)
    store(end+1)=Xin(i);
  end
end
Xin=store';

store=[Xp1in(1)];
for i=2:length(Xp1in)
  if Xp1in(i-1)~=Xp1in(i)
    store(end+1)=Xp1in(i);
  end
end
Xp1in=store';

store=[Yin(1)];
for i=2:length(Yin)
  if Yin(i-1)~=Yin(i)
    store(end+1)=Yin(i);
  end
end
Yin=store';

store=[Yp1in(1)];
for i=2:length(Yp1in)
  if Yp1in(i-1)~=Yp1in(i)
    store(end+1)=Yp1in(i);
  end
end
Yp1in=store';

fout=netcdf([dirout '/' pickout(1).name],'nowrite');
Zcomp=fout{'Z'}(:);
Xout=fout{'X'}(:);
Yout=fout{'Y'}(:);
Xp1out=fout{'Xp1'}(:);
Yp1out=fout{'Yp1'}(:);
close(fout)

for i=2:length(pickout)
  fout=netcdf([dirout '/' pickout(i).name],'nowrite');
  Z=fout{'Z'}(:);
  if sum(Zcomp~=Z)
    error('Z','Incompatible vertical axes in output gridfiles')
  end
  Xout=sort([Xout;fout{'X'}(:)]);
  Xp1out=sort([Xp1out;fout{'Xp1'}(:)]);
  Yout=sort([Yout;fout{'Y'}(:)]);
  Yp1out=sort([Yp1out;fout{'Yp1'}(:)]);
  close(fout);
end

store=[Xout(1)];
for i=2:length(Xout)
  if Xout(i-1)~=Xout(i)
    store(end+1)=Xout(i);
  end
end
Xout=store';

store=[Xp1out(1)];
for i=2:length(Xp1out)
  if Xp1out(i-1)~=Xp1out(i)
    store(end+1)=Xp1out(i);
  end
end
Xp1out=store';

store=[Yout(1)];
for i=2:length(Yout)
  if Yout(i-1)~=Yout(i)
    store(end+1)=Yout(i);
  end
end
Yout=store';

store=[Yp1out(1)];
for i=2:length(Yp1out)
  if Yp1out(i-1)~=Yp1out(i)
    store(end+1)=Yp1out(i);
  end
end
Yp1out=store';


% First, do the Centered variables
Xinext=[2*Xin(1)-Xin(2);Xin;2*Xin(end)-Xin(end-1)];

[xcin,ycin]=meshgrid(Xinext,Yin);
[xcout,ycout]=meshgrid(Xout,Yout);

Fieldin=NaN*ones(size(xcin));
Fieldout=NaN*ones(size(xcout));

  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'Eta'}(:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(jj,ii);
      end
    end
    close(fin)
  end
    % This utilizes channel geometry
    Fieldin(:,1)=Fieldin(:,end-1);
    Fieldin(:,end)=Fieldin(:,2);

    disp(['Interpolating Eta ...'])
    %Nearests used to eliminate Nans in non-periodic direction
    Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
    Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
    for ii=1:length(Fieldout(:))
       if isnan(Fieldout(ii))
         Fieldout(ii)=Fieldoutn(ii);
       end
    end
    disp('Done.')
  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'Eta'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end


  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'EtaH'}(:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(jj,ii);
      end
    end
    close(fin)
  end
    % This utilizes channel geometry
    Fieldin(:,1)=Fieldin(:,end-1);
    Fieldin(:,end)=Fieldin(:,2);

    disp(['Interpolating EtaH ...'])
    %Nearests used to eliminate Nans in non-periodic direction
    Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
    Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
    for ii=1:length(Fieldout(:))
       if isnan(Fieldout(ii))
         Fieldout(ii)=Fieldoutn(ii);
       end
    end
    disp('Done.')
  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'EtaH'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end


  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'dEtaHdt'}(:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(jj,ii);
      end
    end
    close(fin)
  end
    % This utilizes channel geometry
    Fieldin(:,1)=Fieldin(:,end-1);
    Fieldin(:,end)=Fieldin(:,2);

    disp(['Interpolating dEtaHdt ...'])
    %Nearests used to eliminate Nans in non-periodic direction
    Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
    Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
    for ii=1:length(Fieldout(:))
       if isnan(Fieldout(ii))
         Fieldout(ii)=Fieldoutn(ii);
       end
    end
    disp('Done.')
  for j=1:length(pickout)
    fout=netcdf([dirout '/' pickout(j).name],'write');
    Xhere=fout{'X'}(:);
    Yhere=fout{'Y'}(:);
    [jj,ii]=find((Xhere(1)<=xcout).*(xcout<=Xhere(end)).*(Yhere(1)<=ycout).*(ycout<=Yhere(end)));
    fout{'dEtaHdt'}(:,:)=Fieldout(min(jj):max(jj),min(ii):max(ii));
    close(fout)
  end

%S,gSnm1,Temp,gTnm1,phi_nh is on Xc,Rc

Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'S'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  disp(['Interpolating S level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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


Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'gSnm1'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  disp(['Interpolating gSnm1 level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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



Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'Temp'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  disp(['Interpolating Temp level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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


Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'gTnm1'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  Fieldin(:,1)=Fieldin(:,end-1);
  Fieldin(:,end)=Fieldin(:,2);

  disp(['Interpolating gTnm1 level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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


fin=netcdf([dirin '/' pickin(1).name],'nowrite');
status=fin{'phi_nh'};

if ~isempty(status)
  Fieldoutk=ones([length(Zcomp) size(xcout)]);
  for k=1:length(Z)
    Fieldin=NaN*ones(size(xcin));
    Fieldout=NaN*ones(size(xcout));
    for j=1:length(pickin)
      fin=netcdf([dirin '/' pickin(j).name],'nowrite');
      Xhere=fin{'X'}(:);
      Yhere=fin{'Y'}(:);
      Fieldinhere=fin{'phi_nh'}(:,:,:);
      for ii=1:length(Xhere)
        for jj=1:length(Yhere)
          Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
        end
      end
      close(fin)
    end
    % This utilizes channel geometry
    Fieldin(:,1)=Fieldin(:,end-1);
    Fieldin(:,end)=Fieldin(:,2);

    disp(['Interpolating phi_nh level=' num2str(k) '...'])
    %Nearests used to eliminate Nans in non-periodic direction
    Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'}); 
    Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
    for ii=1:length(Fieldout(:))
       if isnan(Fieldout(ii))
         Fieldout(ii)=Fieldoutn(ii);
       end
    end
    Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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
end

fin=netcdf([dirin '/' pickin(1).name],'nowrite');
status=fin{'gW'};

if ~isempty(status)
  Fieldoutk=ones([length(Zcomp) size(xcout)]);
  for k=1:length(Z)
    Fieldin=NaN*ones(size(xcin));
    Fieldout=NaN*ones(size(xcout));
    for j=1:length(pickin)
      fin=netcdf([dirin '/' pickin(j).name],'nowrite');
      Xhere=fin{'X'}(:);
      Yhere=fin{'Y'}(:);
      Fieldinhere=fin{'gW'}(:,:,:);
      for ii=1:length(Xhere)
        for jj=1:length(Yhere)
          Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
        end
      end
      close(fin)
    end
    % This utilizes channel geometry
    Fieldin(:,1)=Fieldin(:,end-1);
    Fieldin(:,end)=Fieldin(:,2);

    disp(['Interpolating gW level=' num2str(k) '...'])
    %Nearests used to eliminate Nans in non-periodic direction
    Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'}); 
    Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
    for ii=1:length(Fieldout(:))
       if isnan(Fieldout(ii))
         Fieldout(ii)=Fieldoutn(ii);
       end
    end
    Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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
end

%U,gUnm1 is on XU,Rc
%Note that there is already a periodic repeat in Uin...
Xp1inext=[2*Xp1in(1)-Xp1in(2);Xp1in;2*Xp1in(end)-Xp1in(end-1)];

[xcin,ycin]=meshgrid(Xp1inext,Yin);
[xcout,ycout]=meshgrid(Xp1out,Yout);

Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'Xp1'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'U'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  %Note that there is already a periodic repeat in Uin...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  disp(['Interpolating U level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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

Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'Xp1'}(:);
    Yhere=fin{'Y'}(:);
    Fieldinhere=fin{'gUnm1'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  %Note that there is already a periodic repeat in gUnm1in...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  disp(['Interpolating gUnm1 level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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

[xcin,ycin]=meshgrid(Xinext,Yp1in);
[xcout,ycout]=meshgrid(Xout,Yp1out);

Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Yp1'}(:);
    Fieldinhere=fin{'V'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  %Note that there is already a periodic repeat in Vin...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  disp(['Interpolating V level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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

Fieldoutk=ones([length(Zcomp) size(xcout)]);
for k=1:length(Z)
  Fieldin=NaN*ones(size(xcin));
  Fieldout=NaN*ones(size(xcout));
  for j=1:length(pickin)
    fin=netcdf([dirin '/' pickin(j).name],'nowrite');
    Xhere=fin{'X'}(:);
    Yhere=fin{'Yp1'}(:);
    Fieldinhere=fin{'gVnm1'}(:,:,:);
    for ii=1:length(Xhere)
      for jj=1:length(Yhere)
        Fieldin(find((Xhere(ii)==xcin).*(Yhere(jj)==ycin)))=Fieldinhere(k,jj,ii);
      end
    end
    close(fin)
  end
  % This utilizes channel geometry
  %Note that there is already a periodic repeat in gVnm1in...
  Fieldin(:,1)=Fieldin(:,end-2);
  Fieldin(:,end)=Fieldin(:,3);

  disp(['Interpolating gVnm1 level=' num2str(k) '...'])
  %Nearests used to eliminate Nans in non-periodic direction
  Fieldoutn=griddata(xcin,ycin,Fieldin,xcout,ycout,'nearest',{'Qt','Qbb','Qc','Qz'});
  Fieldout=griddata(xcin,ycin,Fieldin,xcout,ycout,'linear',{'Qt','Qbb','Qc','Qz'});
  for ii=1:length(Fieldout(:))
     if isnan(Fieldout(ii))
       Fieldout(ii)=Fieldoutn(ii);
     end
  end
  Fieldoutk(k,:,:)=reshape(Fieldout,1,length(Fieldout(:,1)),length(Fieldout(1,:)));
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


