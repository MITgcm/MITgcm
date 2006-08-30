
% reduce the size of the domain by removing "land points"
%- new domain = j=8:30 of old domain (j=1:31)
%  here we apply this size reduction to input files.

%- old size:
dim1=[120 31 29];
%- new size: 
dim2=dim1;
dim2(2)=length([8:30]);

b0=rdda('bathyPol.bin',dim1(1:2),1,'real*4','b');
b1=b0(:,[8:30]);
size(b1)
fid=fopen('bathyPolR.bin','w','b');
fwrite(fid,b1,'real*4'); fclose(fid);

t0=rdda('thetaPol.bin',dim1,1,'real*4','b');
t1=t0(:,[8:30],:);
size(t1)
fid=fopen('thetaPolR.bin','w','b');
fwrite(fid,t1,'real*4'); fclose(fid);

return

%- make some plots to check:
figure(1);clf;
subplot(211)
imagesc(b0');set(gca,'YDir','normal');colorbar;
subplot(212)
imagesc(b1');set(gca,'YDir','normal');colorbar;

figure(2);clf;
subplot(211)
imagesc(t0(:,:,1)');set(gca,'YDir','normal');colorbar;
subplot(212)
imagesc(t1(:,:,1)');set(gca,'YDir','normal');colorbar;

