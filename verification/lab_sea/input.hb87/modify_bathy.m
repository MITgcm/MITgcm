
%---- modify bathymetry file: close the Northern boundary:
nx=20; ny=16;

%- read-in original file:
fid=fopen('bathy.labsea1979','r','b');
h0=fread(fid,nx*ny,'real*4'); fclose(fid);
h0=reshape(h0,[nx ny]);

%- close Norther boundary (put a wall):
h=h0; h(:,end)=0;

%- write to file:
fid=fopen('bathy_closeN.bin','w','b'); fwrite(fid,h,'real*4'); fclose(fid);

%- plot to check
figure(1);clf;
subplot(211)
imagesc(h0'); set(gca,'YDir','normal'); 
colorbar
title('Original Bathy')

subplot(212)
imagesc(h'); set(gca,'YDir','normal'); 
colorbar
title('Modified Bathy')
