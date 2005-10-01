iimg=0;
for i=0:60:720
figure
fnam1=sprintf('PTRACER01.%10.10d',i)
phi=rdmds(fnam1);
phixy1=squeeze(phi(:,:,1));
fid=fopen('depth_g77.bin','r','b');
depth=fread(fid,'float32');
depth=reshape(depth,[128 64]);
phixy1(find(depth==0))=NaN;
subplot(2,1,1,'align');
surf(phixy1');view(2);shading flat;colorbar
set(gca,'XLim',[1 128]);
set(gca,'YLim',[1 64]);
set(gca,'Box','on')

  months=((i/60));
  titlestring=[num2str(months) ' months'];
  title(titlestring);

fnam2=sprintf('PTRACER02.%10.10d',i)
phi=rdmds(fnam2);
phixy2=squeeze(phi(:,:,1));
fid=fopen('depth_g77.bin','r','b');
depth=fread(fid,'float32');
depth=reshape(depth,[128 64]);
phixy2(find(depth==0))=NaN;
subplot(2,1,2,'align');
surf(phixy2');view(2);shading flat;colorbar
set(gca,'XLim',[1 128]);
set(gca,'YLim',[1 64]);
set(gca,'Box','on');

inam=sprintf('trxy01.%10.10d',iimg)
print('-djpeg100','-r80',inam);
iimg=iimg+1;
end

%--> animate img.*
%--> convert -loop 10000 img.* myanimgif.gif


