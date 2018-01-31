function [AA,itrs,MM] = rdmds(fnamearg,varargin)
% RDMDS  Read MITgcmUV meta/data files
%
% A = RDMDS(FNAME)
% A = RDMDS(FNAME,ITER)
% A = RDMDS(FNAME,[ITER1 ITER2 ...])
% A = RDMDS(FNAME,NaN)
% A = RDMDS(FNAME,Inf)
% [A,ITS,M] = RDMDS(FNAME,[...])
% A = RDMDS(FNAME,[...],'rec',RECNUM)
%
%   A = RDMDS(FNAME) reads data described by meta/data file format.
%   FNAME is a string containing the "head" of the file names.
%
%   eg. To load the meta-data files
%       T.0000002880.000.000.meta, T.0000002880.000.000.data
%       T.0000002880.001.000.meta, T.0000002880.001.000.data
%       T.0000002880.002.000.meta, T.0000002880.002.000.data
%       T.0000002880.003.000.meta, T.0000002880.003.000.data
%   use
%      >> A=rdmds('T.0000002880');
%      >> size(A)
%   ans =
%      64    32     5
%   eg. To load a multiple record file
%      >> A=rdmds('pickup.0000002880');
%      >> size(A)
%   ans =
%      64    32     5    61
%
%
%   A = RDMDS(FNAME,ITER) reads data described by meta/data file format.
%   FNAME is a string containing the "head" of the file name excluding the
%   10-digit iterartion number.
%   ITER is a vector of positive integers that will expand to the 10-digit
%   number in the file name.
%   If ITER=NaN, all iterations will be read.
%   If ITER=Inf, the last (highest) iteration will be read.
%
%   eg. To repeat above operation
%      >> A=rdmds('T',2880);
%   eg. To read multiple time steps
%      >> A=rdmds('T',[0 1440 2880]);
%   eg. To read all time steps
%      >> [A,ITS]=rdmds('T',NaN);
%   eg. To read the last time step
%      >> [A,IT]=rdmds('T',Inf);
%   Note: this form can not read files with no iteration count in file name.
%
%
%   A = RDMDS(FNAME,[...],'rec',RECNUM) reads individual records from
%   multiple record files.
%
%   eg. To read a single record from a multi-record file
%      >> [A,IT]=rdmds('pickup.ckptA',11);
%   eg. To read several records from a multi-record file
%      >> [A,IT]=rdmds('pickup',Inf,'rec',[1:5 8 12]);
%
%
%   A = RDMDS(FNAME,ITER,MACHINEFORMAT) allows the machine format to be
%   A = RDMDS(FNAME,MACHINEFORMAT)
%   specified which MACHINEFORMAT is on of the following strings:
%     'n' 'l' 'b' 'd' 'g' 'c' 'a' 's'  - see FOPEN for more details
%

AA=[];
itrs=[];
MM=[];

% Default options
ieee='b';
fname=fnamearg;
userecords=0;
recnum=[];

% Check optional arguments
for ind=1:size(varargin,2);
 arg=varargin{ind};
 if ischar(arg)
  if strcmp(arg,'n') | strcmp(arg,'native')
   ieee='n';
  elseif strcmp(arg,'l') | strcmp(arg,'ieee-le')
   ieee='l';
  elseif strcmp(arg,'b') | strcmp(arg,'ieee-be')
   ieee='b';
  elseif strcmp(arg,'c') | strcmp(arg,'cray')
   ieee='c';
  elseif strcmp(arg,'a') | strcmp(arg,'ieee-le.l64')
   ieee='a';
  elseif strcmp(arg,'s') | strcmp(arg,'ieee-be.l64')
   ieee='s';
  elseif strcmp(arg,'rec')
   userecords=1;
  else
   error(['Optional argument ' arg ' is unknown'])
  end
 else
  if userecords==1
   recnum=arg;
  elseif isempty(itrs)
  if isnan(arg)
   itrs=scanforfiles(fname);
   disp([ sprintf('Reading %i time levels:',size(itrs,2)) sprintf(' %i',itrs) ]);
  elseif isinf(arg)
   itrs=scanforfiles(fname);
   if isempty(itrs)
    AA=[];itrs=[];return;
   end
   disp([ sprintf('Found %i time levels, reading %i',size(itrs,2),itrs(end)) ]);
   itrs=itrs(end);
% elseif prod(double(arg>=0)) & prod(double(round(arg)==arg))
% elseif prod(arg>=0) & prod(round(arg)==arg)
  elseif min(arg)>=0 & isempty(find(round(arg)~=arg))
   if arg>=9999999999
    error(sprintf('Argument %i > 9999999999',arg))
   end
   itrs=arg;
  elseif length(arg) == 1 & arg == -1
   itrs=arg;
  else
   error(sprintf('Argument %i must be a positive integer',arg))
  end
  else
   error('Multiple iterations should be specified as a vector')
  end
 end
end

if isempty(itrs)
 itrs=-1;
end

% Loop over each iteration
for iter=1:size(itrs,2);
 if itrs(iter)>=0
  fname=sprintf('%s.%10.10i',fnamearg,itrs(iter));
 end

% Figure out if there is a path in the filename
 NS=findstr('/',fname);
 if size(NS)>0
  Dir=fname(1:NS(end));
 else
  Dir='./';
 end

% Match name of all meta-files
  %fprintf(' search for file "%s".*meta\n',fname);
 allfiles=dir( sprintf('%s.*meta',fname) );

 if size(allfiles,1)==0
  disp(sprintf('No files match the search: %s.*meta',fname));
 %allow partial reads%  error('No files found.')
 end

% Loop through allfiles
 for j=1:size(allfiles,1);
  %fprintf(' file # %3i : %s\n',j,allfiles(j).name);

% Read meta- and data-file
  [A,N,M,mG] = localrdmds([Dir allfiles(j).name],ieee,recnum);

%- Merge local Meta file content (M) to MM string:
  if j > 0, %- to comment out this block: "if j < 0" (since j is always > 0)
   ii=findstr(M,' timeStepNumber');
   if isempty(ii), ii1=0; ii2=0;
   else ii1=ii; ii2=ii+min(findstr(M(1+ii:end),'];')); end
   ii=findstr(M,' timeInterval');
   if isempty(ii), jj1=0; jj2=0;
   else jj1=ii; jj2=ii+min(findstr(M(1+ii:end),'];')); end
   if iter==1 & j==1,
    MM=M; ind1=0; ind2=0; is1=ii1; js1=jj1; M3='';
    if ii1*jj1 > 0,
     %ind1=min(ii1,jj1); ind2=max(ii2,jj2);
     %if ii1 < jj1, ii3=ii2+1; jj3=jj1-1;
     %else  ii3=jj2+1; jj3=ii1-1; end
      order=sort([ii1 ii2 jj1 jj2]);
      ind1=order(1); ii3=order(2)+1; jj3=order(3)-1; ind2=order(4);
      M2=M(ii1:ii2); M4=M(jj1:jj2); M3=M(ii3:jj3);
    elseif ii1 > 0,
      ind1=ii1; ind2=ii2;
      M2=M(ii1:ii2); M4='';
    elseif jj1 > 0,
      ind1=jj1; ind2=jj2;
      M4=M(jj1:jj2); M2='';
    end
    M5=M(1+ind2:end);
    %fprintf(' ii1,ii2 = %i %i ; jj1,jj2= %i %i ;', ii1,ii2, jj1,jj2);
    %fprintf(' ii3,jj3= %i %i ; ind1,ind2= %i %i\n',ii3,jj3,ind1,ind2);
    %fprintf('M(1:ind1)=%s<\n',M(1:ind1));
    %fprintf(' M2=%s<\n',M2);
    %fprintf(' M3=%s<\n',M3);
    %fprintf(' M4=%s<\n',M4);
    %fprintf(' M5=%s<\n',M5);
   else
    if ii1*jj1 > 0,
         order=sort([ii1 ii2 jj1 jj2]);
         ind=order(1); ii3=order(2)+1; jj3=order(3)-1; ind2=order(4);
    else ind=max(ii1,jj1); ind2=ii2+jj2; end
    compar=(ind == ind1);    ii=0;
    if compar & ind1 == 0,   ii=1; compar=strcmp (MM,M); end
    if compar & ind1 > 0,    ii=2; compar=strncmp(MM,M,ind1) ; end
    if compar & ind1 > 0,    ii=3; compar=strcmp(M5,M(1+ind2:end)); end
    if compar & ii1*jj1 > 0, ii=4; compar=strcmp(M3,M(ii3:jj3)); end
    if ~compar,
     fprintf('WARNING: Meta file (%s) is different (%i) from 1rst one:\n', ...
              allfiles(j).name,ii);
     fprintf(' it=%i :MM:%s\n',itrs(1),MM);
     fprintf(' it=%i :M :%s\n\n',itrs(iter),M);
    elseif ind1 > 0,
     if ii1 > 0,
      Mj=M(ii1:ii2); ii=findstr(Mj,'['); Mj=Mj(1+ii:end);
%   add it-number from Mj to M2 (if different):
      if isempty(findstr(M2,Mj)), M2=[deblank(M2(1:end-1)),Mj]; end
     end
     if jj1 > 0,
      Mj=M(jj1:jj2); ii=findstr(Mj,'['); Mj=Mj(1+ii:end);
%   add time interval from Mj to M4 (if different):
      if isempty(findstr(M4,Mj)), M4=[deblank(M4(1:end-1)),' ;',Mj]; end
     end
    end
   end
%  save modifications:
   if ind1>0 & j==size(allfiles,1) & iter==size(itrs,2),
     if ii1 < jj1, MM=[MM(1:ind1-1),M2,M3,M4,M5];
     else          MM=[MM(1:ind1-1),M4,M3,M2,M5]; end
   end
  end

%- put local data file content in global array AA:
  bdims=N(1,:);
  r0=N(2,:);
  rN=N(3,:);
  ndims=prod(size(bdims));
  if j==1 & iter==1, AA=zeros([bdims size(itrs,2)]); end
  if mG(1)==0 & mG(2)==1,
    if     (ndims == 1)
     AA(r0(1):rN(1),iter)=A;
    elseif (ndims == 2)
     AA(r0(1):rN(1),r0(2):rN(2),iter)=A;
    elseif (ndims == 3)
     AA(r0(1):rN(1),r0(2):rN(2),r0(3):rN(3),iter)=A;
    elseif (ndims == 4)
     AA(r0(1):rN(1),r0(2):rN(2),r0(3):rN(3),r0(4):rN(4),iter)=A;
    elseif (ndims == 5)
     AA(r0(1):rN(1),r0(2):rN(2),r0(3):rN(3),r0(4):rN(4),r0(5):rN(5),iter)=A;
    else
     error('Dimension of data set is larger than currently coded. Sorry!')
    end
  elseif     (ndims == 1)
     AA(r0(1):rN(1),iter)=A;
  else
%- to debug: do simple stransfert (with a loop on 2nd index);
%  will need to change this later, to improve efficiency:
   for i=0:rN(2)-r0(2),
    if (ndims == 2)
     AA(r0(1)+i*mG(1):rN(1)+i*mG(1),r0(2)+i*mG(2),iter)=A(:,1+i);
    elseif (ndims == 3)
     AA(r0(1)+i*mG(1):rN(1)+i*mG(1),r0(2)+i*mG(2), ...
                                    r0(3):rN(3),iter)=A(:,1+i,:);
    elseif (ndims == 4)
     AA(r0(1)+i*mG(1):rN(1)+i*mG(1),r0(2)+i*mG(2), ...
                        r0(3):rN(3),r0(4):rN(4),iter)=A(:,1+i,:,:);
    elseif (ndims == 5)
     AA(r0(1)+i*mG(1):rN(1)+i*mG(1),r0(2)+i*mG(2), ...
            r0(3):rN(3),r0(4):rN(4),r0(5):rN(5),iter)=A(:,1+i,:,:,:);
    else
     error('Dimension of data set is larger than currently coded. Sorry!')
    end
   end
  end

 end % files
end % iterations

%-------------------------------------------------------------------------------

function [A,N,M,map2glob] = localrdmds(fname,ieee,recnum)

mname=strrep(fname,' ','');
dname=strrep(mname,'.meta','.data');

%- set default mapping from tile to global file:
map2glob=[0 1];

% Read and interpret Meta file
fid = fopen(mname,'r');
if (fid == -1)
 error(['File' mname ' could not be opened'])
end

% Scan each line of the Meta file
allstr=' ';
keepgoing = 1;
while keepgoing > 0,
 line = fgetl(fid);
 if (line == -1)
  keepgoing=-1;
 else
% Strip out "(PID.TID *.*)" by finding first ")"
%old  ind=findstr([line ')'],')'); line=line(ind(1)+1:end);
  ind=findstr(line,')');
  if size(ind) ~= 0
    line=line(ind(1)+1:end);
  end
% Remove comments of form //
  line=[line,' //']; ind=findstr(line,'//'); line=line(1:ind(1)-1);
% Add to total string (without starting & ending blanks)
  while line(1:1) == ' ', line=line(2:end); end
  if strncmp(line,'map2glob',8), eval(line);
  else allstr=[allstr,deblank(line),' '];
  end
 end
end

% Close meta file
fclose(fid);

% Strip out comments of form /* ... */
ind1=findstr(allstr,'/*'); ind2=findstr(allstr,'*/');
if size(ind1) ~= size(ind2)
 error('The /* ... */ comments are not properly paired')
end
while size(ind1,2) > 0
 allstr=[deblank(allstr(1:ind1(1)-1)) allstr(ind2(1)+2:end)];
%allstr=[allstr(1:ind1(1)-1) allstr(ind2(1)+3:end)];
 ind1=findstr(allstr,'/*'); ind2=findstr(allstr,'*/');
end

% This is a kludge to catch whether the meta-file is of the
% old or new type. nrecords does not exist in the old type.
nrecords = NaN;

%- store the full string for output:
M=strrep(allstr,'format','dataprec');

% Everything in lower case
allstr=lower(allstr);

% Fix the unfortunate choice of 'format'
allstr=strrep(allstr,'format','dataprec');

% Evaluate meta information
eval(allstr);

N=reshape( dimlist , 3 , prod(size(dimlist))/3 );
rep=[' dimList = [ ',sprintf('%i ',N(1,:)),']'];
if ~isnan(nrecords) & nrecords > 1 & isempty(recnum)
 N=[N,[nrecords 1 nrecords]'];
elseif ~isempty(recnum) & recnum>nrecords
 error('Requested record number is higher than the number of available records')
end

%- make "dimList" shorter (& fit output array size) in output "M":
 pat=' dimList = \[(\s*\d+\,?)*\s*\]';
 M=regexprep(M,pat,rep);
%  and remove double space within sq.brakets:
ind1=findstr(M,'['); ind2=findstr(M,']');
if length(ind1) == length(ind2),
 for i=length(ind1):-1:1, if ind1(i) < ind2(i),
  M=[M(1:ind1(i)),regexprep(M(ind1(i)+1:ind2(i)-1),'(\s+)',' '),M(ind2(i):end)];
 end; end
else error('The [ ... ] brakets are not properly paired')
end

if isempty(recnum)
 recnum=1;
end

if isnan(nrecords)
% This is the old 'meta' method that used sequential access

A=allstr;
% Open data file
fid=fopen(dname,'r',ieee);

% Read record size in bytes
recsz=fread(fid,1,'uint32');
ldims=N(3,:)-N(2,:)+1;
numels=prod(ldims);

rat=recsz/numels;
if rat == 4
 A=fread(fid,numels,'real*4');
elseif rat == 8
 A=fread(fid,numels,'real*8');
else
 sprintf(' Implied size in meta-file = %d', numels )
 sprintf(' Record size in data-file = %d', recsz )
 error('Ratio between record size and size in meta-file inconsistent')
end

erecsz=fread(fid,1,'uint32');
if erecsz ~= recsz
 sprintf('WARNING: Record sizes at beginning and end of file are inconsistent')
end

fclose(fid);

A=reshape(A,ldims);

else
% This is the new MDS format that uses direct access

 ldims=N(3,:)-N(2,:)+1;
 for r=1:size(recnum(:),1);
 if dataprec == 'float32'
  A(:,r)=myrdda(dname,ldims,recnum(r),'real*4',ieee);
 elseif dataprec == 'float64'
  A(:,r)=myrdda(dname,ldims,recnum(r),'real*8',ieee);
 else
  error(['Unrecognized format in meta-file = ' format]);
 end
 end

 A=reshape(A,[ldims size(recnum(:),1)]);
 if size(recnum(:),1)>1
  N(1,end+1)=size(recnum(:),1);
  N(2,end)=1;
  N(3,end)=size(recnum(:),1);
 end

end

%-------------------------------------------------------------------------------

% result = RDDA( file, dim, irec [options] )
%
% This routine reads the irec'th record of shape 'dim' from the
% direct-access binary file (float or double precision) 'file'.
%
% Required arguments:
%
%   file  - string  - name of file to read from
%   dim   - vector  - dimensions of the file records and the resulting array
%   irec  - integer - record number in file in which to write data
%
% Optional arguments (must appear after the required arguments):
%   prec  - string  - precision of storage in file. Default = 'real*8'.
%   ieee  - string  - IEEE bit-wise representation in file. Default = 'b'.
%
% 'prec' may take the values:
%       'real*4' - floating point, 32 bits.
%       'real*8' - floating point, 64 bits - the efault.
%
% 'ieee' may take values:
%    'ieee-be'     or 'b' - IEEE floating point with big-endian
%                           byte ordering - the default
%    'ieee-le'     or 'l' - IEEE floating point with little-endian
%                           byte ordering
%    'native'      or 'n' - local machine format
%    'cray'        or 'c' - Cray floating point with big-endian
%                           byte ordering
%    'ieee-le.l64' or 'a' - IEEE floating point with little-endian
%                           byte ordering and 64 bit long data type
%    'ieee-be.l64' or 's' - IEEE floating point with big-endian byte
%                           ordering and 64 bit long data type.
%
% eg.   T=rdda('t.data',[64 64 32],1);
%       T=rdda('t.data',[256],4,'real*4');
%       T=rdda('t.data',[128 64],2,'real*4','b');
function [arr] = myrdda(file,N,k,varargin)

% Defaults
WORDLENGTH=8;
rtype='real*8';
ieee='b';

% Check optional arguments
args=char(varargin);
while (size(args,1) > 0)
 if deblank(args(1,:)) == 'real*4'
  WORDLENGTH=4;
  rtype='real*4';
 elseif deblank(args(1,:)) == 'real*8'
  WORDLENGTH=8;
  rtype='real*8';
 elseif deblank(args(1,:)) == 'n' | deblank(args(1,:)) == 'native'
  ieee='n';
 elseif deblank(args(1,:)) == 'l' | deblank(args(1,:)) == 'ieee-le'
  ieee='l';
 elseif deblank(args(1,:)) == 'b' | deblank(args(1,:)) == 'ieee-be'
  ieee='b';
 elseif deblank(args(1,:)) == 'c' | deblank(args(1,:)) == 'cray'
  ieee='c';
 elseif deblank(args(1,:)) == 'a' | deblank(args(1,:)) == 'ieee-le.l64'
  ieee='a';
 elseif deblank(args(1,:)) == 's' | deblank(args(1,:)) == 'ieee-be.l64'
  ieee='s';
 else
  error(['Optional argument ' args(1,:) ' is unknown'])
 end
 args=args(2:end,:);
end

nnn=prod(N);

[fid mess]=fopen(file,'r',ieee);
if fid == -1
 error('Error while opening file:\n%s',mess)
end
st=fseek(fid,nnn*(k-1)*WORDLENGTH,'bof');
if st ~= 0
 mess=ferror(fid);
 error('There was an error while positioning the file pointer:\n%s',mess)
end
[arr count]=fread(fid,nnn,rtype);
if count ~= nnn
 error('Not enough data was available to be read: off EOF?')
end
st=fclose(fid);
%arr=reshape(arr,N);

%
function [itrs] = scanforfiles(fname)

itrs=[];
allfiles=dir([fname '.*.001.001.meta']);
if isempty(allfiles)
 allfiles=dir([fname '.*.meta']);
 ioff=0;
else
 ioff=8;
end
for k=1:size(allfiles,1);
 hh=allfiles(k).name;
 itrs(k)=str2num( hh(end-14-ioff:end-5-ioff) );
end
itrs=sort(itrs);
