function [AA] = rdmds(fnamearg,varargin)
% RDMDS  Read MITgcmUV meta/data files
%
% A = RDMDS(FNAME)
% A = RDMDS(FNAME,ITER)
% A = RDMDS(FNAME,[ITER1 ITER2 ...])
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
%  
%   eg. To repeat above operation
%      >> A=rdmds('T',2880);
%   eg. To read multiple time steps
%      >> A=rdmds('T',[0 1440 2880]);
%   Note: this form can not read files with no iteration count in file name.
%  
%   A = RDMDS(FNAME,MACHINEFORMAT)
%   A = RDMDS(FNAME,ITER,MACHINEFORMAT) allows the machine format to be
%   specified which MACHINEFORMAT is on of the following strings:
%     'n' 'l' 'b' 'd' 'g' 'c' 'a' 's'  - see FOPEN for more details
%  
%  
% $Header: /u/gcmpack/MITgcm/utils/matlab/rdmds.m,v 1.12 2001/11/27 19:21:56 adcroft Exp $

% Default options
ieee='b';
fname=fnamearg;
iters=-1;

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
  else
   error(['Optional argument ' arg ' is unknown'])
  end
 else
  if isnan(arg)
   iters=scanforfiles(fname);
   disp([ sprintf('Reading %i time levels:',size(iters,2)) sprintf(' %i',iters) ]);
  elseif isinf(arg)
   iters=scanforfiles(fname);
   disp([ sprintf('Found %i time levels, reading %i',size(iters,2),iters(end)) ]);
   iters=iters(end);
  elseif prod(arg>=0) & prod(round(arg)==arg)
   if arg>=9999999999
    error(sprintf('Argument %i > 9999999999',arg))
   end
   iters=arg;
  else
   error(sprintf('Argument %i must be a positive integer',arg))
  end
 end
end

% Loop over each iteration
for iter=1:size(iters,2);
if iters(iter)>=0
 fname=sprintf('%s.%10.10i',fnamearg,iters(iter));
end

% Figure out if there is a path in the filename
NS=findstr('/',fname);
if size(NS)>0
 Dir=fname(1:NS(end));
else
 Dir='./';
end

% Match name of all meta-files
allfiles=dir( sprintf('%s*.meta',fname) );

if size(allfiles,1)==0
 disp(sprintf('No files match the search: %s.*.meta',fname));
%allow partial reads%  error('No files found.')
end

% Loop through allfiles
for j=1:size(allfiles,1);

% Read meta- and data-file
[A,N] = localrdmds([Dir allfiles(j).name],ieee);

bdims=N(1,:);
r0=N(2,:);
rN=N(3,:);
ndims=prod(size(bdims));
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

end % files
end % iterations

%-------------------------------------------------------------------------------

function [A,N] = localrdmds(fname,ieee)

mname=strrep(fname,' ','');
dname=strrep(mname,'.meta','.data');

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
  line=[line ' //']; ind=findstr(line,'//'); line=line(1:ind(1)-1);
% Add to total string
  allstr=[allstr line];
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
 allstr=[allstr(1:ind1(1)-1) allstr(ind2(1)+3:end)];
 ind1=findstr(allstr,'/*'); ind2=findstr(allstr,'*/');
end

% This is a kludge to catch whether the meta-file is of the
% old or new type. nrecords does not exist in the old type.
nrecords = -987;

% Everything in lower case
allstr=lower(allstr);

% Fix the unfortunate choice of 'format'
allstr=strrep(allstr,'format','dataprec');

% Evaluate meta information
eval(allstr);

N=reshape( dimlist , 3 , prod(size(dimlist))/3 );
if nrecords ~= -987 & nrecords > 1
 N=[N,[nrecords 1 nrecords]'];
end

if nrecords == -987
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
 if dataprec == 'float32'
  A=myrdda(dname,ldims,1,'real*4',ieee);
 elseif dataprec == 'float64'
  A=myrdda(dname,ldims,1,'real*8',ieee);
 else
  error(['Unrecognized format in meta-file = ' format]);
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
arr=reshape(arr,N);

%
function [iters] = scanforfiles(fname)

allfiles=dir([fname '.*.001.001.meta']);
if isempty(allfiles)
 allfiles=dir([fname '.*.meta']);
 ioff=0;
else
 ioff=8;
end
for k=1:size(allfiles,1);
 hh=allfiles(k).name;
 iters(k)=str2num( hh(end-14-ioff:end-5-ioff) );
end

