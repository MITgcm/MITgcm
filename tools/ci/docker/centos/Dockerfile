FROM centos:centos7
LABEL maintainer=mitgcm-devel@mitgcm.org

RUN yum -y update; yum clean all
RUN yum -y install epel-release wget csh; yum clean all
RUN yum -y groupinstall "Development Tools"
RUN cd /root; wget http://www.mcs.anl.gov/%7Eutke/OpenAD_tars/493/OpenAD_2014-03-15.tgz; tar -xzvf OpenAD_2014-03-15.tgz
COPY pfile /root/pfile
RUN cd /root/OpenAD; patch openadConfig.py ../pfile; pwd ; export PATH=".":$PATH; source setenv.sh; make
RUN cd /root/OpenAD; export PATH=".:"${PATH}; ./tools/setenv/setenv.py --shell=sh > setenv.tmp
RUN cd /root/OpenAD; cp setenv.tmp  /etc/profile.d/openad.sh
