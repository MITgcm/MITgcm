FROM ubuntu:latest

RUN apt-get update
RUN apt-get install -y openjdk-11-jre
RUN apt-get install -y gcc
RUN apt-get install -y gfortran
RUN apt-get install -y git-all
RUN apt-get install -y wget
RUN apt-get install -y tar
RUN apt-get install -y make
RUN apt-get install -y libc6

RUN wget https://tapenade.gitlabpages.inria.fr/tapenade/distrib/tapenade_3.16.tar
RUN tar -xvf tapenade_3.16.tar

ENV PATH=$PATH:/tapenade_3.16/bin
ENV _JAVA_OPTIONS=-Xmx1g
