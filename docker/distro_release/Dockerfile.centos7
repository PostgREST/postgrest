FROM centos:centos7

RUN yum -y update
RUN yum -y install perl make automake gcc gmp-devel libffi zlib zlib-devel xz tar
RUN yum -y install yum install https://download.postgresql.org/pub/repos/yum/9.3/redhat/rhel-7-x86_64/pgdg-centos93-9.3-2.noarch.rpm
RUN yum -y install postgresql93-devel
RUN yum clean all
RUN curl -sSL https://get.haskellstack.org/ | sh

ENV PATH $PATH:/usr/pgsql-9.3/bin

# To disable warning when building
ENV PATH $PATH:/root/.local/bin

RUN mkdir /source
WORKDIR /source

ENTRYPOINT ["stack"]
