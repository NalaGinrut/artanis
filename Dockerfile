FROM        debian:buster
MAINTAINER  Mu Lei
ENV         LANG C.UTF-8
RUN     echo "deb http://mirrors.ustc.edu.cn/debian buster main contrib non-free" > /etc/apt/sources.list \
        && echo "deb-src http://mirrors.ustc.edu.cn/debian buster main contrib non-free" >> /etc/apt/sources.list
RUN     apt-get update \
        && apt-get install -y --no-install-recommends libnss3 wget libmariadbclient-dev mariadb-server git ca-certificates lzip \
        && apt-get build-dep -y --no-install-recommends guile-2.2 \
        && rm -rf /var/lib/apt/lists/*

ARG CACHE_GUILE=1
RUN set -ex \
       && wget http://ftp.gnu.org/gnu/guile/guile-2.2.7.tar.lz \
       && tar xvf guile-2.2.7.tar.lz \
       && cd guile-2.2.7 \
       && ./configure --prefix=/usr \
       && make -j \
       && make install && ldconfig && cd .. && rm -fr guile-2.2.7

ARG CACHE_DBI=1
RUN set -ex \
        && git clone --depth 1 git://github.com/opencog/guile-dbi.git \
        && cd guile-dbi/guile-dbi && ./autogen.sh && ./configure && make -j \
        && make install && ldconfig && cd .. \
        \
        && cd guile-dbd-mysql \
        && ./autogen.sh && ./configure && make -j \
        && make install && ldconfig && cd ../../ && rm -fr guile-dbi

ARG CACHE_ARTANIS=1
RUN     git clone --depth 1 git://git.savannah.gnu.org/artanis.git \
        && cd artanis \
	&& ./autogen.sh \
	&& ./configure \
	&& make -j \
        && make install && cd .. && rm -fr artanis
