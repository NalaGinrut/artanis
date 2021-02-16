FROM        debian:sid-slim
MAINTAINER  Mu Lei
ENV         LANG C.UTF-8
RUN     apt-get update \
        && apt-get install -y texinfo guile-3.0 guile-3.0-dev build-essential automake git autoconf libtool libmariadbclient-dev libmariadbd-dev libnss3 libnss3-dev redis redis-server \
        && rm -rf /var/lib/apt/lists/*

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
RUN     git clone --depth 1 --single-branch --branch master git://git.savannah.gnu.org/artanis.git \
        && cd artanis \
	&& ./autogen.sh \
	&& ./configure \
	&& make -j \
        && make install && cd .. && rm -fr artanis
