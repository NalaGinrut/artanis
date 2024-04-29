FROM        debian:sid-slim
MAINTAINER  Mu Lei
ENV         LANG C.UTF-8
RUN     apt-get update \
        && apt-get install --no-install-recommends -y texinfo guile-3.0 guile-3.0-dev build-essential automake git autoconf libtool libmariadbd-dev libnss3 libnss3-dev redis redis-server \
        && apt-get install --no-install-recommends -y libcurl4-openssl-dev gettext

ARG CACHE_DBI=1
ENV GIT_SSL_NO_VERIFY 1
RUN set -ex \
        && git clone --depth 1 https://github.com/opencog/guile-dbi \
        && cd guile-dbi \
        && git checkout guile-dbi-2.1.8 \
        && cd guile-dbi && ./autogen.sh && ./configure --prefix=/usr \
        && make -j \
        && make install && ldconfig && cd .. \
        && git checkout guile-dbd-mysql-2.1.8 \
        && cd guile-dbd-mysql \
        && ./autogen.sh && ./configure --prefix=/usr \
        && make -j \
        && make install && ldconfig && cd ../../ && rm -fr guile-dbi

ARG CACHE_CURL=1
RUN set -ex \
        && git clone --depth 1 https://github.com/spk121/guile-curl \
        && cd guile-curl && ./bootstrap && ./configure --prefix=/usr \
        && make -j \
        && make install && ldconfig && cd .. \
        && ln -s /usr/lib/guile/3.0/extensions/libguile-curl.* /usr/lib/ \
        && rm -fr guile-curl

ARG CACHE_ARTANIS=1
RUN     git clone --depth 1 --single-branch --branch master git://git.savannah.gnu.org/artanis.git \
        && cd artanis \
        && ./autogen.sh \
        && ./configure --prefix=/usr \
        && make -j \
        && make install && cd .. && rm -fr artanis
