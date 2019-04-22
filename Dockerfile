FROM        debian:buster
MAINTAINER  Mu Lei
ENV         LANG C.UTF-8
ENV         ARTANIS_VERSION latest
ENV         GUILE_DBI_VERSION 2.1.7
ENV         GUILE_DBD_MYSQL_VERSION 2.1.6
RUN     echo "deb http://mirrors.ustc.edu.cn/debian jessie main contrib non-free" >> /etc/apt/sources.list \
        && echo "deb-src http://mirrors.ustc.edu.cn/debian jessie main contrib non-free" >> /etc/apt/sources.list
RUN         apt-get update && apt-get install -y --no-install-recommends guile-2.2 guile-2.2-dev libmariadbclient-dev mariadb-server \
                          && rm -rf /var/lib/apt/lists/*

RUN set -ex \
        wget -c http://download.gna.org/guile-dbi/guile-dbi-$GUILE_DBI_VERSION.tar.gz \
        && tar xvzf guile-dbi-$GUILE_DBI_VERSION.tar.gz \
        && rm -f guile-dbi-$GUILE_DBI_VERSION.tar.gz \
        && cd guile-dbi-$GUILE_DBI_VERSION && ./configure && make \
        && make install && ldconfig && rm -fr * \
        \
        && wget -c http://download.gna.org/guile-dbi/guile-dbd-mysql-$GUILE_DBD_MYSQL_VERSION.tar.gz \
        && tar xvzf guile-dbd-mysql-$GUILE_DBD_MYSQL_VERSION.tar.gz \
        && rm -f guile-dbd-mysql-$GUILE_DBD_MYSQL_VERSION.tar.gz \
        && cd guile-dbd-mysql-$GUILE_DBD_MYSQL_VERSION && ./configure && make \
        && make install && ldconfig && rm -fr *
        \
	./autogen.sh \
	&& ./configure \
	&& make -j \
        && make install && rm -fr *

