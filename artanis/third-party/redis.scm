;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2014,2015,2017
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Artanis is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License and GNU
;;  Lesser General Public License published by the Free Software
;;  Foundation, either version 3 of the License, or (at your option)
;;  any later version.

;;  Artanis is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License and GNU Lesser General Public License
;;  for more details.

;;  You should have received a copy of the GNU General Public License
;;  and GNU Lesser General Public License along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; JSON module for Guile

;;; Code:

(define-module (artanis third-party redis)
  #:use-module (artanis third-party redis upstream main)
  #:use-module ((artanis third-party redis upstream commands connection)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands hashes)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands keys)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands lists)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands publish)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands scripting)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands server)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands sets)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands sortedsets)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands strings)
                #:renamer (symbol-prefix-proc 'redis-))
  #:use-module ((artanis third-party redis upstream commands transactions)
                #:renamer (symbol-prefix-proc 'redis-))
  #:re-export (;; from main
               redis-connect
               redis-close
               redis-send

               ;; from connection
               redis-auth
               redis-echo
               redis-ping
               redis-quit
               redis-select

               ;; from hashes
               redis-hdel
               redis-hexists
               redis-hget
               redis-hgetall
               redis-hincrby
               redis-hincrbyfloat
               redis-hkeys
               redis-hlen
               redis-hmget
               redis-hmset
               redis-hset
               redis-hsetnx
               redis-hstrlen
               redis-hvals
               redis-hscan

               ;; from keys
               redis-del
               redis-dump
               redis-exists
               redis-expire
               redis-expireat
               redis-keys
               redis-migrate
               redis-move
               redis-object
               redis-persist
               redis-pexpire
               redis-pexpireat
               redis-pttl
               redis-randomkey
               redis-rename
               redis-renamenx
               redis-restore
               redis-ttl
               redis-type

               ;; from lists
               redis-blpop
               redis-brpop
               redis-brpoplpush
               redis-lindex
               redis-linsert
               redis-llen
               redis-lpop
               redis-lpush
               redis-lpushx
               redis-lrange
               redis-lrem
               redis-lset
               redis-ltrim
               redis-rpop
               redis-rpoplpush
               redis-rpush
               redis-rpushx

               ;; from publish
               redis-psubscribe
               redis-pubsub
               redis-publish
               redis-punsubscribe
               redis-subscribe
               redis-unsubscribe

               ;; scripting
               redis-eval
               redis-evalsha
               redis-script-exists
               redis-script-flush
               redis-script-kill
               redis-script-load

               ;; server
               redis-bgrewriteaof
               redis-bgsave
               redis-client-kill
               redis-client-list
               redis-client-getname
               redis-client-setname
               redis-config-get
               redis-config-set
               redis-config-resetstat
               redis-dbsize
               redis-debug-object
               redis-debug-segfault
               redis-flushall
               redis-flushdb
               redis-info
               redis-lastsave
               redis-monitor
               redis-save
               redis-shutdown
               redis-slaveof
               redis-slowlog
               redis-sync
               redis-time

               ;; from sets
               redis-sadd
               redis-scard
               redis-sdiff
               redis-sdiffstore
               redis-sinter
               redis-sinterstore
               redis-sismember
               redis-smembers
               redis-smove
               redis-spop
               redis-srandmember
               redis-srem
               redis-sunion
               redis-sunionstore

               ;; from sortedsets
               redis-zadd
               redis-zcard
               redis-zcount
               redis-zincrby
               redis-zrange
               redis-zrank
               redis-zrem
               redis-zremrangebyrank
               redis-zremrangebyscore
               redis-zrevrange
               redis-zrevrank
               redis-zscore

               ;; from strings
               redis-append
               redis-bitcount
               redis-bitop
               redis-decr
               redis-decrby
               redis-get
               redis-getbit
               redis-getrange
               redis-getset
               redis-incr
               redis-incrby
               redis-incrbyfloat
               redis-mget
               redis-mset
               redis-msetnx
               redis-psetex
               redis-set
               redis-setbit
               redis-setex
               redis-setnx
               redis-setrange
               redis-strlen

               ;; transactions
               redis-discard
               redis-exec
               redis-multi
               redis-unwatch
               redis-watch))
