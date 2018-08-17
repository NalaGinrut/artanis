;;; (redis commands) --- redis module for Guile.

;; Copyright (C) 2013-2018 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-redis.
;;
;; guile-redis is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License and
;; the GNU Lesser General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your
;; option) any later version.
;;
;; guile-redis is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License and the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; and the GNU Lesser General Public License along with guile-redis;
;; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Commentary:

;; Redis module for Guile

;;; Code:

(define-module (artanis third-party redis upstream commands)
  #:use-module (artanis third-party redis upstream utils)
  #:use-module (srfi srfi-9)
  #:export (redis-command?
            redis-cmd-name
            redis-cmd-params
            redis-cmd-reply))

(define-record-type <redis-command>
  (create-command name params reply)
  redis-command?
  (name redis-cmd-name)
  (params redis-cmd-params)
  (reply redis-cmd-reply))

(define* (make-command name #:rest args)
  (create-command name args read-reply))

(define-syntax create-commands
  (syntax-rules ()
    ((_ (cmd ...) ...)
     (eval
      `(begin
         ,@(map
            (lambda (args)
              (apply (lambda* (name #:rest subnames)
                       (let* ((cmd-name (string-join `(,name ,@subnames) " "))
                              (func-name (string->symbol
                                          (string-append "redis-"
                                                         (string-join `(,name ,@subnames)
                                                                      "-")))))
                         `(begin
                            (define* (,func-name #:rest params)
                              (apply make-command ,(string-upcase cmd-name) params))
                            (module-export! (current-module) '(,func-name)))))
                     args))
            `((,(symbol->string (syntax->datum #'cmd)) ...) ...)))
      (current-module)))))

(create-commands
 ;; Cluster
 (cluster addslots)
 (cluster count-failure-reports)
 (cluster countkeysinslot)
 (cluster delslots)
 (cluster failover)
 (cluster forget)
 (cluster getkeysinslot)
 (cluster info)
 (cluster keyslot)
 (cluster meet)
 (cluster nodes)
 (cluster replicate)
 (cluster reset)
 (cluster saveconfig)
 (cluster set-config-epoch)
 (cluster setslot)
 (cluster slaves)
 (cluster slots)
 (readonly)
 (readwrite)
 ;; Connection
 (auth)
 (echo)
 (ping)
 (quit)
 (select)
 (swapdb)
 ;; Geo
 (geoadd)
 (geohash)
 (geopos)
 (geodist)
 (georadius)
 (georadiusbymember)
 ;; Hashes
 (hdel)
 (hexists)
 (hget)
 (hgetall)
 (hincrby)
 (hincrbyfloat)
 (hkeys)
 (hlen)
 (hmget)
 (hmset)
 (hset)
 (hsetnx)
 (hstrlen)
 (hvals)
 (hscan)
 ;; HyperLogLog
 (pfadd)
 (pfcount)
 (pfmerge)
 ;; Keys
 (del)
 (dump)
 (exists)
 (expire)
 (expireat)
 (keys)
 (migrate)
 (move)
 (object)
 (persist)
 (pexpire)
 (pexpireat)
 (pttl)
 (randomkey)
 (rename)
 (renamenx)
 (restore)
 (sort)
 (touch)
 (ttl)
 (type)
 (unlink)
 (wait)
 (scan)
 ;; Lists
 (blpop)
 (brpop)
 (brpoplpush)
 (lindex)
 (linsert)
 (llen)
 (lpop)
 (lpush)
 (lpushx)
 (lrange)
 (lrem)
 (lset)
 (ltrim)
 (rpop)
 (rpoplpush)
 (rpush)
 (rpushx)
 ;; Pub/Sub
 (psubscribe)
 (pubsub)
 (publish)
 (punsubscribe)
 (subscribe)
 (unsubscribe)
 ;; Scripting
 (eval)
 (evalsha)
 (script debug)
 (script exists)
 (script flush)
 (script kill)
 (script load)
 ;; Server
 (bgrewriteaof)
 (bgsave)
 (client kill)
 (client list)
 (client getname)
 (client pause)
 (client reply)
 (client setname)
 (command)
 (command count)
 (command getkeys)
 (command info)
 (config get)
 (config rewrite)
 (config set)
 (config resetstat)
 (dbsize)
 (debug object)
 (debug segfault)
 (flushall)
 (flushdb)
 (info)
 (lastsave)
 (memory doctor)
 (memory help)
 (memory malloc-stats)
 (memory purge)
 (memory stats)
 (memory usage)
 (monitor)
 (role)
 (save)
 (shutdown)
 (slaveof)
 (slowlog)
 (sync)
 (time)
 ;; Sets
 (sadd)
 (scard)
 (sdiff)
 (sdiffstore)
 (sinter)
 (sinterstore)
 (sismember)
 (smembers)
 (smove)
 (spop)
 (srandmember)
 (srem)
 (sunion)
 (sunionstore)
 (sscan)
 ;; Sorted Sets
 (bzpopmin)
 (bzpopmax)
 (zadd)
 (zcard)
 (zcount)
 (zincrby)
 (zinterstore)
 (zlexcount)
 (zpopmax)
 (zpopmin)
 (zrange)
 (zrangebylex)
 (zrevrangebylex)
 (zrangebyscore)
 (zrank)
 (zrem)
 (zremrangebylex)
 (zremrangebyrank)
 (zremrangebyscore)
 (zrevrange)
 (zrevrangebyscore)
 (zrevrank)
 (zscore)
 (zunionstore)
 (zscan)
 ;; Streams
 (xadd)
 (xrange)
 (xrevrange)
 (xlen)
 (xread)
 (xreadgroup)
 (xpending)
 ;; Strings
 (append)
 (bitcount)
 (bitfield)
 (bitop)
 (bitpos)
 (decr)
 (decrby)
 (get)
 (getbit)
 (getrange)
 (getset)
 (incr)
 (incrby)
 (incrbyfloat)
 (mget)
 (mset)
 (msetnx)
 (psetex)
 (set)
 (setbit)
 (setex)
 (setnx)
 (setrange)
 (strlen)
 ;; Transactions
 (discard)
 (exec)
 (multi)
 (unwatch)
 (watch))

;;; (artanis third-party redis upstream commands) ends here
