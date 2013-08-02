((title . "About Artanis")
 (layouts "about.sxml")
 (authors "Mu Lei known as NalaGinrut <nalaginrut@gmail.com>")
 (date . 1374652887))

About Artanis
=============

* What is Artanis?

Artanis is used to generate HTML pages dynamically, which is also called a web-framework.

It contains URL-remap(the so-called "URL Router") to provides the [RESTful](https://en.wikipedia.org/wiki/Restful)
URL. The "URL Router" is some kind of [Rewrite engine](http://en.wikipedia.org/wiki/Rewrite_engine), which adds a 
layer of abstraction between the files used to generate a web page and the URL that is presented to the outside world.

Besides, it also provides [Object Relational Mapping](https://en.wikipedia.org/wiki/Object-relational_mapping "what's it?")
which is known that map some kind of object to a table in database. 

* Who write it?

Mu Lei known as NalaGinrut who is a Scheme-web-mad, which means the guy who choose to die if he can't use Scheme to build a website.

* Why write it?

Seriously, Artanis is written with [GNU Guile](http://www.gnu.org/software/guile/), one of the best implementation of Scheme language.
Someday GNU guys talked about "what language to write GNU website", many guys choose Python. But I think it's strange because GNU Guile
**IS** the official extension language of GNU. And I asked why not start a brand new project named GLOW(Guile Launch On Web) to provide
a full-stack web-framework written with GNU Guile, and RMS said "it's cool, I like this idea". But at that time, it's just an idea without any plan.

Fortunately, few months later, Guile community holds the [potluck party](http://lists.gnu.org/archive/html/guile-user/2013-01/msg00007.html) 
to celibrate 2 years birthday of Guile2. It's actually a contest to write cool program in several weeks. So, Artanis was born.

Artanis is the core of GLOW, which provides the most of low-level web handler, and other useful API. However, Artanis could live standalone
as a self-contained lightweight web-framework.
Another project named "Ricecat" is the front-end of GLOW, which provides configurations, MVC, automatically template generation...Think about how you
play Ruby-on-Rails.

And any other related project could be work with them, just like [redis](https://github.com/aconchillo/guile-redis), 
[dbi](http://home.gna.org/guile-dbi/), [json](https://github.com/aconchillo/guile-json)...
All of them will compose the so-called GLOW.


