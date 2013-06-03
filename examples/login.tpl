<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title> <%= blog-title %> </title>
    <link rel="alternate" type="application/atom+xml" title="Atom 1.0" href="atom/1" />
    <link rel="stylesheet" type="text/css" media="screen" href="css/common.css" />

  </head>

  <body>

    <div id="container">
      <h1><a href="/"> <%= blog-title %> </a></h1>
      <div id="main">
      <form id="login" action="/auth" method="POST">
            <p>user name: <input type="text" name="user"></p>
            <p>password : <input type="password" name="passwd"></p>
            <input type="submit" value="Submit"> 
            <input type="checkbox" name="remember_me">Remember me
      </form>
      </div>
    </div>

    <%= footer %>
  </body>
</html>
