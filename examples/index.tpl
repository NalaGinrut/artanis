<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title> <%= blog-title %> </title>
    <link rel="alternate" type="application/atom+xml" title="Atom 1.0" href="atom/1" />
    <link rel="stylesheet" type="text/css" media="screen" href="css/common.css" />

  </head>

  <body>

    <div id="skiplinks">
      <a href="#navigation">Jump to navigation</a>
      <a href="#content">Jump to main content</a>
      <a href="#searchform">Jump to search form</a>
    </div>

    <div id="container">
      <h1><a href="/"> <%= blog-title %> </a></h1>
      <div id="main">
        <%= all-posts %>
      </div>

      <div id="sidebar">
        <form method="get" id="searchform" action="/search">
          <div><label for="s">Search</label>
            <input type="text" id="s" name="criteria" value=""></div>
          <div><button>search</button></div>
        </form>

        <form method="get" id="admin" action="/admin">
          <div><button>admin</button></div>
        </form>

        <div id="navigation">
          <h3>Navigation</h3>
          <ul>
          </ul>
        </div>
      </div>

      <div class="clear">
      </div>
    </div>

    <%= footer %>
  </body>
</html>
