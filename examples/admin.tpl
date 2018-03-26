<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <title> <%= blog-title %> </title>
    <link rel="alternate" type="application/atom+xml" title="Atom 1.0" href="atom/1" />
    <link rel="stylesheet" type="text/css" media="screen" href="css/common.css" />
  </head>

  <body>
    <p>edit your article</p>
    <form id="post_article" action="/new_post" method="POST">
      <p>title:</p>
      <input type="text" name="title"/></br>
      <p>content:</p>
      <textarea name="content" rows="25" cols="38">write something</textarea></br>
      <input type="submit" value="Submit"/>
    </form>
  </body>
</html>

