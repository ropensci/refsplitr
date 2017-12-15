
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php
#	if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
#		$contents = '';
#		while (!feof($handle)) {
#			$contents .= fread($handle, 8192);
#		}
#		fclose($handle);
#		echo $contents; 
#	}
?>

<!-- end of project description -->
<h1>Welcome to refnet - Bibliometric Tools for R project!</h1>

<p>This is the <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">refnet</a> package for the <a href="http://www.r-project.org/">R statistical computing environment</a>. Our goal is to provide a package of tools for bibliometric analyses. This package and its tools are currently designed to read, analyze and visualize <a href="http://apps.webofknowledge.com/">Thomson-Reuters Web of Knowledge/Science, ISI</a> and <a href="http://www.scopus.com/home.url">SCOPUS</a> format reference data files. Social network analyses, geocoding of addresses and spatial visualization are supported.</p>

<img src="refnet_sample_output.png" alt="refnet sample output for publications from the UF Geography Department, 2010-2013" width="742" height="370" />

<p>We consider this work currently pre-alpha, however, as we progress we will continue to release source code and updates via <a href="http://r-forge.r-project.org/">R-Forge</a>.  If you would like more information, help or have comments regarding the code and package development please don't hesitate to email or <a href="http://www.clas.ufl.edu/users/forrest/">contact us</a>.

<p>
	<a href="http://www.clas.ufl.edu/users/forrest/">Forrest R. Stevens</a><br />
	Package Administrator<br />
	Ph.D. Candidate<br />
	Land Use and Environmental Change Institute (LUECI)<br />
	<a href="http://www.geog.ufl.edu/">Department of Geography, University of Florida</a><br />
</p>


<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
