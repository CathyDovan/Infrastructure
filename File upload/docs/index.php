<!DOCTYPE html>
<html>
<head>
<title>INFC All Docs</title>
<meta charset="utf-8" />
</head>

<body>
<h1>INFC Docs</h1>
<a href="../docs-zipped/" style="padding:20px;font-size:1.2em">Download all as zip</a>
<?php
function show_files($start) {
    $contents = scandir($start);
    array_splice($contents, 0,2);
	usort($contents, function($a, $b){
        return filemtime($a) < filemtime($b);
    });
    echo "<ul>";
    foreach ( $contents as $item ) {
		$outputitem = "<li>".date('F d Y H:i:s', filemtime($item))." - <a href='$item'>$item</a></li>";
		if ($item != "index.php") {
        if ( is_dir("$start/$item") && (substr($item, 0,1) != '.') ) {
            echo $outputitem;
            show_files("$start/$item");
        } else {
            echo $outputitem;
        }
		}
    }
    echo "</ul>";
}

show_files('./');
?>
</body>
</html>