if (-not (Test-Path env:INKSCAPE_HOME)) { $env:INKSCAPE_HOME = 'C:\Program Files\Inkscape' }
Get-ChildItem "..\src\main\resources\se\kth\speech\coin\tangrams\content\images\icons" -Filter *.svg | 
Foreach-Object {
	iex $env:INKSCAPE_HOME\inkscape -z -f "$_.name" -w 300 -e "$_.name.png"
}