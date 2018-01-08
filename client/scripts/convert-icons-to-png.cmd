REM This file is part of Tangrams-restricted.

REM Tangrams-restricted is free software: you can redistribute it and/or modify
REM it under the terms of the GNU General Public License as published by
REM the Free Software Foundation, either version 3 of the License, or
REM (at your option) any later version.

REM This program is distributed in the hope that it will be useful,
REM but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
REM GNU General Public License for more details.

REM You should have received a copy of the GNU General Public License
REM along with this program.  If not, see <http://www.gnu.org/licenses/>.

REM A script for converting all SVG icon files into PNG using Inkscape.

REM Author: Todd Shore <errantlinguist+github@gmail.com>
REM Since: 2017-03-07

IF "%INKSCAPE_HOME%"=="" SET INKSCAPE_HOME=C:\Program Files\Inkscape\
FOR %I IN (%USERPROFILE%\*) DO @ECHO %I
FOR %I IN (..\src\main\resources\se\kth\speech\coin\tangrams\content\images\icons\*) DO @ECHO %I