@echo off
cd /d "%~dp0" 
..\..\compro-pp\bin\Debug\net6.0\compro-pp.exe test-source.cpp -o test-output.cpp -hd test-header.cpp
