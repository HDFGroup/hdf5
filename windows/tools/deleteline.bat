@echo off
type nul > ..\temptest\fctemp\%1
more /e +%2 %1 > ..\temptest\fctemp\%1
