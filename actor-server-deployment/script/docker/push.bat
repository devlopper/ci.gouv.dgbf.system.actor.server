@echo off
for /f "skip=1" %%x in ('wmic os get localdatetime') do if not defined MyDate set MyDate=%%x
for /f %%x in ('wmic path win32_localtime get /format:list ^| findstr "="') do set %%x
set fmonth=00%Month%
set fday=00%Day%
set mydate=%Year%%fmonth:~-2%%fday:~-2%
echo %mydate=%

For /f "tokens=1-2 delims=/:" %%a in ("%TIME%") do (set mytime=%%a%%b)
set mytime=%mytime: =0%

SET timestamp=v0.0.0-%mydate%%mytime%
echo Publishing with time stamp : %timestamp% >> push_outputs.txt
docker tag mic-acteur-api 10.3.4.18:5000/mic-acteur-api:%timestamp% >> push_outputs.txt
docker push 10.3.4.18:5000/mic-acteur-api:%timestamp% >> push_outputs.txt

REM push under latest
call docker tag mic-acteur-api 10.3.4.18:5000/mic-acteur-api
call docker push 10.3.4.18:5000/mic-acteur-api