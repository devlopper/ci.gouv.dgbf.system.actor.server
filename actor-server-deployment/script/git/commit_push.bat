@echo off
echo ################################################## COMMITTING AND PUSHING FROM %1 TO %2 ##################################################
echo #### timestamp computation
FOR /f "skip=1" %%x in ('wmic os get localdatetime') do if not defined MyDate set MyDate=%%x
FOR /f %%x in ('wmic path win32_localtime get /format:list ^| findstr "="') do set %%x
SET fmonth=00%Month%
SET fday=00%Day%
SET mydate=%Year%%fmonth:~-2%%fday:~-2%
For /f "tokens=1-2 delims=/:" %%a in ("%TIME%") do (set mytime=%%a%%b)
set mytime=%mytime: =0%
SET timestamp=v0.0.0-%mydate%%mytime%

SET commit_message="actor server using bootable publishing %timestamp%"

echo #### %commit_message%
git add .
git commit --all -m %commit_message%
git push -u origin %1:%2
git push mic-acteur-api %1:%2