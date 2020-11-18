call mvn clean package -P prod,swagger
set ORACLE_DATASOURCE=ActorDataSource
set ORACLE_URL=jdbc:oracle:thin:@localhost:1521:XE
set ORACLE_USER=SIIBC_Actor
set ORACLE_PASSWORD=actor
call java -jar target\ROOT-bootable.jar --deployment=target\ROOT.war