call mvn clean package -P package.war,package.wildfly.bootable.jaxrs.oracle
set ORACLE_DATASOURCE=actor/server
set ORACLE_URL=jdbc:oracle:thin:@localhost:1521:XE
set ORACLE_USER=SIIBC_Actor
set ORACLE_PASSWORD=actor
call java -jar target\ROOT-bootable.jar --deployment=target\ROOT.war -Djboss.http.port=8081