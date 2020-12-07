FROM adoptopenjdk/openjdk11-openj9:jre-11.0.3_7_openj9-0.14.0-alpine
ENV SIIB_DB_HOST sib-database
ENV SIIB_DB_PORT 1521
ENV SIIB_DB_SID SIB
ENV SIIB_DB_USER acteur
ENV SIIB_DB_PASSWORD acteur
ENV KEYCLOAK_REALM SIIBTEST
#ENV security.single.sign.on.system.keycloak.prod.realm.name SIIBTEST
ENV security.single.sign.on.system.keycloak.prod.client.identifier admin-cli
ENV security.single.sign.on.system.keycloak.prod.client.secret ac0749e3-14f8-4a14-a6e1-4edfb9375e69
ENV security.single.sign.on.system.keycloak.prod.credential.username mic-acteur-api
ENV security.single.sign.on.system.keycloak.prod.credential.password mic-@cteur-@pi@2O2o
ADD gitignore/thorntail/hollow.jar /opt/hollow.jar
ADD gitignore/thorntail/project-defaults.yml /opt/project-defaults.yml
ADD target/actor-server-deployment-0.1.0.war /opt/runnable.war
EXPOSE 8080
ENTRYPOINT ["java", "-jar","-Djava.net.preferIPv4Stack=true","-Djava.net.preferIPv4Addresses=true", "-XX:MaxRAM=1G","/opt/hollow.jar","/opt/runnable.war","-s","/opt/project-defaults.yml"]