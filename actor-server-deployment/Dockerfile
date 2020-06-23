FROM adoptopenjdk/openjdk11-openj9:jre-11.0.3_7_openj9-0.14.0-alpine
ADD target/actor-server-deployment-0.1.0-thorntail.jar /opt/runnable.jar
EXPOSE 8080
ENTRYPOINT ["java", "-jar","-Djava.net.preferIPv4Stack=true","-Djava.net.preferIPv4Addresses=true", "-XX:MaxRAM=1G","/opt/runnable.jar"]

#FROM adoptopenjdk/openjdk11-openj9:jre-11.0.3_7_openj9-0.14.0-alpine
#ADD gitignore/thorntail/ojdbc6-11.2.0.4.jar /opt/ojdbc6-11.2.0.4.jar
#ADD gitignore/thorntail/hollow.jar /opt/hollow.jar
#ADD target/actor-server-deployment-0.1.0.war /opt/runnable.jar
#EXPOSE 8080
#ENTRYPOINT ["java","-Dthorntail.classpath=/opt/ojdbc6-11.2.0.4.jar", "-jar","-Djava.net.preferIPv4Stack=true","-Djava.net.preferIPv4Addresses=true", "-XX:MaxRAM=1G","/opt/hollow.jar","/opt/runnable.jar"]