package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import org.cyk.utility.test.business.server.AbstractUnitTest;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.junit.Test;
import org.junit.runner.RunWith;

import ci.gouv.dgbf.system.actor.server.business.impl.ApplicationScopeLifeCycleListener;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;

@RunWith(Arquillian.class)
public class BusinessIT {

    @Deployment
    public static Archive<?> buildArchive() {
        WebArchive archive = ShrinkWrap.create(WebArchive.class, "acteur-business.war");
        archive.addAsResource("META-INF/beans.xml");
        archive.addAsResource("META-INF/MANIFEST.MF");
        archive.addAsResource("memory/persistence.xml","META-INF/persistence.xml");
        archive.addAsResource("memory/data.sql","META-INF/data.sql");
        archive.addPackages(Boolean.TRUE, ApplicationScopeLifeCycleListener.class.getPackage());
        archive.addPackages(Boolean.TRUE, AbstractUnitTest.class.getPackage());
        archive.addAsLibraries(Maven.resolver().loadPomFromFile("pom.xml").importCompileAndRuntimeDependencies().resolve().withTransitivity().asFile());
        return archive;
    }

    @Test
    public void inContainer_persistence() throws Exception {
        System.out.println("THANKS LORDD ************************");
        System.out.println(ExpenditureNatureQuerier.getInstance().readAllForUI());
    }  
}