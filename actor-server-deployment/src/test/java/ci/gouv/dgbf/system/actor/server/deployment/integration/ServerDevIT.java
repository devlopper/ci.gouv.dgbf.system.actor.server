package ci.gouv.dgbf.system.actor.server.deployment.integration;

import java.net.URL;

import org.cyk.utility.__kernel__.DependencyInjection;
import org.cyk.utility.representation.EntityReader;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.container.test.api.RunAsClient;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.jboss.shrinkwrap.api.Archive;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.jboss.shrinkwrap.resolver.api.maven.Maven;
import org.junit.Test;
import org.junit.runner.RunWith;

import ci.gouv.dgbf.system.actor.server.business.api.AssignmentsBusiness;
import ci.gouv.dgbf.system.actor.server.deployment.ServletContextListener;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.representation.entities.ExpenditureNatureDto;

@RunWith(Arquillian.class)
public class ServerDevIT {

    @Deployment
    public static Archive<?> buildArchive() {
        WebArchive archive = ShrinkWrap.create(WebArchive.class, "acteur.war");
        archive.addAsResource("META-INF/beans.xml");
        archive.addAsResource("META-INF/MANIFEST.MF");
        archive.addAsResource("META-INF/microprofile-config.properties");
        archive.addAsResource("META-INF/persistence.xml");
        archive.addAsResource("ehcache.xml");
        archive.addClass(ServletContextListener.class);
        // Import compile and runtime dependencies
        archive.addAsLibraries(Maven.resolver()
                    .loadPomFromFile("pom.xml")
                    .importCompileAndRuntimeDependencies()
                    .resolve()
                    .withTransitivity()
                    .asFile());
        return archive;
    }

    @Test
    @RunAsClient
    public void asClient(@ArquillianResource URL url) throws Exception {
        //System.out.println("HollowJarITCase.asClient()");
        //System.out.println(url);
        //System.out.println(org.cyk.utility.controller.EntityReader.getInstance().readMany(ExpenditureNature.class.getName(), ExpenditureNatureQuerier.QUERY_IDENTIFIER_READ_ALL_FOR_UI
        //		, null, null, null, null, null));
    }

    @Test
    public void inContainer_persistence() throws Exception {
        //System.out.println("HollowJarITCase.inContainer_persistence()");
        System.out.println(ExpenditureNatureQuerier.getInstance().readAllForUI());
    }
    
    @Test
    public void inContainer_business() throws Exception {
        DependencyInjection.inject(AssignmentsBusiness.class).export("TEST");
    }
    
    @Test
    public void inContainer_representation() throws Exception {
        //System.out.println("HollowJarITCase.inContainer_representation()");
        System.out.println(EntityReader.getInstance().read(ExpenditureNatureDto.class.getName(), ExpenditureNatureQuerier.QUERY_IDENTIFIER_READ_ALL_FOR_UI
        		, null, null, null, null, null));
    }

}