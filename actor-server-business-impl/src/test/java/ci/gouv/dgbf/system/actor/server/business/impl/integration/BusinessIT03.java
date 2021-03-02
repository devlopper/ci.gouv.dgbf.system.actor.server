package ci.gouv.dgbf.system.actor.server.business.impl.integration;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.business.server.EntityCreator;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityFinder;
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
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@RunWith(Arquillian.class)
public class BusinessIT03 extends AbstractObject {

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
        archive.addAsLibraries(Maven.resolver().loadPomFromFile("pom.xml").importTestDependencies().resolve().withTransitivity().asFile());
        return archive;
    }

    @Test
	public void create_profileType() throws Exception{
		assertThat(EntityCounter.getInstance().count(ProfileType.class)).isEqualTo(0l);
		ProfileType profileType = new ProfileType();
		profileType.setCode("P01");
		EntityCreator.getInstance().createMany(profileType);
		assertThat(EntityCounter.getInstance().count(ProfileType.class)).isEqualTo(1l);
		profileType = EntityFinder.getInstance().find(ProfileType.class, "P01");
		assertThat(profileType).isNotNull();
		assertThat(profileType.getIdentifier()).isEqualTo("P01");
		assertThat(profileType.getCode()).isEqualTo("P01");
		assertThat(profileType.getName()).isEqualTo("P01");
	}
}