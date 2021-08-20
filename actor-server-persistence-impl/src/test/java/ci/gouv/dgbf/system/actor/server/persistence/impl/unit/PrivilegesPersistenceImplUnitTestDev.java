package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.util.Collection;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public class PrivilegesPersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		EXCAT = Boolean.FALSE;
	}

	@Test
	public void read(){
		QueryExecutorArguments arguments = new QueryExecutorArguments();
		arguments.setQuery(new Query().setIdentifier(ProfileQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		arguments.addProcessableTransientFieldsNames(Profile.FIELD_NUMBER_OF_ACTORS);
		Collection<Profile> profiles = EntityReader.getInstance().readMany(Profile.class, arguments);
		profiles.forEach(p -> {
			System.out.println(p.getCode()+" : "+p.getNumberOfActors());
		});
	}
}