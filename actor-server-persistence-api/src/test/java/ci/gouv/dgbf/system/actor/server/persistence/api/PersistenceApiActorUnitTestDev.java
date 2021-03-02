package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.IdentityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public class PersistenceApiActorUnitTestDev extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void readWhereFilter(){
		assertReadWhereFilter(null, null, null, null,"101",null, "michaeladjey@yahoo.fr","agae.apo@gmail.com");
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		assertReadWhereFilter(null, null, null, null,null,"01001", "agae.apo@gmail.com");
		assertReadWhereFilter(null, null, null, null,null,"05003", "agae.apo@gmail.com");
		assertReadWhereFilter(null, null, null, null,null,"13001", "michaeladjey@yahoo.fr","agae.apo@gmail.com");
		/*
		assertReadWhereFilter("admin", null, null, null,null,null, "admin");
		assertReadWhereFilter("u", null, null, null,null,null, "u01","u02","u03");
		assertReadWhereFilter("1", null, null, null,null,null, "u01");
		assertReadWhereFilter(null, "user", null, null,null,null, "u01","u02","u03");
		assertReadWhereFilter(null, null, null, "P1",null,null, "u01","u03");
		
		//QueryExecutor.AbstractImpl.LOG_LEVEL = Level.INFO;
		
		assertReadWhereFilter(null, null, null, null,"101",null, "u01","u03");
		assertReadWhereFilter(null, null, null, null,"327",null, "admin","u03");
		assertReadWhereFilter(null, null, null, null,"323",null, "u01","u02","u03");
		
		assertReadWhereFilter(null, null, null, null,null,"22001", "u01","u03");
		assertReadWhereFilter(null, null, null, null,null,"22002", "u01","u03");
		assertReadWhereFilter(null, null, null, null,null,"22003", "admin","u03");
		*/
	}
	
	/**/
	
	private void assertReadWhereFilter(String actorCode,String firstName,String lastNames,String profileCode,String visibleSectionCode
			,String visibleBudgetSpecializationUnitCode ,String...expectedCodes) {
		QueryExecutorArguments arguments = new QueryExecutorArguments().addFilterFieldsValues(
				IdentityQuerier.PARAMETER_NAME_CODE,actorCode,IdentityQuerier.PARAMETER_NAME_FIRST_NAME,firstName,IdentityQuerier.PARAMETER_NAME_LAST_NAMES,lastNames);
		if(StringHelper.isNotBlank(profileCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_PROFILE_CODE,profileCode);
		if(StringHelper.isNotBlank(visibleSectionCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_VISIBLE_SECTION_CODE,visibleSectionCode);
		if(StringHelper.isNotBlank(visibleBudgetSpecializationUnitCode))
			arguments.addFilterField(ActorQuerier.PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE,visibleBudgetSpecializationUnitCode);
		Collection<Actor> actors = ActorQuerier.getInstance().readWhereFilter(arguments);
		if(ArrayHelper.isEmpty(expectedCodes)) {
			assertThat(actors).as("actors found").isNull();
		}else {
			assertThat(actors).as("actors not found for parameters AC="+actorCode+", FN="+firstName+", LN="+lastNames+", PC="+profileCode+", VSC="+visibleSectionCode
					+", VUSB="+visibleBudgetSpecializationUnitCode).isNotNull();
			assertThat(actors.stream().map(x -> x.getCode()).collect(Collectors.toList())).containsExactly(expectedCodes);
		}
	}
}