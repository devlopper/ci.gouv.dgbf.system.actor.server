package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.persistence.query.EntityCreator;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceApiAdministrativeUnitUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		ApplicationScopeLifeCycleListener.initialize();//TODO it is not working when removed
		//org.cyk.utility.__kernel__.persistence.query.QueryExecutor.AbstractImpl.LOG_LEVEL = java.util.logging.Level.INFO;
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void readWhereCodeOrNameLike(){
		assertReadWhereCodeOrNameLike(null, null, "1101","1102");
		assertReadWhereCodeOrNameLike("1102", null, "1102");
		assertReadWhereCodeOrNameLike(null, "N", "1101","1102");
		assertReadWhereCodeOrNameLike("200", "200", new String[] {});
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(
			new Section().setCode("101").setName("R. N.")
			,new Section().setCode("327").setName("M. B. N.")
		);
		
		EntityCreator.getInstance().createManyInTransaction(
			new AdministrativeUnit().setCode("1101").setSectionFromIdentifier("101")
			,new AdministrativeUnit().setCode("1102").setSectionFromIdentifier("327")
		);
	}
	
	private void assertReadWhereCodeOrNameLike(String code,String name,String...codes){
		Collection<AdministrativeUnit> administrativeUnits = AdministrativeUnitQuerier.getInstance().readWhereCodeOrNameLike(new QueryExecutorArguments()
				.setQueryFromIdentifier(QueryIdentifierGetter.getInstance().get(AdministrativeUnit.class, QueryName.READ_WHERE_CODE_OR_NAME_LIKE))
				.addFilterFieldsValues(AdministrativeUnitQuerier.PARAMETER_NAME_CODE,code,AdministrativeUnitQuerier.PARAMETER_NAME_NAME,name));
		if(ArrayHelper.isEmpty(codes)) {
			assertThat(administrativeUnits).isNull();
		}else {
			assertThat(administrativeUnits).isNotNull();
			assertThat(administrativeUnits.stream().map(x -> x.getCode()).collect(Collectors.toList())).contains(codes);	
		}
	}
}