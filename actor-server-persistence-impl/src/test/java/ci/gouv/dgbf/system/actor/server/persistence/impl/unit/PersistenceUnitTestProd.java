package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.test.persistence.server.AbstractUnitTest;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class PersistenceUnitTestProd extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ci.gouv.dgbf.system.actor.server.persistence.api.ApplicationScopeLifeCycleListener.initialize();
		//ApplicationScopeLifeCycleListener.initialize();
		//SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "prod";
	}
	
	@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	@Test
	public void request_readTransients(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(Request.FIELD_ACCEPTED,Request.FIELD_ACCOUNT_CREATION_MESSAGE));
		queryExecutorArguments.setNumberOfTuples(100);
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		requests.stream().forEach(x -> {
			if(StringHelper.isNotBlank(x.getAccountCreationMessage()))
				System.out.println(x.getCode()+" | "+x.getAccepted()+" | "+x.getAccountCreationMessage());
		});
	}
	
	@Test
	public void executeNativeSql_V_APP_EX_CPT_ERREUR(){
		@SuppressWarnings("unchecked")
		List<Object[]> arrays = EntityManagerGetter.getInstance().get().createNativeQuery("SELECT email,message FROM V_APP_EX_CPT_ERREUR").getResultList();
		arrays.forEach(array -> {
			System.out.println(array[0]+" - "+array[1]);
		});
	}
	
	@Test
	public void requestQuerier_readAccountCreationMessagesByElectronicMailAddresses(){
		//VariableHelper.write(VariableName.VIEW_USER_ACCOUNT_AVAILABLE, Boolean.TRUE);
		Collection<Object[]> arrays = RequestQuerier.getInstance().readFromViewByElectronicMailAddresses("mariekoni2018@gmail.com");
		if(CollectionHelper.isEmpty(arrays))
			return;
		arrays.forEach(array -> {
			System.out.println(array[0]+" - "+array[1]);
		});
	}
}