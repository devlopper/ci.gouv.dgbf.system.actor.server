package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class ScopeFunctionPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void sectionQuerier_read(){
		assertThat(SectionQuerier.getInstance().read().stream().map(Section::getCode)
				.collect(Collectors.toList())).contains("101");
	}
	
	@Test
	public void functionQuerier_readExecutionHolders(){
		assertThat(FunctionQuerier.getInstance().readExecutionHolders(null).stream().map(Function::getCode)
				.collect(Collectors.toList())).containsExactly("GC","ORD","CF","CPT");
	}
	
	@Test
	public void functionQuerier_readExecutionHoldersAndAssistants(){
		assertThat(FunctionQuerier.getInstance().readExecutionHoldersAndAssistants(null).stream().map(Function::getCode)
				.collect(Collectors.toList())).containsExactly("GC","AGC","ORD","AORD","CF","ACF","CPT","ACPT");
	}
	
	@Test
	public void administrativeUnitQuerier_read(){
		assertThat(EntityReader.getInstance().readMany(AdministrativeUnit.class).stream().map(AdministrativeUnit::getCode)
				.collect(Collectors.toList())).contains("DTI");
	}
	
	@Test
	public void expenditureNatureQuerier_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).contains("1","2","3","4");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G1", "G100762");
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G2", "G200751");
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G3", "G301190");
	}
	
	@Test
	public void scopeFunctionPersistence_readByScopeIdentifierByFunctionIdentifier(){
		Collection<ScopeFunction> collection = ScopeFunctionQuerier.getInstance().readByScopeIdentifierByFunctionIdentifier(new QueryExecutorArguments()
				.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_SCOPE_IDENTIFIER,"UA68a6d9e7-420a-4bd9-9c01-12cfdad33fb9"
						,ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_IDENTIFIER,"GC"));
		assertThat(collection).hasSize(1);
		assertThat(collection.iterator().next().getCode()).isEqualTo("G100762");
	}
	
	@Test
	public void scopeFunctionPersistence_computeCreditManagerHolderName(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeCreditManagerHolderName("DTI"))
			.isEqualTo("Gestionnaire de crédits Direction des traitements informatiques");
	}
	
	@Test
	public void scopeFunctionPersistence_computeAuthorizingOfficerHolderName(){
		assertThat(__inject__(ScopeFunctionPersistence.class).computeAuthorizingOfficerHolderName("USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2",null))
		.isEqualTo("Ordonnateur délégué du Programme Budget");
		
		assertThat(__inject__(ScopeFunctionPersistence.class).computeAuthorizingOfficerHolderName("USB7d152f5a-3bcb-4ba3-a107-b680b6a230b2", "DIMBOKRO"))
			.isEqualTo("Ordonnateur secondaire du Programme Budget a Dimbokro");
	}
	
	@Test
	public void scopeFunctionQuerier_readActorsCodes(){
		Collection<ScopeFunction> scopeFunctions = EntityReader.getInstance().readMany(ScopeFunction.class, new QueryExecutorArguments()
				.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_CODES)));
		scopeFunctions.forEach(x -> {
			System.out.println(x.getActorsCodes());
		});
	}
	
	/**/
	
	private void assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(String string,String expectedCode) {
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(string);
		assertThat(scopeFunction).as("No scope function starting with "+string+" not found").isNotNull();
		assertThat(scopeFunction.getCode()).as(scopeFunction.getCode()+" is not equal to "+expectedCode).isEqualTo(expectedCode);
	}
}