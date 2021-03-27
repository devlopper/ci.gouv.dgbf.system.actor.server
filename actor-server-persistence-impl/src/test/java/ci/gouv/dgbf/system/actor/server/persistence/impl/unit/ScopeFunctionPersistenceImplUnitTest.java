package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityReader;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
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
	
	/**/
	
	private void assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(String string,String expectedCode) {
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(string);
		assertThat(scopeFunction).as("No scope function starting with "+string+" not found").isNotNull();
		assertThat(scopeFunction.getCode()).as(scopeFunction.getCode()+" is not equal to "+expectedCode).isEqualTo(expectedCode);
	}
}