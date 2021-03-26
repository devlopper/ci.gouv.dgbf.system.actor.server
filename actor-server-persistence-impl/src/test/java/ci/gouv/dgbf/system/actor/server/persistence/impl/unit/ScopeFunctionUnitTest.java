package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class ScopeFunctionUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void sectionQuerier_read(){
		assertThat(SectionQuerier.getInstance().read().stream().map(Section::getCode)
				.collect(Collectors.toList())).contains("101");
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
	
	/**/
	
	private void assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith(String string,String expectedCode) {
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readMaxCodeWhereCodeStartsWith(string);
		assertThat(scopeFunction).as("No scope function starting with "+string+" not found").isNotNull();
		assertThat(scopeFunction.getCode()).as(scopeFunction.getCode()+" is not equal to "+expectedCode).isEqualTo(expectedCode);
	}
}