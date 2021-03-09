package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityCreator;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsQueryStringBuilder;

public class PersistenceUnitTest extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	@Test
	public void expenditureNature_readAllForUI_01(){
		EntityCreator.getInstance().createManyInTransaction(new ExpenditureNature().setCode("5"));
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4","5");
	}
	
	@Test
	public void expenditureNature_readAllForUI_02(){
		EntityCreator.getInstance().createManyInTransaction(new ExpenditureNature().setCode("6"),new ExpenditureNature().setCode("7"));
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4","6","7");
	}
	
	@Test
	public void assignmentsQueryStringBuilder_build(){
		AssignmentsQueryStringBuilder.Arguments arguments = new AssignmentsQueryStringBuilder.Arguments();
		arguments.setImputationFieldsNames(List.of(ExecutionImputation.FIELD_SECTION_CODE));
		arguments.setJoinableHoldersFieldsNames(List.of(Assignments.FIELD_CREDIT_MANAGER_HOLDER));
		assertThat(AssignmentsQueryStringBuilder.getInstance().build(arguments)).isEqualTo("");
	}
}