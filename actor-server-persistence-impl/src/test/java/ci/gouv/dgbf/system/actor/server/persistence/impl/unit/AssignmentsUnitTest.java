package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class AssignmentsUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void dataCountIsGreaterThanZero(){
		assertThatCountIsGreaterThanZero(Boolean.FALSE,Section.class,Scope.class,Function.class,ScopeFunction.class,ExecutionImputation.class,Assignments.class);
	}
	
	@Test
	public void readWhereFilterUsingIdentifiersOnly_filterByActivityIdentifier(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64320000","64323000");
	}
	
	@Test
	public void readWhereFilterUsingIdentifiersOnly_filterByActivityIdentifierAndEconomicNatureCode(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE,"64323000");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
	}
	
	@Test
	public void readWhereFilterUsingIdentifiersOnly_filterByActivityIdentifierAndEconomicNature(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE,"64323000");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
		
		queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE,"64323");
		collection = AssignmentsQuerier.getInstance().readWhereFilterUsingIdentifiersOnly(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
	}
}