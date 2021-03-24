package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.RuntimeQueryStringBuilder;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class PersistenceUnitTest extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void sectionQuerier_readAll(){
		assertThat(SectionQuerier.getInstance().read().stream().map(Section::getCode)
				.collect(Collectors.toList())).contains("101");
	}
	
	@Test
	public void scopeFunctionQuerier_readAll(){
		System.out.println("SCOPE TYPE : "+EntityCounter.getInstance().count(ScopeType.class));
		System.out.println("SCOPE : "+EntityCounter.getInstance().count(Scope.class));
		System.out.println("FUNCTION : "+EntityCounter.getInstance().count(Function.class));
		System.out.println("SCOPE FUNCTION : "+EntityCounter.getInstance().count(ScopeFunction.class));		
		System.out.println("EX IMP : "+EntityCounter.getInstance().count(ExecutionImputation.class));
		System.out.println("AFF : "+EntityCounter.getInstance().count(Assignments.class));
		
		assertThat(ScopeFunctionQuerier.getInstance().read().stream().map(ScopeFunction::getCode)
				.collect(Collectors.toList())).contains("101");
	}
	
	@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	/**/
	
	//@Test
	public void assignmentsQueryStringBuilder_build_where_noCriteria(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
		//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
		//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		System.out.println("PersistenceUnitTest.assignmentsQueryStringBuilder_build_where_noCriteria()");
		System.out.println(RuntimeQueryStringBuilder.getInstance().build(queryExecutorArguments));
	}
	
	//@Test
	public void assignmentsQueryStringBuilder_build_where_sectionIdentifier(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_SECTION_IDENTIFIER,"1");
		//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
		//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		System.out.println("PersistenceUnitTest.assignmentsQueryStringBuilder_build_where_noCriteria()");
		System.out.println(RuntimeQueryStringBuilder.getInstance().build(queryExecutorArguments));
	}
	
	//@Test
	public void assignmentsQueryStringBuilder_build_where_allHoldersDefined(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE,Boolean.FALSE);
		System.out.println("PersistenceUnitTest.assignmentsQueryStringBuilder_build_where_allHoldersDefined()");
		System.out.println(RuntimeQueryStringBuilder.getInstance().build(queryExecutorArguments));
	}
	
	//@Test
	public void assignmentsQueryStringBuilder_build_where_someHoldersNotDefined(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQueryFromIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE,Boolean.FALSE);
		System.out.println("PersistenceUnitTest.assignmentsQueryStringBuilder_build_where_someHoldersNotDefined()");
		System.out.println(RuntimeQueryStringBuilder.getInstance().build(queryExecutorArguments));
	}
}