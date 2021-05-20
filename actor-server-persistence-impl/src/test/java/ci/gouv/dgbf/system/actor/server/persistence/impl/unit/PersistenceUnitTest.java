package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.RuntimeQueryStringBuilder;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
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
		assertThat(ScopeFunctionQuerier.getInstance().read().stream().map(ScopeFunction::getCode)
				.collect(Collectors.toList())).contains("A1000000");
	}
	
	@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	@Test
	public void localities_regions(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION))
				.stream().map(Locality::getCode).collect(Collectors.toList())).contains("r01","r02");
	}
	
	@Test
	public void localities_departmentsByRegionsIdentifiers(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"1"))
				.stream().map(Locality::getCode).collect(Collectors.toList())).isNotNull().contains("d01","d02");
		
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"7"))).isNull();
	}
	
	@Test
	public void localities_sousPrefecturesByDepartmentsIdentifiers(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"2"))
				.stream().map(Locality::getCode).collect(Collectors.toList())).contains("sp01","sp02");		
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