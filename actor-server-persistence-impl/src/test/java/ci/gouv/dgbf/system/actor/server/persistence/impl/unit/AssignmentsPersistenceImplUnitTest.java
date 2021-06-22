package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.persistence.query.EntityCreator;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.EntityUpdater;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.audit.AuditReader;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class AssignmentsPersistenceImplUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void dataCountIsGreaterThanZero(){
		assertThatCountIsGreaterThanZero(Boolean.FALSE,Section.class,Scope.class,Function.class,ScopeFunction.class,ExecutionImputation.class,Assignments.class);
	}
	
	@Test
	public void countDynamic(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC));
		Long count = AssignmentsQuerier.getInstance().count(queryExecutorArguments);
		assertThat(count).isGreaterThan(1l);
	}
	
	@Test
	public void readDynamic_filterByActivityIdentifier(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64320000","64323000");
		collection.forEach(x -> assertThat(x.getCreditManagerHolder()).isNull());
		collection.forEach(x -> assertThat(x.getCreditManagerHolderAsString()).isNotNull());
	}
	
	@Test
	public void readDynamic_filterByActivityIdentifier_holdersAreNotNull(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY_WITHOUT_SCOPE_FUNCTIONS);
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_HOLDERS);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64320000","64323000");
		collection.forEach(x -> {
			assertThat(x.getCreditManagerHolderAsString()).as("credit manager holder as string").isNull();
			assertThat(x.getCreditManagerHolder()).as("credit manager holder").isNotNull();
			assertThat(x.getCreditManagerAssistant()).as("credit manager assistant").isNull();
			assertThat(x.getAuthorizingOfficerHolderAsString()).as("authorizing officer holder as string").isNull();
			assertThat(x.getAuthorizingOfficerHolder()).as("authorizing officer holder").isNotNull();
			assertThat(x.getAuthorizingOfficerAssistant()).as("authorizing officer assistant").isNull();
			assertThat(x.getFinancialControllerHolderAsString()).as("financial controller holder as string").isNull();
			assertThat(x.getFinancialControllerHolder()).as("financial controller holder").isNotNull();
			assertThat(x.getFinancialControllerAssistant()).as("financial controller assistant").isNull();
			assertThat(x.getAccountingHolderAsString()).as("accounting holder as string").isNull();
			//assertThat(x.getAccountingHolder()).as("accounting holder").isNotNull();
			assertThat(x.getAccountingAssistant()).as("accounting assistant").isNull();
		});
	}
	
	@Test
	public void readDynamic_filterByActivitiesIdentifiers(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITIES_IDENTIFIERS,List.of("ACTIVITE13001010003"));
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64320000","64323000");
	}
	
	@Test
	public void readDynamic_filterByActivityIdentifierAndEconomicNatureCode(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE,"64323000");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
	}
	
	@Test
	public void readDynamic_filterByActivityIdentifierAndEconomicNature(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE,"64323000");
		Collection<Assignments> collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
		
		queryExecutorArguments = new QueryExecutorArguments().setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addProcessableTransientFieldsNames(Assignments.FIELDS_ALL_STRINGS_CODES_ONLY);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE13001010003"
				,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE,"64323");
		collection = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		assertThat(collection).isNotEmpty();
		assertThat(collection.stream().map(x -> x.getEconomicNatureAsString()).collect(Collectors.toList())).containsExactlyInAnyOrder("64323000");
	}
	
	@Test
	public void create(){
		String identifier = "AFF01";
		Collection<Assignments> histories = null;
		histories = AuditReader.getInstance().readByIdentifier(Assignments.class, identifier);
		assertThat(histories).isNull();		
		
		Assignments assignments = new Assignments().setIdentifier(identifier);
		assignments.setExecutionImputation(EntityFinder.getInstance().find(ExecutionImputation.class, "2021130010100034266B70FF1A84632B0981B585E11CE7F"));
		EntityCreator.getInstance().createOneInTransaction(assignments);
		assignments = EntityReader.getInstance().readOneDynamically(Assignments.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		//EntityFinder.getInstance().find(Assignments.class, identifier);
		assertThat(assignments).isNotNull();
		histories = assignments.get__auditRecords__();
		assertThat(histories).hasSize(1);
		for(Assignments history : histories) {
			assertThat(history.getCreditManagerHolder()).isNull();
		}
		
		assignments.setCreditManagerHolder(EntityFinder.getInstance().find(ScopeFunction.class, "x001"));
		EntityUpdater.getInstance().updateOneInTransaction(assignments);
		assignments = EntityReader.getInstance().readOneDynamically(Assignments.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		histories = assignments.get__auditRecords__();
		assertThat(histories).hasSize(2);
		
		assertThat(CollectionHelper.getElementAt(histories, 1).getCreditManagerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(histories, 0).getCreditManagerHolder()).isNull();
		assertThat(CollectionHelper.getElementAt(histories, 0).getCreditManagerHolderAsString()).isEqualTo("O3000");
	}
	
}