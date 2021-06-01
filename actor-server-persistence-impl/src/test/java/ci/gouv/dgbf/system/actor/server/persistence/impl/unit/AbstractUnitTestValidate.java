package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.RequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public abstract class AbstractUnitTestValidate extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	//@org.junit.jupiter.api.Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	//@org.junit.jupiter.api.Test
	public void requestQuerier_section101(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(RequestQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_SECTION_IDENTIFIER
				,"SECTION01172e2c-7eb0-41a3-8d18-4c786e933ff7");
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getSectionAsString());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void request_readTransients_accountCreationDate_accountCreationMessage(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(Request.FIELD_ACCOUNT_CREATION_DATE_AS_STRING));
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getAccountCreationDateAsString()+" | "+x.getAccountCreationMessage());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void scopeFunction_readTransients_actorsNames(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setProcessableTransientFieldsNames(List.of(ScopeFunction.FIELD_ACTORS_AS_STRINGS));
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getActorsCodes()+" | "+x.getActorsAsStrings());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void requestQuerier_readWhereFilter_sortByFirstName_asc(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setSortOrders(Map.of(RequestQuerier.PARAMETER_NAME_FIRST_NAME,SortOrder.DESCENDING));
		queryExecutorArguments.setNumberOfTuples(5);
		Collection<Request> requests = RequestQuerier.getInstance().readWhereFilter(queryExecutorArguments);
		requests.stream().forEach(x -> {
			System.out.println(x.getCode()+" | "+x.getFirstName()+" | "+x.getLastNames());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void scopeFunctionQuerier_readWhereCodeOrNameLikeByFunctionCode(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterField(ScopeFunctionQuerier.PARAMETER_NAME_FUNCTION_CODE, "GC");
		queryExecutorArguments.setNumberOfTuples(5);
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereCodeOrNameLikeByFunctionCode(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCode());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void scopeFunctionQuerier_readWhereCodeOrNameLikeByFunctionsCodes(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterField(ScopeFunctionQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		queryExecutorArguments.setNumberOfTuples(5);
		Collection<ScopeFunction> scopeFunctions = ScopeFunctionQuerier.getInstance().readWhereCodeOrNameLikeByFunctionsCodes(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCode());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterForUI(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
		//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		queryExecutorArguments.setNumberOfTuples(100);
		Collection<Assignments> scopeFunctions = AssignmentsQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCreditManagerHolderAsString());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterForUI_activity(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE22086050001");
		//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		queryExecutorArguments.setNumberOfTuples(5);
		Collection<Assignments> scopeFunctions = AssignmentsQuerier.getInstance().readWhereFilterForUI(queryExecutorArguments);
		scopeFunctions.stream().forEach(x -> {
			System.out.println(x.getCreditManagerHolderAsString());
		});
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterUsingIdentifiersOnly(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,Boolean.TRUE);
		//queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,"G100761");
		//queryExecutorArguments.addFilterField(AssignmentsQuerier.PARAMETER_NAME_FUNCTIONS_CODES, List.of("GC"));
		queryExecutorArguments.setNumberOfTuples(1);
		Collection<Assignments> result = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		result.stream().forEach(x -> {
			print(x);
		});
	}
	
	@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readWhereFilterUsingIdentifiersOnly_activityIdentifier(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments().setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC));
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE22086050001");
		queryExecutorArguments.setNumberOfTuples(1);
		Collection<Assignments> result = AssignmentsQuerier.getInstance().readMany(queryExecutorArguments);
		result.stream().forEach(x -> {
			print(x);
		});
	}
	
	@org.junit.jupiter.api.Test
	public void assignmentsQuerier_countWhereFilterUsingIdentifiersOnly(){
		System.out.println(AssignmentsQuerier.getInstance().count(new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC))));
		System.out.println(AssignmentsQuerier.getInstance().count(new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC))
				.addFilterField(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE, Boolean.FALSE)));
		System.out.println(AssignmentsQuerier.getInstance().count(new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC))
				.addFilterField(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE, Boolean.FALSE)));
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_countWhereFilterUsingIdentifiersOnly_activityIdentifier(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_DYNAMIC));
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,"ACTIVITE22086050001");
		System.out.println(AssignmentsQuerier.getInstance().count(queryExecutorArguments));
		System.out.println(AssignmentsQuerier.getInstance().count(queryExecutorArguments));
	}
	
	//@org.junit.jupiter.api.Test
	public void assignmentsQuerier_readByIdentifierForUI_allProperties(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.addFilterFieldsValues(AssignmentsQuerier.PARAMETER_NAME_IDENTIFIER,"57ef78b9-629b-40c2-9fb5-ee433dc69537");
		//queryExecutorArguments.setProcessableTransientFieldsNames(List.of(Assignments.FIELD___ALL__));
		Assignments assignments = AssignmentsQuerier.getInstance().readByIdentifierForUI(queryExecutorArguments);
		print(assignments);
	}
	
	private static void print(Assignments assignments) {
		System.out.println(assignments.getSectionAsString());
		System.out.println(assignments.getAdministrativeUnitAsString());
		System.out.println(assignments.getBudgetSpecializationUnitAsString());
		System.out.println(assignments.getActionAsString());
		System.out.println(assignments.getActivityAsString());
		System.out.println(assignments.getActivityCategoryAsString());
		System.out.println(assignments.getExpenditureNatureAsString());
		System.out.println(assignments.getEconomicNatureAsString());
		System.out.println(assignments.getCreditManagerHolderAsString());
		System.out.println(assignments.getCreditManagerAssistantAsString());
		System.out.println(assignments.getAuthorizingOfficerHolderAsString());
		System.out.println(assignments.getAuthorizingOfficerAssistantAsString());
		System.out.println(assignments.getFinancialControllerHolderAsString());
		System.out.println(assignments.getFinancialControllerAssistantAsString());
		System.out.println(assignments.getAccountingHolderAsString());
		System.out.println(assignments.getAccountingAssistantAsString());
	}
}