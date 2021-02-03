package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;

public interface ActivityQuerier extends Querier.CodableAndNamable<Activity> {

	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByIdentifierForUI");
	Activity readByIdentifierForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByBudgetSpecializationUnitIdentifier");
	Collection<Activity> readByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByBudgetSpecializationUnitIdentifierForUI");
	Collection<Activity> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier);
	
	String QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER);
	Long countByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Activity> implements ActivityQuerier,Serializable {
		
		@Override
		public Activity readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI))
				return readByIdentifierForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Activity> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER))
				return readByBudgetSpecializationUnitIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI))
				return readByBudgetSpecializationUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER))
				return countByBudgetSpecializationUnitIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.count(arguments);
		}
		
		@Override
		public Activity readByIdentifierForUI(String identifier) {
			return QueryExecutor.getInstance().executeReadOne(Activity.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI)
					.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
		}
		
		@Override
		public Collection<Activity> readByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
					,budgetSpecializationUnitIdentifier);
		}
		
		@Override
		public Long countByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier);
		}
		
		@Override
		public Collection<Activity> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier);
		}
		
		@Override
		protected Class<Activity> getKlass() {
			return Activity.class;
		}
		
	}
	
	/**/
	
	static ActivityQuerier getInstance() {
		return Helper.getInstance(ActivityQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Activity.class);
		QueryHelper.addQueries(
				Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
						, "SELECT t FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER+" ORDER BY t.code ASC")
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
						, "SELECT COUNT(t.identifier) FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME)
					
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name,b.identifier FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
					.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME,Activity.FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)
			);
	}
}