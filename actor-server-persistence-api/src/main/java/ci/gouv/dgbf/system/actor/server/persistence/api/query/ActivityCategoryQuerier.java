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
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = ActivityCategory.class,name = ActivityCategoryQuerier.QUERY_NAME_READ,value = "SELECT t FROM ActivityCategory t ORDER BY t.code ASC")
	//,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_COUNT,value = "SELECT COUNT(t.identifier) FROM Function t")
})
public interface ActivityCategoryQuerier extends Querier.CodableAndNamable<ActivityCategory> {

	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeUnitIdentifier";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "read";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, QUERY_NAME_READ);
	Collection<ActivityCategory> read();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, "readAllForUI");
	Collection<ActivityCategory> readAllForUI();
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, "readBySectionIdentifierForUI");
	Collection<ActivityCategory> readBySectionIdentifierForUI(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, "readByAdministrativeUnitIdentifierForUI");
	Collection<ActivityCategory> readByAdministrativeUnitIdentifierForUI(String administrativeUnitIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, "readByBudgetSpecializationUnitIdentifierForUI");
	Collection<ActivityCategory> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ActivityCategory> implements ActivityCategoryQuerier,Serializable {
		@Override
		public Collection<ActivityCategory> read() {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<ActivityCategory> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			if(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readBySectionIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByAdministrativeUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByBudgetSpecializationUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		public Collection<ActivityCategory> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		public Collection<ActivityCategory> readBySectionIdentifierForUI(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<ActivityCategory> readByAdministrativeUnitIdentifierForUI(String administrativeUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER,administrativeUnitIdentifier);
		}
		
		@Override
		public Collection<ActivityCategory> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
					,PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier);
		}
		
		@Override
		protected Class<ActivityCategory> getKlass() {
			return ActivityCategory.class;
		}
	}
	
	/**/
	
	static ActivityCategoryQuerier getInstance() {
		return Helper.getInstance(ActivityCategoryQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ActivityCategory.class);
		QueryHelper.addQueries(Query.buildSelect(ActivityCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM ActivityCategory t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ActivityCategory.FIELD_IDENTIFIER,ActivityCategory.FIELD_CODE,ActivityCategory.FIELD_NAME)
				
				,Query.buildSelect(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ActivityCategory t"
					+" LEFT JOIN Activity a ON a.category = t"
					+" LEFT JOIN Section s ON s = a.section"
					+" WHERE s.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ActivityCategory.FIELD_IDENTIFIER,ActivityCategory.FIELD_CODE,ActivityCategory.FIELD_NAME)
				
				,Query.buildSelect(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ActivityCategory t"
					+" LEFT JOIN Activity a ON a.category = t"
					+" LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit"
					+" WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ActivityCategory.FIELD_IDENTIFIER,ActivityCategory.FIELD_CODE,ActivityCategory.FIELD_NAME)
					
				,Query.buildSelect(ActivityCategory.class, QUERY_IDENTIFIER_READ_BY_ADMINISTRATIVE_UNIT_IDENTIFIER_FOR_UI
					, "SELECT DISTINCT(t.identifier),t.code,t.name"
					+" FROM ActivityCategory t"
					+" LEFT JOIN Activity a ON a.category = t"
					+" LEFT JOIN AdministrativeUnit au ON au = a.administrativeUnit"
					+" WHERE au.identifier = :"+PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
					+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(ActivityCategory.FIELD_IDENTIFIER,ActivityCategory.FIELD_CODE,ActivityCategory.FIELD_NAME)		
				
			);
		
	}
}