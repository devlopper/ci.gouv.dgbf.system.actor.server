package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.asc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.isNullableFromFieldName;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.ParameterNameBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Action;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Activity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface ActivityQuerier extends Querier.CodableAndNamable<Activity> {

	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER = "administrativeUnitIdentifier";
	String PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER, null, Boolean.TRUE);
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = "budgetSpecializationUnitIdentifier";
	String PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER, null, Boolean.TRUE);
	String PARAMETER_NAME_ACTION_IDENTIFIER = "actionIdentifier";
	String PARAMETER_NAME_ACTION_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_ACTION_IDENTIFIER, null, Boolean.TRUE);
	String PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER = "expenditureNatureIdentifier";
	String PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER, null, Boolean.TRUE);
	String PARAMETER_NAME_CATEGORY_IDENTIFIER = "categoryIdentifier";
	String PARAMETER_NAME_CATEGORY_IDENTIFIER_NULLABLE = ParameterNameBuilder.getInstance().build(PARAMETER_NAME_CATEGORY_IDENTIFIER, null, Boolean.TRUE);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByIdentifier");
	Activity readByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByIdentifierForUI");
	Activity readByIdentifierForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByIdentifierWithCodesNamesForUI");
	Activity readByIdentifierWithCodesNamesForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByBudgetSpecializationUnitIdentifier");
	Collection<Activity> readByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readByBudgetSpecializationUnitIdentifierForUI");
	Collection<Activity> readByBudgetSpecializationUnitIdentifierForUI(String budgetSpecializationUnitIdentifier);
	
	String QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER);
	Long countByBudgetSpecializationUnitIdentifier(String budgetSpecializationUnitIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class
			, "readByBudgetSpecializationUnitIdentifierByCategoryIdentifierForUI");
	Collection<Activity> readByBudgetSpecializationUnitIdentifierByCategoryIdentifierForUI(String budgetSpecializationUnitIdentifier,String categoryIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class
			, "readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierForUI");
	Collection<Activity> readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierForUI(String budgetSpecializationUnitIdentifier
			,String expenditureIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class
			, "readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierByCategoryIdentifierForUI");
	Collection<Activity> readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierByCategoryIdentifierForUI(String budgetSpecializationUnitIdentifier
			,String expenditureIdentifier,String categoryIdentifier);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Activity.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<Activity> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Activity.class, "readWhereFilterForUI");
	Collection<Activity> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Activity> implements ActivityQuerier,Serializable {
		
		@Override
		public Activity readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI))
				return readByIdentifierForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER))
				return readByIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI))
				return readByIdentifierWithCodesNamesForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Activity> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			if(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readWhereFilterForUI(arguments);
			
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER))
				return readByBudgetSpecializationUnitIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI))
				return readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierByCategoryIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(PARAMETER_NAME_CATEGORY_IDENTIFIER));
			
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_FOR_UI))
				return readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER));
			
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI))
				return readByBudgetSpecializationUnitIdentifierByCategoryIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(PARAMETER_NAME_CATEGORY_IDENTIFIER));
			
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_FOR_UI))
				return readByBudgetSpecializationUnitIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_WHERE_FILTER.equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER))
				return countByBudgetSpecializationUnitIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER));
			return super.count(arguments);
		}
		
		@Override
		public Activity readByIdentifier(String identifier) {
			return QueryExecutor.getInstance().executeReadOne(Activity.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER)
					.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
		}
		
		@Override
		public Activity readByIdentifierForUI(String identifier) {
			return QueryExecutor.getInstance().executeReadOne(Activity.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI)
					.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
		}
		
		@Override
		public Activity readByIdentifierWithCodesNamesForUI(String identifier) {
			Activity activity = QueryExecutor.getInstance().executeReadOne(Activity.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI)
					.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			activity.setSection(new Section().setIdentifier(activity.getSectionIdentifier()).setCode(StringUtils.substringBefore(activity.getSectionCodeName(), " ")).setName(StringUtils.substringAfter(activity.getSectionCodeName(), " ")));
			activity.setAdministrativeUnit(new AdministrativeUnit().setIdentifier(activity.getAdministrativeUnitIdentifier()).setCode(StringUtils.substringBefore(activity.getAdministrativeUnitCodeName(), " ")).setName(StringUtils.substringAfter(activity.getAdministrativeUnitCodeName(), " ")));
			activity.setBudgetSpecializationUnit(new BudgetSpecializationUnit().setIdentifier(activity.getBudgetSpecializationUnitIdentifier()).setCode(StringUtils.substringBefore(activity.getBudgetSpecializationUnitCodeName(), " ")).setName(StringUtils.substringAfter(activity.getBudgetSpecializationUnitCodeName(), " ")));
			activity.setAction(new Action().setIdentifier(activity.getActionIdentifier()).setCode(StringUtils.substringBefore(activity.getActionCodeName(), " ")).setName(StringUtils.substringAfter(activity.getActionCodeName(), " ")));
			activity.setExpenditureNature(new ExpenditureNature().setIdentifier(activity.getExpenditureNatureIdentifier()).setCode(StringUtils.substringBefore(activity.getExpenditureNatureCodeName(), " ")).setName(StringUtils.substringAfter(activity.getExpenditureNatureCodeName(), " ")));
			activity.setCategory(new ActivityCategory().setIdentifier(activity.getCategoryIdentifier()).setCode(StringUtils.substringBefore(activity.getCategoryCodeName(), " ")).setName(StringUtils.substringAfter(activity.getCategoryCodeName(), " ")));
			return activity;
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
		public Collection<Activity> readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierByCategoryIdentifierForUI(String budgetSpecializationUnitIdentifier
				, String expenditureIdentifier, String categoryIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier
					, PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER,expenditureIdentifier
					, PARAMETER_NAME_CATEGORY_IDENTIFIER,categoryIdentifier);
		}
		
		@Override
		public Collection<Activity> readByBudgetSpecializationUnitIdentifierByCategoryIdentifierForUI(String budgetSpecializationUnitIdentifier, String categoryIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier
					, PARAMETER_NAME_CATEGORY_IDENTIFIER,categoryIdentifier);
		}
		
		@Override
		public Collection<Activity> readByBudgetSpecializationUnitIdentifierByExpenditureNatureIdentifierForUI(String budgetSpecializationUnitIdentifier
				, String expenditureIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,budgetSpecializationUnitIdentifier
					, PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER,expenditureIdentifier);
		}
		
		@Override
		public Collection<Activity> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(Activity.class, QueryName.READ_WHERE_FILTER);
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Activity.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = QueryExecutorArguments.instantiate(Activity.class, QueryName.COUNT_WHERE_FILTER);
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();			
			filter.addFieldsNullable(arguments
					,PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
					,PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
					,PARAMETER_NAME_ACTION_IDENTIFIER
					,PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER
					,PARAMETER_NAME_CATEGORY_IDENTIFIER
					);			
			filter.addFieldEquals(PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_ACTION_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_CATEGORY_IDENTIFIER, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Activity> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
			return readWhereFilter(arguments);
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
				Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_WHERE_FILTER, jpql(select("t"),getReadWhereFilterFromWhere(),getOrderBy()))			
				,Query.buildCount(QUERY_IDENTIFIER_COUNT_WHERE_FILTER, jpql(select("COUNT(t.identifier)"),getReadWhereFilterFromWhere()))
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
					, jpql(
							select(fields("t",Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME))
							,getReadWhereFilterFromWhere()
							,getOrderBy()
							))
				.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME)
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
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
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "LEFT JOIN ExpenditureNature nd ON nd = t.expenditureNature "
								+ "LEFT JOIN ActivityCategory ac ON ac = t.category "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
								+" AND nd.identifier = :"+PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER
								+" AND ac.identifier = :"+PARAMETER_NAME_CATEGORY_IDENTIFIER
								+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME)	
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_EXPENDITURE_NATURE_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "LEFT JOIN ExpenditureNature nd ON nd = t.expenditureNature "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
								+" AND nd.identifier = :"+PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER
								+" ORDER BY t.code ASC")
						.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME)		
					
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER_BY_CATEGORY_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = a.budgetSpecializationUnit "
								+ "LEFT JOIN ActivityCategory ac ON ac = t.category "
								+ "WHERE b.identifier = :"+PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER
								+" AND ac.identifier = :"+PARAMETER_NAME_CATEGORY_IDENTIFIER
								+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME)	
				
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER, "SELECT t FROM Activity t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)

				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name"
								+ ",s.identifier"
								+ ",b.identifier"
								+ ",en.identifier"
								+ ",ac.identifier"
								+ " FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = t.budgetSpecializationUnit "
								+ "LEFT JOIN Section s ON s = t.section "
								+ "LEFT JOIN ExpenditureNature en ON en = t.expenditureNature "
								+ "LEFT JOIN ActivityCategory ac ON ac = t.category "
								+ "WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
					.setTupleFieldsNamesIndexesFromFieldsNames(
							Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME,Activity.FIELD_SECTION_IDENTIFIER
							,Activity.FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,Activity.FIELD_EXPENDITURE_NATURE_IDENTIFIER,Activity.FIELD_CATEGORY_IDENTIFIER
							)
					
				,Query.buildSelect(Activity.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI
						, "SELECT t.identifier,t.code,t.name"
								+ ",s.identifier,t.sectionCodeName"
								+ ",au.identifier,t.administrativeUnitCodeName"
								+ ",b.identifier,t.budgetSpecializationUnitCodeName"
								+ ",a.identifier,t.actionCodeName"
								+ ",en.identifier,t.expenditureNatureCodeName"
								+ ",ac.identifier,t.categoryCodeName"
								+ " FROM Activity t "
								+ "LEFT JOIN Action a ON a = t.action "
								+ "LEFT JOIN BudgetSpecializationUnit b ON b = t.budgetSpecializationUnit "
								+ "LEFT JOIN AdministrativeUnit au ON au = t.administrativeUnit "
								+ "LEFT JOIN Section s ON s = t.section "
								+ "LEFT JOIN ExpenditureNature en ON en = t.expenditureNature "
								+ "LEFT JOIN ActivityCategory ac ON ac = t.category "
								+ "WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
					.setTupleFieldsNamesIndexesFromFieldsNames(
							Activity.FIELD_IDENTIFIER,Activity.FIELD_CODE,Activity.FIELD_NAME
							,Activity.FIELD_SECTION_IDENTIFIER,Activity.FIELD_SECTION_CODE_NAME
							,Activity.FIELD_ADMINISTRATIVE_UNIT_IDENTIFIER,Activity.FIELD_ADMINISTRATIVE_UNIT_CODE_NAME
							,Activity.FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,Activity.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE_NAME
							,Activity.FIELD_ACTION_IDENTIFIER,Activity.FIELD_ACTION_CODE_NAME
							,Activity.FIELD_EXPENDITURE_NATURE_IDENTIFIER,Activity.FIELD_EXPENDITURE_NATURE_CODE_NAME
							,Activity.FIELD_CATEGORY_IDENTIFIER,Activity.FIELD_CATEGORY_CODE_NAME
							)	
			);
	}
	
	/* read where filter */
	
	static String getReadWhereFilterFrom() {
		return from(
				"Activity t"
				,"LEFT JOIN Action action ON action = t.action"
				,"LEFT JOIN BudgetSpecializationUnit budgetSpecializationUnit ON budgetSpecializationUnit = t.budgetSpecializationUnit"
				,"LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = t.administrativeUnit"
				,"LEFT JOIN ExpenditureNature expenditureNature ON expenditureNature = t.expenditureNature"
				,"LEFT JOIN ActivityCategory category ON category = t.category"
			);
	}
	
	static String getReadWhereFilterWherePredicate() {
		return and(
				parenthesis(or(isNullableFromFieldName(Activity.FIELD_ADMINISTRATIVE_UNIT_IDENTIFIER),Where.equals("administrativeUnit", AbstractIdentifiableSystemScalarStringImpl.FIELD_IDENTIFIER,PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER)))
				,parenthesis(or(isNullableFromFieldName(Activity.FIELD_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER),Where.equals("budgetSpecializationUnit",AbstractIdentifiableSystemScalarStringImpl.FIELD_IDENTIFIER, PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER)))
				,parenthesis(or(isNullableFromFieldName(Activity.FIELD_ACTION_IDENTIFIER),Where.equals("action", AbstractIdentifiableSystemScalarStringImpl.FIELD_IDENTIFIER,PARAMETER_NAME_ACTION_IDENTIFIER)))
				,parenthesis(or(isNullableFromFieldName(Activity.FIELD_EXPENDITURE_NATURE_IDENTIFIER),Where.equals("expenditureNature", AbstractIdentifiableSystemScalarStringImpl.FIELD_IDENTIFIER,PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER)))
				,parenthesis(or(isNullableFromFieldName(Activity.FIELD_CATEGORY_IDENTIFIER),Where.equals("category", AbstractIdentifiableSystemScalarStringImpl.FIELD_IDENTIFIER,PARAMETER_NAME_CATEGORY_IDENTIFIER)))
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(getReadWhereFilterFrom(),where(getReadWhereFilterWherePredicate()));
	}

	static String getOrderBy() {
		return order(asc("t",Activity.FIELD_CODE));
	}
}