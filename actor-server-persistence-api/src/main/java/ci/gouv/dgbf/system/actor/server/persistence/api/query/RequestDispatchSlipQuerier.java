package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.Order.desc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Select.concatCodeName;
import static org.cyk.utility.persistence.query.Language.Select.fields;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetCategory;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public interface RequestDispatchSlipQuerier extends Querier {

	String PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER = "budgetCategoryIdentifier";
	String PARAMETER_NAME_BUDGETS_CATEGORIES_IDENTIFIERS = "budgetCategoryIdentifiers";
	String PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER_NULLABLE = PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_SECTIONS_IDENTIFIERS = "sectionsIdentifiers";
	String PARAMETER_NAME_SECTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_SECTION_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_FUNCTION_IDENTIFIER = "functionIdentifier";
	String PARAMETER_NAME_FUNCTIONS_IDENTIFIERS = "functionsIdentifiers";
	String PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE = PARAMETER_NAME_FUNCTION_IDENTIFIER+"Nullable";
	
	String PARAMETER_NAME_SENDING_DATE_IS_NULL = "sendingDateIsNull";
	String PARAMETER_NAME_SENDING_DATE_IS_NULL_NULLABLE = PARAMETER_NAME_SENDING_DATE_IS_NULL+"Nullable";
	String PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL = "sendingDateIsNotNull";
	String PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL+"Nullable";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL = "processingDateIsNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NULL+"Nullable";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL = "processingDateIsNotNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL+"Nullable";
	
	String PARAMETER_NAME_SENT = "sent";
	String PARAMETER_NAME_PROCESSED = "processed";
	String PARAMETER_NAME_SEARCH = "search";
	/**/
	
	RequestDispatchSlip readOne(QueryExecutorArguments arguments);
	Collection<RequestDispatchSlip> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<RequestDispatchSlip> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QUERY_IDENTIFIER_READ_WHERE_FILTER+"ForUI";
	Collection<RequestDispatchSlip> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, QueryName.COUNT_WHERE_FILTER.getValue());
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, "readByIdentifier");
	RequestDispatchSlip readByIdentifier(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, "readByIdentifierForUI");
	RequestDispatchSlip readByIdentifierForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, "readByIdentifierForEdit");
	RequestDispatchSlip readByIdentifierForEdit(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_PROCESS = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, "readByIdentifierForProcess");
	RequestDispatchSlip readByIdentifierForProcess(String identifier);
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(RequestDispatchSlip.class, QueryName.COUNT_DYNAMIC);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestDispatchSlipQuerier,Serializable {
		@Override
		public RequestDispatchSlip readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER))
				return readByIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI))
				return readByIdentifierForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_PROCESS))
				return readByIdentifierForProcess((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(RequestDispatchSlip.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestDispatchSlip> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER))
				return readWhereFilter(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI))
				return readWhereFilterForUI(arguments);
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(RequestDispatchSlip.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_WHERE_FILTER))
				return countWhereFilter(arguments);
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(RequestDispatchSlip.class,arguments.setQuery(null));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}

		@Override
		public RequestDispatchSlip readByIdentifier(String identifier) {
			return QueryExecutor.getInstance().executeReadOne(RequestDispatchSlip.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER)
					.addFilterField(PARAMETER_NAME_IDENTIFIER, identifier));
		}
		
		@Override
		public RequestDispatchSlip readByIdentifierForUI(String identifier) {
			RequestDispatchSlip requestDispatchSlip = readByIdentifier(identifier);
			if(requestDispatchSlip == null)
				return null;
			prepareForUI(requestDispatchSlip);
			return requestDispatchSlip;
		}
		
		private void prepareForUI(RequestDispatchSlip requestDispatchSlip) {
			if(requestDispatchSlip == null)
				return;
			
			//if(StringHelper.isBlank(requestDispatchSlip.getBudgetCategoryAsString()) && requestDispatchSlip.getSection() != null)
			//	requestDispatchSlip.setSectionAsString(requestDispatchSlip.getSection().toString());
			
			if(StringHelper.isBlank(requestDispatchSlip.getSectionAsString()) && requestDispatchSlip.getSection() != null)
				requestDispatchSlip.setSectionAsString(requestDispatchSlip.getSection().toString());
			
			if(StringHelper.isBlank(requestDispatchSlip.getFunctionAsString()) && requestDispatchSlip.getFunction() != null)
				requestDispatchSlip.setFunctionAsString(requestDispatchSlip.getFunction().getName());
			
			if(requestDispatchSlip.getCreationDate() != null) {
				requestDispatchSlip.setCreationDateAsString(TimeHelper.formatLocalDateTime(requestDispatchSlip.getCreationDate(),"dd/MM/yyyy à HH:mm"));
				requestDispatchSlip.setCreationDate(null);
			}
			
			if(requestDispatchSlip.getProcessingDate() != null) {
				requestDispatchSlip.setProcessingDateAsString(TimeHelper.formatLocalDateTime(requestDispatchSlip.getProcessingDate(),"dd/MM/yyyy à HH:mm"));
				requestDispatchSlip.setProcessingDate(null);
			}
			
			if(requestDispatchSlip.getSendingDate() != null) {
				requestDispatchSlip.setSendingDateAsString(TimeHelper.formatLocalDateTime(requestDispatchSlip.getSendingDate(),"dd/MM/yyyy à HH:mm"));
				requestDispatchSlip.setSendingDate(null);
			}
		}
		
		@Override
		public RequestDispatchSlip readByIdentifierForEdit(String identifier) {
			RequestDispatchSlip requestDispatchSlip = readByIdentifier(identifier);
			if(requestDispatchSlip == null)
				return null;
			requestDispatchSlip.setRequests(RequestQuerier.getInstance().readByDispatchSlipIdentifierForUI(identifier));
			return requestDispatchSlip;
		}
		
		@Override
		public RequestDispatchSlip readByIdentifierForProcess(String identifier) {
			RequestDispatchSlip requestDispatchSlip = readByIdentifier(identifier);
			if(requestDispatchSlip == null)
				return null;
			requestDispatchSlip.setRequests(RequestQuerier.getInstance().readByDispatchSlipIdentifierForUI(identifier));
			return requestDispatchSlip;
		}
		
		@Override
		public Collection<RequestDispatchSlip> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(RequestDispatchSlip.class, arguments);
		}
		
		@Override
		public Collection<RequestDispatchSlip> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI);
			Collection<RequestDispatchSlip> requestDispatchSlips = readWhereFilter(arguments);
			if(CollectionHelper.isEmpty(requestDispatchSlips))
				return null;
			requestDispatchSlips.forEach(requestDispatchSlip -> {
				prepareForUI(requestDispatchSlip);
			});			
			return requestDispatchSlips;
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsNullable(arguments,PARAMETER_NAME_PROCESSING_DATE_IS_NULL,PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL
					,PARAMETER_NAME_SENDING_DATE_IS_NULL,PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL
					,PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER,PARAMETER_NAME_SECTION_IDENTIFIER,PARAMETER_NAME_FUNCTION_IDENTIFIER);
			filter.addFieldEquals(PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_SECTION_IDENTIFIER, arguments);
			filter.addFieldEquals(PARAMETER_NAME_FUNCTION_IDENTIFIER, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	static RequestDispatchSlipQuerier getInstance() {
		return Helper.getInstance(RequestDispatchSlipQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
				Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,RequestDispatchSlip.class,Query.FIELD_RESULT_CLASS,RequestDispatchSlip.class
				,Query.FIELD_VALUE,jpql(select(
						fields("t",RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME)
						,fields("f",Function.FIELD_NAME)
						)
						,getReadWhereFilterFromWhere(),getOrderBy())
				).setTupleFieldsNamesIndexesFromFieldsNames(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
						,RequestDispatchSlip.FIELD_FUNCTION_AS_STRING)

				,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,RequestDispatchSlip.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,jpql("SELECT COUNT(t.identifier)",getReadWhereFilterFromWhere()))
				
				,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI
						,Query.FIELD_TUPLE_CLASS,RequestDispatchSlip.class,Query.FIELD_RESULT_CLASS,RequestDispatchSlip.class
						,Query.FIELD_VALUE,jpql(select(
								fields("t",RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
										,RequestDispatchSlip.FIELD_CREATION_DATE,RequestDispatchSlip.FIELD_SENDING_DATE,RequestDispatchSlip.FIELD_PROCESSING_DATE)
								,concatCodeName("s"),fields("f",Function.FIELD_NAME),fields("bc",BudgetCategory.FIELD_NAME)
								)
								,getReadWhereFilterFromWhere(),getOrderBy())
						).setTupleFieldsNamesIndexesFromFieldsNames(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
								,RequestDispatchSlip.FIELD_CREATION_DATE_AS_STRING,RequestDispatchSlip.FIELD_SENDING_DATE_AS_STRING
								,RequestDispatchSlip.FIELD_PROCESSING_DATE_AS_STRING
								,RequestDispatchSlip.FIELD_SECTION_AS_STRING,RequestDispatchSlip.FIELD_FUNCTION_AS_STRING)
				
				,Query.buildSelect(RequestDispatchSlip.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER, "SELECT t FROM RequestDispatchSlip t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				"FROM RequestDispatchSlip t"
				,"LEFT JOIN BudgetCategory bc ON bc.identifier = t.budgetCategoryIdentifier"
				,"LEFT JOIN Section s ON s = t.section"
				,"LEFT JOIN Function f ON f = t.function"
				,getReadWhereFilterWhere()
			);
	}
	
	static String getReadWhereFilterWhere() {
		return where(and(
				/*
				isNullableOrIsNull("t", RequestDispatchSlip.FIELD_SENDING_DATE,PARAMETER_NAME_SENDING_DATE_IS_NULL_NULLABLE)
				,isNullableOrIsNull("t", RequestDispatchSlip.FIELD_SENDING_DATE,PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL_NULLABLE)
				,isNullableOrIsNull("t", RequestDispatchSlip.FIELD_PROCESSING_DATE,PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE)
				,isNullableOrIsNull("t", RequestDispatchSlip.FIELD_PROCESSING_DATE,PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE)
				*/
				parenthesis(or(String.format(":%s = true", PARAMETER_NAME_SENDING_DATE_IS_NULL_NULLABLE),"t.sendingDate IS NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL_NULLABLE),"t.sendingDate IS NOT NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE),"t.processingDate IS NULL"))
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE),"t.processingDate IS NOT NULL"))
				
				/* Budget category */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER_NULLABLE),"bc.identifier = :"+PARAMETER_NAME_BUDGET_CATEGORY_IDENTIFIER))
				/* Section */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_SECTION_IDENTIFIER_NULLABLE),"s.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER))
				/* Function */
				,parenthesis(or(String.format(":%s = true", PARAMETER_NAME_FUNCTION_IDENTIFIER_NULLABLE),"f.identifier = :"+PARAMETER_NAME_FUNCTION_IDENTIFIER))
		));
	}
	
	static String getOrderBy() {
		return order(desc("t",RequestDispatchSlip.FIELD_CREATION_DATE));
	}
}