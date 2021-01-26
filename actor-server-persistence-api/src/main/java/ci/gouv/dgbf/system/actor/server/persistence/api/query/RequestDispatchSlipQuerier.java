package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.parenthesis;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.desc;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.and;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.or;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public interface RequestDispatchSlipQuerier extends Querier {

	String PARAMETER_NAME_SENDING_DATE_IS_NULL = "sendingDateIsNull";
	String PARAMETER_NAME_SENDING_DATE_IS_NULL_NULLABLE = PARAMETER_NAME_SENDING_DATE_IS_NULL+"Nullable";
	String PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL = "sendingDateIsNotNull";
	String PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL+"Nullable";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL = "processingDateIsNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NULL+"Nullable";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL = "processingDateIsNotNull";
	String PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL_NULLABLE = PARAMETER_NAME_PROCESSING_DATE_IS_NOT_NULL+"Nullable";
	
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
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestDispatchSlip> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER))
				return readWhereFilter(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI))
				return readWhereFilterForUI(arguments);
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_WHERE_FILTER))
				return countWhereFilter(arguments);
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
					,PARAMETER_NAME_SENDING_DATE_IS_NULL,PARAMETER_NAME_SENDING_DATE_IS_NOT_NULL);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	static RequestDispatchSlipQuerier getInstance() {
		return Helper.getInstance(RequestDispatchSlipQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
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
								fields("t",RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME)
								,fields("f",Function.FIELD_NAME)
								)
								,getReadWhereFilterFromWhere(),getOrderBy())
						).setTupleFieldsNamesIndexesFromFieldsNames(RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CODE,RequestDispatchSlip.FIELD_NAME
								,RequestDispatchSlip.FIELD_FUNCTION_AS_STRING)
				
				,Query.buildSelect(RequestDispatchSlip.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER, "SELECT t FROM RequestDispatchSlip t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				"FROM RequestDispatchSlip t"
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
		));
	}
	
	static String getOrderBy() {
		return order(desc("t",RequestDispatchSlip.FIELD_CREATION_DATE));
	}
}