package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.fields;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public interface RequestQuerier extends Querier {

	Request readOne(QueryExecutorArguments arguments);
	Collection<Request> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.READ_WHERE_FILTER.getValue());
	Collection<Request> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_FOR_UI = QUERY_IDENTIFIER_READ_WHERE_FILTER+"ForUI";
	Collection<Request> readWhereFilterForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Request.class, QueryName.COUNT_WHERE_FILTER.getValue());
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestQuerier,Serializable {
		
		@Override
		public Request readOne(QueryExecutorArguments arguments) {
			
			return null;
		}
		
		@Override
		public Collection<Request> readMany(QueryExecutorArguments arguments) {
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
		public Collection<Request> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Request.class, arguments);
		}
		
		@Override
		public Collection<Request> readWhereFilterForUI(QueryExecutorArguments arguments) {
			if(arguments == null)
				arguments = new QueryExecutorArguments();
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<Request> requests = readWhereFilter(arguments);
			if(CollectionHelper.isEmpty(requests))
				return null;
			setFunctions(requests,Boolean.TRUE);
			return requests;
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
			
			arguments.setFilter(filter);
		}
		
		protected static void setFunctions(Collection<Request> requests,Boolean asString) {
			Collection<RequestFunction> requestFunctions = RequestFunctionQuerier.getInstance().readByRequestsIdentifiers(FieldHelper.readSystemIdentifiersAsStrings(requests));
			if(CollectionHelper.isEmpty(requestFunctions))
				return;
			requests.forEach(request -> {
				Collection<Function> functions = requestFunctions.stream().filter(x -> x.getRequest().equals(request)).map(x -> x.getFunction()).collect(Collectors.toList());
				if(CollectionHelper.isNotEmpty(functions)) {
					if(Boolean.TRUE.equals(asString))
						request.setFunctionsAsStrings(functions.stream().map(function -> function.getName()).collect(Collectors.toList()));
					else
						request.setFunctions(requestFunctions.stream().filter(x -> x.getRequest().equals(request)).map(x -> x.getFunction()).collect(Collectors.toList()));
				}
				
			});
		}
	}
	
	/**/
	
	/**/
	
	static RequestQuerier getInstance() {
		return Helper.getInstance(RequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
			,Query.FIELD_TUPLE_CLASS,Request.class,Query.FIELD_RESULT_CLASS,Request.class
			,Query.FIELD_VALUE,jpql(select(
					fields("t",Request.FIELD_IDENTIFIER,Request.FIELD_COMMENT,Request.FIELD_CREATION_DATE)
					,fields("a",Actor.FIELD_CODE),Language.Select.concat("a.identity", Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES) 
					,fields("rt",RequestType.FIELD_NAME)
					)
					,getReadWhereFilterFromWhere())
			).setTupleFieldsNamesIndexesFromFieldsNames(Request.FIELD_IDENTIFIER,Request.FIELD_COMMENT,Request.FIELD_CREATION_DATE_AS_STRING
					,Request.FIELD_ACTOR_CODE,Request.FIELD_ACTOR_NAMES,Request.FIELD_TYPE_AS_STRING)
				
			,Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
			,Query.FIELD_TUPLE_CLASS,Request.class,Query.FIELD_RESULT_CLASS,Long.class
			,Query.FIELD_VALUE,jpql("SELECT COUNT(t.identifier)",getReadWhereFilterFromWhere())
			)
		);
	}
	
	static String getReadWhereFilterFromWhere() {
		return jpql(
				"FROM Request t"
				,"LEFT JOIN Actor a ON a = t.actor"
				,"LEFT JOIN RequestType rt ON rt = t.type"
			);
	}
}