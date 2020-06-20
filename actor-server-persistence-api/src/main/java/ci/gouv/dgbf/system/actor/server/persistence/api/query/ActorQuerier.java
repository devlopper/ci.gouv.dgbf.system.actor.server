package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public interface ActorQuerier extends Querier {

	//Collection<Actor> readMany(QueryExecutorArguments arguments);
	//Long count(QueryExecutorArguments arguments);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ActorQuerier,Serializable {
		/*
		@Override
		public Collection<Actor> readMany(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_VIEW_01.equals(arguments.getQuery().getIdentifier()))
				prepare(arguments);	
			return QueryExecutor.getInstance().executeReadMany(Actor.class, arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_COUNT_VIEW_01.equals(arguments.getQuery().getIdentifier()))
				prepare(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepare(QueryExecutorArguments arguments) {
			
		}
		*/
	}
	
	/**/
	
	/* All 01 */
	String QUERY_NAME_READ_ALL_01 = "read.all.01";
	String QUERY_IDENTIFIER_READ_ALL_01 = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_ALL_01);
	Map<String,Integer> QUERY_VALUE_READ_ALL_01_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Actor.FIELD_IDENTIFIER
			,Actor.FIELD_NAMES,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_ALL_01 = Language.of(Language.Select.of("t.identifier,"+Language.Select.concat("t", Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES)+",t."+Actor.FIELD_ELECTRONIC_MAIL_ADDRESS)
			,Language.From.of("Actor t")
			,Language.Order.of(Language.Order.join(Language.Order.asc("t", Actor.FIELD_FIRST_NAME),Language.Order.asc("t", Actor.FIELD_LAST_NAMES)))
			);
	
	String QUERY_NAME_COUNT_ALL_01 = "count.all.01";
	String QUERY_IDENTIFIER_COUNT_ALL_01 = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_COUNT_ALL_01);
	String QUERY_VALUE_COUNT_ALL_01 = "SELECT COUNT(t.identifier) FROM Actor t";
	
	/**/
	
	static ActorQuerier getInstance() {
		return Helper.getInstance(ActorQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ALL_01
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ALL_01
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_ALL_01_TUPLE_FIELDS_NAMES_INDEXES)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_ALL_01
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_ALL_01
				)
			);
	}
}