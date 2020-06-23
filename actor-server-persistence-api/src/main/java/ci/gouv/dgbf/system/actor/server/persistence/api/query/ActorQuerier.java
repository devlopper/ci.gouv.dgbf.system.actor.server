package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;

public interface ActorQuerier extends Querier {

	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	
	//Collection<Actor> readMany(QueryExecutorArguments arguments);
	//Long count(QueryExecutorArguments arguments);
	Actor readOne(QueryExecutorArguments arguments);
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ActorQuerier,Serializable {
		
		@Override
		public Actor readOne(QueryExecutorArguments arguments) {
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readOneWithAllPrivilegesByIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));	
			return QueryExecutor.getInstance().executeReadOne(Actor.class, arguments);
		}
		
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
		
		@Override
		public Actor readOneWithAllPrivilegesByIdentifier(String identifier) {
			if(StringHelper.isBlank(identifier))
				return null;
			Actor actor = EntityFinder.getInstance().find(Actor.class, identifier);
			if(actor == null)
				return null;
			actor.setPrivileges(PrivilegeQuerier.getInstance().readByActorsCodes(List.of(actor.getCode())));
			return actor;
		}
	}
	
	/**/
	
	/* Read All 01 */
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
	
	/* Read by string */
	String QUERY_NAME_READ_BY_STRING = "readByString";
	String QUERY_IDENTIFIER_READ_BY_STRING = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_BY_STRING);
	Map<String,Integer> QUERY_VALUE_READ_BY_STRING_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Actor.FIELD_IDENTIFIER,Actor.FIELD_CODE
			,Actor.FIELD_NAMES,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_BY_STRING_WHERE = " WHERE LOWER(t.code) LIKE LOWER(:"+PARAMETER_NAME_STRING+") OR LOWER(t.firstName) LIKE LOWER(:"+PARAMETER_NAME_STRING+") OR LOWER(t.firstName) LIKE LOWER(:"+PARAMETER_NAME_STRING+")";
	String QUERY_VALUE_READ_BY_STRING = Language.of(Language.Select.of("t.identifier,t.code,"+Language.Select.concat("t", Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES)+",t."+Actor.FIELD_ELECTRONIC_MAIL_ADDRESS)
			,Language.From.of("Actor t")
			,QUERY_VALUE_READ_BY_STRING_WHERE
			,Language.Order.of(Language.Order.join(Language.Order.asc("t", Actor.FIELD_FIRST_NAME),Language.Order.asc("t", Actor.FIELD_LAST_NAMES)))
			);
	
	String QUERY_NAME_COUNT_BY_STRING = "countByString";
	String QUERY_IDENTIFIER_COUNT_BY_STRING = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_COUNT_BY_STRING);
	String QUERY_VALUE_COUNT_BY_STRING = "SELECT COUNT(t.identifier) FROM Actor t "+QUERY_VALUE_READ_BY_STRING_WHERE;
	
	/* Read One with all Privileges and all Scopes */
	String QUERY_NAME_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER = "readOneWithAllPrivilegesByIdentifier";
	String QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER);
	Actor readOneWithAllPrivilegesByIdentifier(String identifier);
	
	/* Read One with all Privileges and all Scopes */
	/*
	String QUERY_NAME_READ_ONE_WITH_ALL_PRIVILEGES_ALL_SCOPES_BY_IDENTIFIER = "readOneWithAllPrivilegesAllScopesByIdentifier";
	String QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_ALL_SCOPES_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_ONE_WITH_ALL_PRIVILEGES_ALL_SCOPES_BY_IDENTIFIER);
	Actor readOneWithAllPrivilegesAllScopesByIdentifier(String identifier);
	*/
	
	/* Read by string */
	String QUERY_NAME_READ_BY_ELECTRONIC_MAIL_ADDRESS = "readByElectronicMailAddress";
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_BY_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Language.Select.of("t")
			,Language.From.of("Actor t")
			,"WHERE t."+Actor.FIELD_ELECTRONIC_MAIL_ADDRESS+" = :"+PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS
			,Language.Order.of(Language.Order.join(Language.Order.asc("t", Actor.FIELD_FIRST_NAME),Language.Order.asc("t", Actor.FIELD_LAST_NAMES)))
			);
	
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_STRING
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_STRING
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_BY_STRING_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_STRING
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_STRING
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
	}
}