package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScope;

public interface ActorScopeQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_SCOPES_CODES = "scopesCodes";
	String PARAMETER_NAME_SCOPE_CODE = "scopeCode";
	String PARAMETER_NAME_SCOPE_TYPES_CODES = "scopeTypesCodes";
	
	/* read by actors codes order by scope code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES = "readByActorsCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_BY_ACTORS_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Where.of("t.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			,Language.Order.of("t.scope.code ASC"))
			;
	Collection<ActorScope> readByActorsCodes(Collection<String> actorsCodes);
	
	/* read by scopes codes order by actor code ascending */
	String QUERY_NAME_READ_BY_SCOPES_CODES = "readByScopesCodes";
	String QUERY_IDENTIFIER_READ_BY_SCOPES_CODES = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_BY_SCOPES_CODES);
	String QUERY_VALUE_READ_BY_SCOPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Where.of("t.scope.code IN :"+PARAMETER_NAME_SCOPES_CODES)			
			,Language.Order.of("t.actor.code ASC"))
			;
	Collection<ActorScope> readByScopesCodes(Collection<String> scopesCodes);
	
	/* read by actor code by scopes codes */
	String QUERY_NAME_READ_BY_ACTOR_CODE_BY_SCOPE_CODE = "readByActorCodeByScopeCode";
	String QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_BY_SCOPE_CODE = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_BY_ACTOR_CODE_BY_SCOPE_CODE);
	String QUERY_VALUE_READ_BY_ACTOR_CODE_BY_SCOPE_CODE = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Where.of(Language.Where.and("t.actor.code = :"+PARAMETER_NAME_ACTOR_CODE,"t.scope.code = :"+PARAMETER_NAME_SCOPE_CODE))			
			)
			;
	ActorScope readByActorCodeByScopeCode(String actorCode,String scopeCode);
	
	/* read by actors codes by scopes codes */
	String QUERY_NAME_READ_BY_ACTORS_CODES_BY_SCOPES_CODES = "readByActorsCodesByScopesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPES_CODES = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_BY_ACTORS_CODES_BY_SCOPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_SCOPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Where.of(Language.Where.and("t.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES,"t.scope.code IN :"+PARAMETER_NAME_SCOPES_CODES))			
			,Language.Order.of("t.actor.code ASC,t.scope.code ASC"))
			;
	Collection<ActorScope> readByActorsCodesByScopesCodes(Collection<String> actorsCodes,Collection<String> scopesCodes);
	
	/* read by actors codes by scope types codes order by scope code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = "readByActorsCodesByScopeTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Where.of(Language.Where.and("t.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES,"t.scope.type.code IN :"+PARAMETER_NAME_SCOPE_TYPES_CODES))			
			,Language.Order.of("t.actor.code ASC,t.scope.code ASC"))
			;
	Collection<ActorScope> readByActorsCodesByScopeTypesCodes(Collection<String> actorsCodes,Collection<String> scopeTypesCodes);
	
	/* count by actors codes by scope types codes */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = "countByActorsCodesByScopeTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES = Language.of(Language.Select.of("COUNT(t)")
			,Language.From.of("ActorScope t")			
			,Language.Where.of(Language.Where.and("t.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES,"t.scope.type.code IN :"+PARAMETER_NAME_SCOPE_TYPES_CODES))		
			);
	Long countByActorsCodesByScopeTypesCodes(Collection<String> actorsCodes,Collection<String> scopeTypesCodes);
	
	/* read order by actor code ascending by scope code ascending */
	String QUERY_NAME_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING = "readOrderByActorCodeAscendingByScopeCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(ActorScope.class, QUERY_NAME_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING);
	String QUERY_VALUE_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING = Language.of(Language.Select.of("t")
			,Language.From.of("ActorScope t")			
			,Language.Order.of(Language.Order.join(Language.Order.asc("t", "actor.code"),Language.Order.asc("t", "scope.code"))))
			;
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ActorScopeQuerier,Serializable {
		@Override
		public Collection<ActorScope> readByActorsCodes(Collection<String> actorsCodes) {
			return EntityReader.getInstance().readMany(ActorScope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<ActorScope> readByScopesCodes(Collection<String> scopesCodes) {
			return EntityReader.getInstance().readMany(ActorScope.class, QUERY_IDENTIFIER_READ_BY_SCOPES_CODES, PARAMETER_NAME_SCOPES_CODES,scopesCodes);
		}
		
		@Override
		public ActorScope readByActorCodeByScopeCode(String actorCode,String scopeCode) {
			return EntityReader.getInstance().readOne(ActorScope.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_BY_SCOPE_CODE)
					.addFilterFieldsValues(PARAMETER_NAME_ACTOR_CODE,actorCode, PARAMETER_NAME_SCOPE_CODE,scopeCode));
		}
		
		@Override
		public Collection<ActorScope> readByActorsCodesByScopesCodes(Collection<String> actorsCodes,Collection<String> scopesCodes) {
			return EntityReader.getInstance().readMany(ActorScope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPES_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes
					, PARAMETER_NAME_SCOPES_CODES,scopesCodes);
		}
		
		@Override
		public Collection<ActorScope> readByActorsCodesByScopeTypesCodes(Collection<String> actorsCodes,Collection<String> scopeTypesCodes) {
			return EntityReader.getInstance().readMany(ActorScope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes
					, PARAMETER_NAME_SCOPE_TYPES_CODES,scopeTypesCodes);
		}
		
		@Override
		public Long countByActorsCodesByScopeTypesCodes(Collection<String> actorsCodes,Collection<String> scopeTypesCodes) {
			return EntityCounter.getInstance().count(ActorScope.class,new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES).addFilterFieldsValues(PARAMETER_NAME_ACTORS_CODES,actorsCodes
					, PARAMETER_NAME_SCOPE_TYPES_CODES,scopeTypesCodes));
		}
	}
	
	/**/
	
	static ActorScopeQuerier getInstance() {
		return Helper.getInstance(ActorScopeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_SCOPES_CODES
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_SCOPES_CODES));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_BY_SCOPE_CODE
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTOR_CODE_BY_SCOPE_CODE));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPES_CODES
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_BY_SCOPES_CODES));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,Long.class,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_SCOPE_TYPES_CODES));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING
				,Query.FIELD_TUPLE_CLASS,ActorScope.class,Query.FIELD_RESULT_CLASS,ActorScope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ORDER_BY_ACTOR_CODE_ASCENDING_BY_SCOPE_CODE_ASCENDING
				)
			);
	}
}