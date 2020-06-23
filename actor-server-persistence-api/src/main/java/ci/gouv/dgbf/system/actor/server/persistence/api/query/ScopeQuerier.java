package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ScopeQuerier extends Querier {

	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	
	/* read all order by type code asscending by code ascending */
	String QUERY_NAME_READ_ALL_01 = "read.all.01";
	String QUERY_IDENTIFIER_READ_ALL_01 = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_ALL_01);
	String QUERY_VALUE_READ_ALL_01 = Language.of(Language.Select.of("t"),"From Scope t"
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readAll01();
	
	/* count all */
	String QUERY_NAME_COUNT_ALL_01 = "count.all.01";
	String QUERY_IDENTIFIER_COUNT_ALL_01 = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_ALL_01);
	String QUERY_VALUE_COUNT_ALL_01 = Language.of(Language.Select.of("COUNT(t.identifier)"),"From Scope t");
	Long countAll01();
	
	/* read by actors codes by scopes types codes order by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES = "readByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t JOIN ActorScope actorScope ON actorScope.scope.identifier = t.identifier")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,"actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* count by actors codes by scopes types codes */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = "countByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t)")
			,Language.From.of("Scope t JOIN ActorScope actorScope ON actorScope.scope.identifier = t.identifier")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,"actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)))
			;
	Long countByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* read by actors codes not associated by types codes order by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = "readByActorsCodesNotAssociatedByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES+")"))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByActorsCodesNotAssociatedByTypesCodes(Collection<String> profilesCodes,Collection<String> typesCodes);
	
	/* count by actors codes not associated by types codes */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = "countByActorsCodesNotAssociatedByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"
							+PARAMETER_NAME_ACTORS_CODES+")"))
			);
	Long countByActorsCodesNotAssociatedByTypesCodes(Collection<String> profilesCodes,Collection<String> typesCodes);
	
	/* read by types codes order by code ascending */
	String QUERY_NAME_READ_BY_TYPES_CODES = "readByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))			
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_NAME_COUNT_BY_TYPES_CODES = "countByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
			);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeQuerier,Serializable {
		
		@Override
		public Collection<Scope> readAll01() {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_ALL_01);
		}
		
		@Override
		public Long countAll01() {
			return EntityCounter.getInstance().count(Scope.class,QUERY_IDENTIFIER_COUNT_ALL_01);
		}
		
		@Override
		public Collection<Scope> readByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES
					, PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES)
					.addFilterFieldsValues(PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
		
		@Override
		public Collection<Scope> readByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
					, PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES).addFilterFieldsValues(
					PARAMETER_NAME_ACTORS_CODES,actorsCodes,PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
		
		@Override
		public Collection<Scope> readByTypesCodes(Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Scope.class, new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES).addFilterFieldsValues(PARAMETER_NAME_TYPES_CODES,typesCodes));
		}
	}
	
	/**/
	
	static ScopeQuerier getInstance() {
		return Helper.getInstance(ScopeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ALL_01,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ALL_01));		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_ALL_01
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class,Query.FIELD_VALUE,QUERY_VALUE_COUNT_ALL_01));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES,Query.FIELD_TUPLE_CLASS,Scope.class
				,Query.FIELD_RESULT_CLASS,Scope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				)
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_TYPES_CODES
				)
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_TYPES_CODES
				)
			);
	}
}