package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
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
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeQuerier extends Querier {

	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	
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
	
	/* read by codes by types codes order by code ascending */
	String QUERY_NAME_READ_BY_CODES_BY_TYPES_CODES = "readByCodesByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.code IN :"+PARAMETER_NAME_CODES,"t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByCodesByTypesCodes(Collection<String> codes,Collection<String> typesCodes);
	
	/* read by actors codes by scopes types codes order by type code by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES = "readByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t JOIN ActorScope actorScope ON actorScope.scope.identifier = t.identifier")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,"actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES))			
			,Language.Order.of("t.type.code ASC,t.code ASC"))
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
	
	/* read visible sections by actor code order by code ascending */
	String QUERY_NAME_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE = "readVisibleSectionsByActorCode";
	String QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE);
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE_WHERE = 
			"WHERE scope.type.code = '"+ScopeType.CODE_SECTION+"' AND ("+
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier "+ 
			"    ) " + 
			"    OR " + 
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        JOIN Scope scopeUa ON actorScope.scope = scopeUa " + 
			"        JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeUa " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND administrativeUnit.section = scope" + 
			"    )" + 
			")";
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE = 
			"SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleSectionsByActorCode(String actorCode);
	
	/* count visible sections by actor code */
	String QUERY_NAME_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = "countVisibleSectionsByActorCode";
	String QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE_WHERE;
	Long countVisibleSectionsByActorCode(String actorCode);
	
	/* read visible administrative units by actor code order by code ascending */
	String QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "readVisibleAdministrativeUnitsByActorCode";
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE = 
			"WHERE scope.type.code = '"+ScopeType.CODE_UA+"' AND ("+
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier AND (actorScope.visible IS NULL OR actorScope.visible = true) "+ 
			"    ) " + 
			"    OR " + 
			"    EXISTS (" + 
			"        SELECT actorScope.identifier " + 
			"        FROM ActorScope actorScope " + 
			"        JOIN Scope scopeSection ON actorScope.scope = scopeSection " + 
			"        JOIN Section section ON section = scopeSection " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND EXISTS("
					+ "SELECT administrativeUnit "
					+ "FROM AdministrativeUnit administrativeUnit "
					+ "WHERE administrativeUnit = scope AND administrativeUnit.section = section AND NOT EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope actorScopeAdministrativeUnit "
					+ "WHERE actorScopeAdministrativeUnit.scope = scope AND actorScopeAdministrativeUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND (actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))"
					+ ")" + 
			"    )" + 
			")";
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = 
			"SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsByActorCode(String actorCode);
	
	/* count visible administrative units by actor code */
	String QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "countVisibleAdministrativeUnitsByActorCode";
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE;
	Long countVisibleAdministrativeUnitsByActorCode(String actorCode);
	
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
	
	Collection<Scope> readVisibleByActorCode(String actorCode);
	Long countVisibleByActorCode(String actorCode);
	
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
		public Collection<Scope> readByCodesByTypesCodes(Collection<String> codes,Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES
					, PARAMETER_NAME_CODES,codes,PARAMETER_NAME_TYPES_CODES,typesCodes);
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
		public Collection<Scope> readVisibleSectionsByActorCode(String actorCode) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		
		@Override
		public Long countVisibleSectionsByActorCode(String actorCode) {
			return EntityCounter.getInstance().count(Scope.class, QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsByActorCode(String actorCode) {
			return EntityReader.getInstance().readMany(Scope.class, QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsByActorCode(String actorCode) {
			return EntityCounter.getInstance().count(Scope.class, QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE,PARAMETER_NAME_ACTOR_CODE,actorCode);
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
		
		@Override
		public Collection<Scope> readVisibleByActorCode(String actorCode) {
			Collection<Scope> scopes = null;
			Collection<Scope> sections = readVisibleSectionsByActorCode(actorCode);
			if(CollectionHelper.isNotEmpty(sections)) {
				if(scopes == null)
					scopes = new ArrayList<>();
				scopes.addAll(sections);
			}
			Collection<Scope> administrativeUnits = readVisibleAdministrativeUnitsByActorCode(actorCode);
			if(CollectionHelper.isNotEmpty(administrativeUnits)) {
				if(scopes == null)
					scopes = new ArrayList<>();
				scopes.addAll(administrativeUnits);
			}
			return scopes;
		}
		
		@Override
		public Long countVisibleByActorCode(String actorCode) {
			return NumberHelper.getLong(NumberHelper.add(countVisibleSectionsByActorCode(actorCode),countVisibleAdministrativeUnitsByActorCode(actorCode)));
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_CODES_BY_TYPES_CODES,Query.FIELD_TUPLE_CLASS,Scope.class
				,Query.FIELD_RESULT_CLASS,Scope.class,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_CODES_BY_TYPES_CODES
				)
			);
		
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE
				)
			);
	}
}