package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public interface ScopeQuerier extends Querier {

	String PARAMETER_NAME_TYPE = "type";
	String PARAMETER_NAME_TYPE_CODE = "typeCode";
	String PARAMETER_NAME_TYPE_NAME = "typeName";
	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME = 4;
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	/* read where filter order by type code ascending by code ascending */
	String QUERY_NAME_READ_WHERE_FILTER = "readWhereFilter";
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_FILTER);
	String QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE = Language.From.of("Scope t")+" "+Language.Where.of(Language.Where.and(
		Language.Where.like("t", "type.code", PARAMETER_NAME_TYPE_CODE)
		,Language.Where.like("t", "type.name", PARAMETER_NAME_TYPE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME)
		,Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
		,Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
		,"(:"+PARAMETER_NAME_ACTOR_CODE_NULLABLE+" = true OR EXISTS(SELECT t0 FROM ActorScope t0 WHERE t0.scope = t AND "+Language.Where.like("t0", "actor.code", PARAMETER_NAME_ACTOR_CODE)+"))"
		));
	String QUERY_VALUE_READ_WHERE_FILTER = Language.of(Language.Select.of("t.identifier,t.code,t.name"),QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_NAME_COUNT_WHERE_FILTER = "countWhereFilter";
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_FILTER);
	String QUERY_VALUE_COUNT_WHERE_FILTER = Language.of(Language.Select.of("COUNT(t.identifier)"),QUERY_VALUE_READ_WHERE_FILTER_FROM_WHERE);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* read where type is ua and filter order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_UA_AND_FILTER = "readWhereTypeIsUAAndFilter";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_TYPE_IS_UA_AND_FILTER);
	Map<String,Integer> QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE
			,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE = " FROM Scope t "
			+ " JOIN AdministrativeUnit administrativeUnit ON administrativeUnit.identifier = t.identifier "
			+ " JOIN Section section ON section.identifier = administrativeUnit.section "
			+ " JOIN Scope sectionScope ON sectionScope.identifier = section.identifier "
			+Language.Where.of(Language.Where.and(				
					Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE),Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
				));
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER = "SELECT t.identifier,t.code,t.name,"+Language.Select.concatCodeName("sectionScope")+ QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE+ " ORDER BY t.code ASC";
	Collection<Scope> readWhereTypeIsUAAndFilter(QueryExecutorArguments arguments);
	
	/* count where type is ua and filter */
	String QUERY_NAME_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = "countWhereTypeIsUAAndFilter";
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_TYPE_IS_UA_AND_FILTER);
	String QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA_AND_FILTER = "SELECT COUNT(t.identifier) "+QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER_FROM_WHERE;
	Long countWhereTypeIsUAAndFilter(QueryExecutorArguments arguments);
	
	/**************************************************************************************************************************************************************************/
	
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
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE = Language.of(Language.From.of("Scope t JOIN ActorScope actorScope ON actorScope.scope = t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,"actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)));
	String QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("t"),QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE
			,Language.Order.of("t.type.code ASC,t.code ASC"));
	Collection<Scope> readByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* count by actors codes by scopes types codes */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = "countByActorsCodesByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t)"),QUERY_VALUE_READ_BY_ACTORS_CODES_BY_TYPES_CODES_FROM_WHERE);
	Long countByActorsCodesByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* read visible sections by actor code order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleSectionsByActorCode");
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
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleSectionsByActorCode(String actorCode);
	
	/* count visible sections by actor code */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_SECTIONS_BY_ACTOR_CODE = "SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_BY_ACTOR_CODE_WHERE;
	Long countVisibleSectionsByActorCode(String actorCode);
	
	/* read visible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleSectionsWhereFilter");
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE =
			"(EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope " + 
			" WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier) "+ 
			" OR " + 
			" EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope " + 
			" JOIN Scope scopeUa ON actorScope.scope = scopeUa JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeUa " + 
			" WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND administrativeUnit.section = scope" + 
			" ))";
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE_PREDICATE = Language.Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
			,QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE_PREDICATE);
			/*
			"WHERE scope.type.code = '"+ScopeType.CODE_SECTION+"' AND ("+
			"    EXISTS (" + 
			"        SELECT actorScope.identifier FROM ActorScope actorScope " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND actorScope.scope.identifier = scope.identifier "+ 
			"    ) " + 
			"    OR " + 
			"    EXISTS (" + 
			"        SELECT actorScope.identifier FROM ActorScope actorScope " + 
			"        JOIN Scope scopeUa ON actorScope.scope = scopeUa JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = scopeUa " + 
			"        WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND administrativeUnit.section = scope" + 
			"    )" + 
			") AND "+Language.Where.and(Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			);
			*/
	
	String QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_VISIBLE_SECTIONS_WHERE_FILTER = "SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE;
	Long countVisibleSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleSectionsWhereFilter");
	String QUERY_VALUE_READ_INVISIBLE_SECTIONS_WHERE_FILTER_WHERE = "WHERE "+Language.Where.and("scope.type.code = '"+ScopeType.CODE_SECTION+"'"
			,"NOT "+QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
	String QUERY_VALUE_READ_INVISIBLE_SECTIONS_WHERE_FILTER = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_INVISIBLE_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readInvisibleSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible sections where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_INVISIBLE_SECTIONS_WHERE_FILTER = "SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_INVISIBLE_SECTIONS_WHERE_FILTER_WHERE;
	Long countInvisibleSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible administrative units by actor code order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsByActorCode");
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
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsByActorCode(String actorCode);
	
	/* count visible administrative units by actor code */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE;
	Long countVisibleAdministrativeUnitsByActorCode(String actorCode);
	
	/* read visible administrative units where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsWhereFilter");
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE = 
			"(EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE
			+" AND actorScope.scope.identifier = scope.identifier AND (actorScope.visible IS NULL OR actorScope.visible = true)) " + 
			" OR " + 
			" EXISTS (SELECT actorScope.identifier FROM ActorScope actorScope JOIN Scope scopeSection ON actorScope.scope = scopeSection " + 
			" JOIN Section section ON section = scopeSection WHERE actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND EXISTS("
			+ "SELECT administrativeUnit FROM AdministrativeUnit administrativeUnit "
			+ "WHERE administrativeUnit = scope AND administrativeUnit.section = section AND NOT EXISTS(SELECT actorScopeAdministrativeUnit FROM ActorScope actorScopeAdministrativeUnit "
			+ "WHERE actorScopeAdministrativeUnit.scope = scope AND actorScopeAdministrativeUnit.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+" AND (actorScopeAdministrativeUnit.visible IS NOT NULL AND actorScopeAdministrativeUnit.visible = false))"
			+ ")))";
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
			,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = "SELECT scope FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible administrative units where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE;
	Long countVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible administrative units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisibleAdministrativeUnitsWithSectionsWhereFilter");
	Map<String,Integer> QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = 
			MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
			,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
		+ QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count visible administrative units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read invisible administrative units with sections where filter order by code ascending */
	String QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisibleAdministrativeUnitsWithSectionsWhereFilter");
	String QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE = Language.Where.of(Language.Where.and("scope.type.code = '"+ScopeType.CODE_UA+"'"
			,"NOT "+QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER_WHERE_PREDICATE_VISIBLE,Language.Where.like("scope", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("scope", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			));
	String QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = "SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
		+ " FROM Scope scope " 
		+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
		+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
		+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
		+ QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE+ " ORDER BY scope.code ASC";
	Collection<Scope> readInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* count invisible administrative units with sections where filter */
	String QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER);
	String QUERY_VALUE_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER = 
			"SELECT COUNT(scope.identifier) FROM Scope scope " + QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_WHERE;
	Long countInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read visible administrative units by actor code order by code ascending */
	String QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = "readVisibleAdministrativeUnitsWithSectionByActorCode";
	String QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE);
	Map<String,Integer> QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE_TUPLE_FIELDS_NAMES_INDEXES = 
			MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = 
			"SELECT scope.identifier,scope.code,scope.name,"+Language.Select.concatCodeName("scopeOfSection")
			+ " FROM Scope scope " 
			+ " JOIN AdministrativeUnit administrativeUnitOfScope ON administrativeUnitOfScope.identifier = scope.identifier "
			+ " JOIN Section sectionOfAdministrativeUnit ON sectionOfAdministrativeUnit.identifier = administrativeUnitOfScope.section "
			+ " JOIN Scope scopeOfSection ON scopeOfSection.identifier = sectionOfAdministrativeUnit.identifier "
			+ QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_BY_ACTOR_CODE_WHERE
			+ " ORDER BY scope.code ASC";
	
	/* count visible administrative units by actor code */
	String QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = "countVisibleAdministrativeUnitsWithSectionByActorCode";
	String QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE);
	
	/* read by actors codes not associated by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, "readByActorsCodesNotAssociatedByTypesCodes");
	String QUERY_VALUE_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES+")"))			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Scope> readByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Scope t")			
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
					"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code IN :"
							+PARAMETER_NAME_ACTORS_CODES+")"))
			);
	Long countByActorsCodesNotAssociatedByTypesCodes(Collection<String> actorsCodes,Collection<String> typesCodes);
	
	/* read by actor code not associated by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Scope.class, "readByActorCodeNotAssociatedByTypesCodes");
	String QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE = Language.of(Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES,
			"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+")")));
	String QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE,Language.Order.of("t.code ASC"));
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,QUERY_VALUE_READ_BY_ACTOR_CODE_NOT_ASSOCIATED_BY_TYPES_CODES_FROM_WHERE);
	
	/* read where filter not associated order by code ascending */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().build(Scope.class, "readWhereFilterNotAssociated");
	String QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE = Language.of(Language.From.of("Scope t")
			,Language.Where.of(Language.Where.and(
			"t.type.code = :"+PARAMETER_NAME_TYPE_CODE
			,Language.Where.like("t", Scope.FIELD_CODE, PARAMETER_NAME_CODE)
			,Language.Where.like("t", Scope.FIELD_NAME, PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME)
			,"NOT EXISTS(SELECT actorScope FROM ActorScope actorScope WHERE actorScope.scope.identifier = t.identifier AND actorScope.actor.code = :"+PARAMETER_NAME_ACTOR_CODE+")")
		));
	String QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED = Language.of(Language.Select.of("t")
			,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE,Language.Order.of("t.code ASC"));
	Collection<Scope> readWhereFilterNotAssociated(QueryExecutorArguments arguments);
	
	/* count by actors codes not associated by types codes */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED);
	String QUERY_VALUE_COUNT_WHERE_FILTER_NOT_ASSOCIATED = Language.of(Language.Select.of("COUNT(t.identifier)")
			,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED_FROM_WHERE);
	Long countWhereFilterNotAssociated(QueryExecutorArguments arguments);
	
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
	
	/* read where type is ua order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_UA = "readWhereTypeIsUA";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_READ_WHERE_TYPE_IS_UA);
	Map<String,Integer> QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE
			,Scope.FIELD_NAME,Scope.FIELD_SECTION_AS_STRING);
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM = " FROM Scope t "
			+ " JOIN AdministrativeUnit administrativeUnit ON administrativeUnit.identifier = t.identifier "
			+ " JOIN Section section ON section.identifier = administrativeUnit.section "
			+ " JOIN Scope sectionScope ON sectionScope.identifier = section.identifier ";
	String QUERY_VALUE_READ_WHERE_TYPE_IS_UA = "SELECT t.identifier,t.code,t.name,"+Language.Select.concatCodeName("sectionScope")+ QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM+ " ORDER BY t.code ASC";
	
	/* count where type is ua */
	String QUERY_NAME_COUNT_WHERE_TYPE_IS_UA = "countWhereTypeIsUA";
	String QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA = QueryIdentifierBuilder.getInstance().build(Scope.class, QUERY_NAME_COUNT_WHERE_TYPE_IS_UA);
	String QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA = "SELECT COUNT(t.identifier) "+QUERY_VALUE_READ_WHERE_TYPE_IS_UA_FROM;
	
	Collection<Scope> readVisibleByActorCode(String actorCode);
	Long countVisibleByActorCode(String actorCode);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeQuerier,Serializable {
		
		@Override
		public Collection<Scope> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_TYPE_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_TYPE_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_TYPE_NAME, arguments);	
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			filter.addFieldContains(PARAMETER_NAME_ACTOR_CODE, arguments);
			filter.addFieldsNullable(arguments, PARAMETER_NAME_ACTOR_CODE);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUAAndFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			prepareWhereTypeIsUAAndFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereTypeIsUAAndFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readVisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readInvisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareInvisibleSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareInvisibleSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareInvisibleSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleSectionsWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleAdministrativeUnitsWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Scope> readVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareInvisibleAdministrativeUnitsWithSectionsWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleAdministrativeUnitsWithSectionsWhereFilter(arguments);
		}
		
		@Override
		public Collection<Scope> readWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			prepareWhereFilterNotAssociated(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			prepareWhereFilterNotAssociated(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilterNotAssociated(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			filter.addFieldsEquals(arguments,PARAMETER_NAME_TYPE_CODE);
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
		
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER_NOT_ASSOCIATED
				)
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER_NOT_ASSOCIATED
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(Scope.FIELD_IDENTIFIER,Scope.FIELD_CODE,Scope.FIELD_NAME))
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_TYPE_IS_UA
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_TUPLE_FIELDS_NAMES_INDEXES)
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_TYPE_IS_UA_AND_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_UA_AND_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_TYPE_IS_UA_AND_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_TYPE_IS_UA_AND_FILTER
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_SECTIONS_WHERE_FILTER
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_SECTIONS_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_INVISIBLE_SECTIONS_WHERE_FILTER
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_INVISIBLE_SECTIONS_WHERE_FILTER
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
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTION_BY_ACTOR_CODE_TUPLE_FIELDS_NAMES_INDEXES)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Scope.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_IDENTIFIER_READ_VISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Scope.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_INVISIBLE_ADMINISTRATIVE_UNITS_WITH_SECTIONS_WHERE_FILTER
				)
			);
	}
}