package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public interface AdministrativeUnitQuerier extends Querier.CodableAndNamable<AdministrativeUnit> {

	String PARAMETER_NAME_SEARCH = "search";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, QueryName.COUNT_DYNAMIC);
	
	String PARAMETER_NAME_SECTIONS_IDENTIFIERS = "sectionsIdentifiers";
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	String PARAMETER_NAME_SERVICE_GROUP_CODE = "serviceGroupCode";
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readByIdentifierWithCodesNamesForUI");
	AdministrativeUnit readByIdentifierWithCodesNamesForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionIdentifier");
	Collection<AdministrativeUnit> readBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionIdentifierForUI");
	Collection<AdministrativeUnit> readBySectionIdentifierForUI(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_BY_SERVICE_GROUP_CODE_STARTS_WITH_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionIdentifierByServiceGroupCodeStartsWithForUI");
	Collection<AdministrativeUnit> readBySectionIdentifierByServiceGroupCodeStartsWithForUI(String sectionIdentifier,String serviceGroupCode);
	
	String QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER);
	Long countBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionsIdentifiers");
	Collection<AdministrativeUnit> readBySectionsIdentifiers(Collection<String> sectionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionsIdentifiersForUI");
	Collection<AdministrativeUnit> readBySectionsIdentifiersForUI(Collection<String> sectionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS);
	Long countBySectionsIdentifiers(Collection<String> sectionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readVisiblesByActorCodeForUI");
	Collection<AdministrativeUnit> readVisiblesByActorCodeForUI(String actorCode);
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_SECTION_IDENTIFIER_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readVisiblesBySectionIdentifierByActorCodeForUI");
	Collection<AdministrativeUnit> readVisiblesBySectionIdentifierByActorCodeForUI(String sectionIdentifier,String actorCode);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<AdministrativeUnit> implements AdministrativeUnitQuerier,Serializable {

		@Override
		public AdministrativeUnit readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(AdministrativeUnit.class,arguments.setQuery(null));
			if(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByIdentifierWithCodesNamesForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_IDENTIFIER));
			return super.readOne(arguments);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Collection<AdministrativeUnit> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(AdministrativeUnit.class,arguments.setQuery(null));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER))
				return readBySectionIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI))
				return readBySectionIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_BY_SERVICE_GROUP_CODE_STARTS_WITH_FOR_UI))
				return readBySectionIdentifierByServiceGroupCodeStartsWithForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER),(String) arguments.getFilterFieldValue(PARAMETER_NAME_SERVICE_GROUP_CODE));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS))
				return readBySectionsIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI))
				return readBySectionsIdentifiersForUI((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_SECTION_IDENTIFIER_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesBySectionIdentifierByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_SECTION_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			return super.readMany(arguments);
			//throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(AdministrativeUnit.class,arguments.setQuery(null));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS))
				return countBySectionsIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER))
				return countBySectionIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			return super.count(arguments);
			//throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public AdministrativeUnit readByIdentifierWithCodesNamesForUI(String identifier) {
			AdministrativeUnit administrativeUnit = DynamicOneExecutor.getInstance().read(AdministrativeUnit.class, new QueryExecutorArguments()
					.addProjectionsFromStrings(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME
							,AdministrativeUnit.FIELD_SECTION_IDENTIFIER,AdministrativeUnit.FIELD_SECTION_CODE_NAME)
					.addProcessableTransientFieldsNames(AdministrativeUnit.FIELD_SUB_PREFECTURE_DEPARTMENT_REGION)
					.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			if(administrativeUnit == null)
				return null;
			//QueryExecutor.getInstance().executeReadOne(AdministrativeUnit.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI)
			//		.addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			administrativeUnit.setSection(Section.instantiateFromIdentifierCodeName(Section.class,administrativeUnit.getSectionIdentifier(),administrativeUnit.getSectionCodeName()));
			return administrativeUnit;
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER
					,sectionIdentifier);
		}
		
		@Override
		public Long countBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionIdentifierForUI(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionIdentifierByServiceGroupCodeStartsWithForUI(String sectionIdentifier,String serviceGroupCode) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_BY_SERVICE_GROUP_CODE_STARTS_WITH_FOR_UI
					, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier,PARAMETER_NAME_SERVICE_GROUP_CODE,serviceGroupCode+"%");
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionsIdentifiers(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS, PARAMETER_NAME_SECTIONS_IDENTIFIERS
					,sectionsIdentifiers);
		}
		
		@Override
		public Long countBySectionsIdentifiers(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS, PARAMETER_NAME_SECTIONS_IDENTIFIERS,sectionsIdentifiers);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionsIdentifiersForUI(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI, PARAMETER_NAME_SECTIONS_IDENTIFIERS
					,sectionsIdentifiers);
		}
		
		@Override
		public Collection<AdministrativeUnit> readVisiblesBySectionIdentifierByActorCodeForUI(String sectionIdentifier,String actorCode) {
			if(StringHelper.isBlank(actorCode))
				return null;
			Collection<Scope> scopes =  ScopeOfTypeAdministrativeUnitQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ScopeOfTypeAdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER)
					.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
			if(CollectionHelper.isEmpty(scopes))
				return null;
			Collection<AdministrativeUnit> administrativeUnits = EntityFinder.getInstance().findMany(AdministrativeUnit.class, FieldHelper.readSystemIdentifiersAsStrings(scopes));
			if(StringHelper.isNotBlank(sectionIdentifier))
				administrativeUnits = administrativeUnits.stream().filter(x -> x.getSection() != null && x.getSection().getIdentifier().equals(sectionIdentifier))
				.collect(Collectors.toList());
			return administrativeUnits.stream().map(x -> new AdministrativeUnit().setIdentifier(x.getIdentifier()).setCode(x.getCode()).setName(x.getName())
					.setSectionIdentifier(x.getSection() == null ? null : x.getSection().getIdentifier()))
					.collect(Collectors.toList());
		}
		
		@Override
		public Collection<AdministrativeUnit> readVisiblesByActorCodeForUI(String actorCode) {
			if(StringHelper.isBlank(actorCode))
				return null;
			return readVisiblesBySectionIdentifierByActorCodeForUI(null,actorCode);
		}
		
		@Override
		protected Class<AdministrativeUnit> getKlass() {
			return AdministrativeUnit.class;
		}
	}
	
	/**/
	
	static AdministrativeUnitQuerier getInstance() {
		return Helper.getInstance(AdministrativeUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(AdministrativeUnit.class,Query.build(Query.FIELD_IDENTIFIER
				,QueryIdentifierGetter.getInstance().get(AdministrativeUnit.class, QueryName.READ_WHERE_CODE_OR_NAME_LIKE)
				,Query.FIELD_TUPLE_CLASS,AdministrativeUnit.class,Query.FIELD_RESULT_CLASS,AdministrativeUnit.class
				,Query.FIELD_VALUE,Querier.CodableAndNamable.getQueryValueReadWhereCodeOrNameLike("AdministrativeUnit", "t.identifier,t.code,t.name,t.sectionCodeName")
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(AdministrativeUnit
						.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME,AdministrativeUnit.FIELD_SECTION_CODE_NAME)));
		QueryManager.getInstance().register(
			Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER
					, "SELECT t FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" ORDER BY t.code ASC")
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER
					, "SELECT COUNT(t.identifier) FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER)
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME)
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_BY_SERVICE_GROUP_CODE_STARTS_WITH_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" AND t.serviceGroupCodeName LIKE :"+PARAMETER_NAME_SERVICE_GROUP_CODE+" ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME)
				
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS
					, "SELECT t FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS+" ORDER BY t.code ASC")
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS
					, "SELECT COUNT(t.identifier) FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS)
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS+" ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME)
			
			/*,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI
					, "SELECT t.identifier,t.code,t.name"
							+ ",s.identifier,t.sectionCodeName"
							+ ",s.identifier,t.sectionCodeName"
							+ ",s.identifier,t.sectionCodeName"
							+ ",s.identifier,t.sectionCodeName"
							+ " FROM AdministrativeUnit t "
							+ "LEFT JOIN Section s ON s = t.section "
							+ "WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
				.setTupleFieldsNamesIndexesFromFieldsNames(
						AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME
						,AdministrativeUnit.FIELD_SECTION_IDENTIFIER,AdministrativeUnit.FIELD_SECTION_CODE_NAME
						,AdministrativeUnit.FIELD_SUB_PREFECTURE_IDENTIFIER,AdministrativeUnit.FIELD_SUB_PREFECTURE_CODE_NAME
						,AdministrativeUnit.FIELD_DEPARTMENT_IDENTIFIER,AdministrativeUnit.FIELD_DEPARTMENT_CODE_NAME
						,AdministrativeUnit.FIELD_REGION_IDENTIFIER,AdministrativeUnit.FIELD_REGION_CODE_NAME
						)*/	
		);
	}
}