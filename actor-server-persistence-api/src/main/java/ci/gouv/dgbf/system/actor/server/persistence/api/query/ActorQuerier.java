package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.exists;
import static org.cyk.utility.persistence.query.Language.Where.like;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.time.ZoneOffset;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.persistence.NoResultException;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.time.TimeHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityFinder;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.Language.Select;
import org.cyk.utility.persistence.query.Language.Where;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.api.ActorPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorProfile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationForm;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfilePrivilege;

public interface ActorQuerier extends Querier {

	String FLAG_SEARCH = "Actor.flag.search";
	
	String PARAMETER_NAME_SEARCH = "search";
	
	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	String PARAMETER_NAME_FUNCTION_CODE = "functionCode";
	String PARAMETER_NAME_FUNCTION_CODE_NULLABLE = PARAMETER_NAME_FUNCTION_CODE+"Nullable";
	String PARAMETER_NAME_PROFILE_CODE = "profileCode";
	String PARAMETER_NAME_PROFILE_CODE_NULLABLE = PARAMETER_NAME_PROFILE_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_SCOPE_IDENTIFIER = "visibleScopeIdentifier";
	String PARAMETER_NAME_VISIBLE_SCOPE_TYPE_CODE = "visibleScopeTypeCode";
	
	String PARAMETER_NAME_VISIBLE_SECTION_CODE = "visibleSectionCode";
	String PARAMETER_NAME_VISIBLE_SECTION_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_SECTION_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_SECTION_NAME = "visibleSectionName";
	
	String PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE = "visibleBudgetSpecializationUnitCode";
	String PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_NAME = "visibleBudgetSpecializationUnitName";
	
	String PARAMETER_NAME_VISIBLE_ACTION_CODE = "visibleActionCode";
	String PARAMETER_NAME_VISIBLE_ACTION_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_ACTION_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_ACTION_NAME = "visibleActionName";
	
	String PARAMETER_NAME_VISIBLE_ACTIVITY_CODE = "visibleActivityCode";
	String PARAMETER_NAME_VISIBLE_ACTIVITY_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_ACTIVITY_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_ACTIVITY_NAME = "visibleActivityName";
	
	String PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE = "visibleActivityCategoryCode";
	String PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_NAME = "visibleActivityCategoryName";
	
	String PARAMETER_NAME_VISIBLE_IMPUTATION_CODE = "visibleImputationCode";
	String PARAMETER_NAME_VISIBLE_IMPUTATION_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_IMPUTATION_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_IMPUTATION_NAME = "visibleImputationName";
	
	String PARAMETER_NAME_VISIBLE_ADMINISTRATIVE_UNIT_CODE = "visibleAdministrativeUnitCode";
	String PARAMETER_NAME_VISIBLE_ADMINISTRATIVE_UNIT_CODE_NULLABLE = PARAMETER_NAME_VISIBLE_ADMINISTRATIVE_UNIT_CODE+"Nullable";
	String PARAMETER_NAME_VISIBLE_ADMINISTRATIVE_UNIT_NAME = "visibleAdministrativeUnitName";
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Actor.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Actor.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Actor.class, QueryName.COUNT_DYNAMIC);
	
	/* Read profile informations by identifier */
	String QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(Actor.class, "readProfileInformationsByIdentifierForUI");
	Actor readProfileInformationsByIdentifierForUI(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(Actor.class, "readByIdentifierForEdit");
	Actor readByIdentifierForEdit(String identifier);
	
	Actor instantiateOneToBeCreatedByPublic();
	
	String readElectronicMailAddressByCode(String code);
	
	//Collection<Actor> readMany(QueryExecutorArguments arguments);
	//Long count(QueryExecutorArguments arguments);
	Actor readOne(QueryExecutorArguments arguments);
	Collection<Actor> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ActorQuerier,Serializable {
		
		@Override
		public String readElectronicMailAddressByCode(String code) {
			if(StringHelper.isBlank(code))
				return null;
			try {
				return (String) EntityManagerGetter.getInstance().get()
						.createQuery(String.format("SELECT t.identity.%s FROM Actor t WHERE t.code = :code",Identity.FIELD_ELECTRONIC_MAIL_ADDRESS))
						.setParameter("code", code)
						.getSingleResult();
			} catch (NoResultException exception) {
				return null;
			}
		}
		
		@Override
		public Actor instantiateOneToBeCreatedByPublic() {
			Actor actor = new Actor();
			actor.setEmailSendableAfterCreation(Boolean.TRUE);
			IdentificationForm form = IdentificationFormQuerier.getInstance().readWithFieldsByCode(IdentificationForm.CODE_CREATION_COMPTE);
			actor.setForm(form);
			return actor;
		}
		
		@Override
		public Actor readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(Actor.class,arguments.setQuery(null));
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_ONE_WITH_ALL_PRIVILEGES_BY_IDENTIFIER.equals(arguments.getQuery().getIdentifier()))
				return readOneWithAllPrivilegesByIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_CODE.equals(arguments.getQuery().getIdentifier()))
				return readProfileInformationsByCode((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_IDENTIFIER_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readProfileInformationsByIdentifierForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments != null && arguments.getQuery() != null && QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT.equals(arguments.getQuery().getIdentifier()))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return QueryExecutor.getInstance().executeReadOne(Actor.class, arguments);
		}
		
		@Override
		public Collection<Actor> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Actor.class,arguments.setQuery(null));
			throw new RuntimeException("Actor read many not yet handled : "+arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(Actor.class,arguments.setQuery(null));
			throw new RuntimeException("Actor count not yet handled : "+arguments);
		}
		
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
		
		@Override
		public Actor readAllInformationsForExternalByCode(String code) {
			if(StringHelper.isBlank(code))
				return null;
			Actor actor = __inject__(ActorPersistence.class).readByBusinessIdentifier(code);
			actor.setPrivileges(PrivilegeQuerier.getInstance().readVisibleByActorCode(code));
			actor.setScopes(ScopeQuerier.getInstance().readVisibleByActorCode(code));
			return actor;
		}
		
		@Override
		public Actor readProfileInformationsByCode(String code) {
			if(StringHelper.isBlank(code))
				return null;
			return QueryExecutor.getInstance().executeReadOne(Actor.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_CODE).addFilterFieldsValues(PARAMETER_NAME_CODE,code));
		}
		
		@Override
		public Actor readProfileInformationsByIdentifierForUI(String identifier) {
			if(StringHelper.isBlank(identifier))
				return null;
			Actor actor = QueryExecutor.getInstance().executeReadOne(Actor.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_IDENTIFIER_FOR_UI).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));			
			actor.setNames(Actor.buildNames(actor.getCivilityAsString(), actor.getFirstName(), actor.getLastNames()));
			if(actor.getActOfAppointmentSignatureDate() != null)
				actor.setActOfAppointmentSignatureDateAsString(TimeHelper.formatLocalDate(actor.getActOfAppointmentSignatureDate(),"dd/MM/yyyy"));
			return actor;
		}
		
		@Override
		public Actor readByIdentifierForEdit(String identifier) {
			Actor actor = QueryExecutor.getInstance().executeReadOne(Actor.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));	
			if(actor == null) {
				return null;
			}
			if(actor.getActOfAppointmentSignatureDate() != null)
				actor.setActOfAppointmentSignatureDateAsTimestamp(actor.getActOfAppointmentSignatureDate().atStartOfDay().toInstant(ZoneOffset.UTC).toEpochMilli());
			return actor;
		}
		
		@Override
		public Collection<Actor> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Actor.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_WHERE_FILTER);
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = IdentityQuerier.AbstractImpl.buildFilterOfWhereFilter(arguments);		
			filter.addFieldContains(PARAMETER_NAME_CODE, arguments);
			filter.addFieldEquals(PARAMETER_NAME_PROFILE_CODE, arguments);
			filter.addFieldsNullable(arguments, PARAMETER_NAME_PROFILE_CODE);
			//filter.addFieldEquals(PARAMETER_NAME_FUNCTION_CODE, arguments);
			//filter.addFieldsNullable(arguments, PARAMETER_NAME_FUNCTION_CODE);
			filter.addFieldsNullable(arguments, PARAMETER_NAME_VISIBLE_SECTION_CODE);
			ScopeOfTypeSectionQuerier.getInstance().prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter,PARAMETER_NAME_VISIBLE_SECTION_CODE,PARAMETER_NAME_VISIBLE_SECTION_NAME);
			
			filter.addFieldsNullable(arguments, PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE);
			ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter,PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE,PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_NAME);
			
			filter.addFieldsNullable(arguments, PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE);
			ScopeOfTypeActivityCategoryQuerier.getInstance().prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter,PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE,PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_NAME);
			
			arguments.setFilter(filter);
		}
		
		@Override
		public Collection<Actor> readWithFunctionsWhereFilter(QueryExecutorArguments arguments) {
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<Actor> actors = readWhereFilter(arguments);
			if(CollectionHelper.isEmpty(actors))
				return null;
			__setFunctions__(actors);
			return actors;
		}
		
		@Override
		public Collection<Actor> readWithAllWhereFilter(QueryExecutorArguments arguments) {
			arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_WHERE_FILTER);
			Collection<Actor> actors = readWhereFilter(arguments);
			if(CollectionHelper.isEmpty(actors))
				return null;
			__setProfiles__(actors);
			//__setFunctions__(actors);
			__setVisibleModules__(actors);
			__setVisibleSections__(actors);
			return actors;
		}
		
		/*@Override
		public Long countWithFunctionsWhereFilter(QueryExecutorArguments arguments) {
			return countWhereFilter(arguments);
		}*/
		
		private void __setProfiles__(Collection<Actor> actors) {
			if(CollectionHelper.isEmpty(actors))
				return;
			Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x -> x.getCode()).collect(Collectors.toList()));
			if(CollectionHelper.isNotEmpty(actorProfiles)) {				
				actors.forEach(actor -> {
					actor.setProfiles(actorProfiles.stream().filter(actorProfile -> actorProfile.getActor().equals(actor))
							.map(actorProfile -> actorProfile.getProfile()).collect(Collectors.toList()));
				});				
			}
		}
		
		private void __setFunctions__(Collection<Actor> actors) {
			if(CollectionHelper.isEmpty(actors))
				return;
			Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x -> x.getCode()).collect(Collectors.toList()));
			if(CollectionHelper.isNotEmpty(actorProfiles)) {
				Collection<ProfileFunction> profileFunctions = ProfileFunctionQuerier.getInstance().readByProfilesCodes(actorProfiles.stream().map(x -> x.getProfile().getCode()).collect(Collectors.toList()));
				if(CollectionHelper.isNotEmpty(profileFunctions)) {
					actors.forEach(actor -> {
						Collection<Profile> profiles = actorProfiles.stream().filter(actorProfile -> actorProfile.getActor().equals(actor))
								.map(actorProfile -> actorProfile.getProfile()).collect(Collectors.toList());
						if(CollectionHelper.isNotEmpty(profiles))
							actor.setFunctions(profileFunctions.stream().filter(profileFunction -> profiles.contains(profileFunction.getProfile())).map(x -> x.getFunction()).collect(Collectors.toList()));
					});
				}
			}
		}
		
		private void __setVisibleModules__(Collection<Actor> actors) {
			if(CollectionHelper.isEmpty(actors))
				return;
			Collection<ActorProfile> actorProfiles = ActorProfileQuerier.getInstance().readByActorsCodes(actors.stream().map(x -> x.getCode()).collect(Collectors.toList()));
			if(CollectionHelper.isNotEmpty(actorProfiles)) {
				Collection<ProfilePrivilege> profilePrivileges = ProfilePrivilegeQuerier.getInstance().readByProfilesCodes(actorProfiles.stream().map(x -> x.getProfile().getCode()).collect(Collectors.toList()));
				if(CollectionHelper.isNotEmpty(profilePrivileges)) {
					actors.forEach(actor -> {
						Collection<Profile> profiles = actorProfiles.stream().filter(actorProfile -> actorProfile.getActor().equals(actor))
								.map(actorProfile -> actorProfile.getProfile()).collect(Collectors.toList());
						if(CollectionHelper.isNotEmpty(profiles))
							actor.setVisibleModules(profilePrivileges.stream()
									.filter(profilePrivilege -> profiles.contains(profilePrivilege.getProfile()))
									.filter(profilePrivilege -> profilePrivilege.getPrivilege().getType().getCode().equals(PrivilegeType.CODE_MODULE))
									.map(x -> x.getPrivilege())
									.collect(Collectors.toList()));
					});
				}
			}
		}
		
		private void __setVisibleSections__(Collection<Actor> actors) {
			if(CollectionHelper.isEmpty(actors))
				return;
			actors.forEach(actor -> {
				actor.setVisibleSections(ScopeOfTypeSectionQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
						.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE,List.of(actor.getCode()))));
			});
		}
	}
	
	/**/
	
	/* Read All 01 */
	/*String QUERY_NAME_READ_ALL_01 = "read.all.01";
	String QUERY_IDENTIFIER_READ_ALL_01 = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_ALL_01);
	Map<String,Integer> QUERY_VALUE_READ_ALL_01_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Actor.FIELD_IDENTIFIER
			,Actor.FIELD_NAMES,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_ALL_01 = Language.of(Select.of("t.identifier,"+Select.concat("t.identity", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)+",t.identity."+Identity.FIELD_ELECTRONIC_MAIL_ADDRESS)
			,From.ofTuple(Actor.class)
			,Order.of(Order.join(Order.asc("t.identity", Identity.FIELD_FIRST_NAME),Order.asc("t.identity", Identity.FIELD_LAST_NAMES))));
	
	String QUERY_NAME_COUNT_ALL_01 = "count.all.01";
	String QUERY_IDENTIFIER_COUNT_ALL_01 = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_COUNT_ALL_01);
	String QUERY_VALUE_COUNT_ALL_01 = "SELECT COUNT(t.identifier) FROM Actor t";
	*/
	/* Read by string */
	String QUERY_NAME_READ_BY_STRING = "readByString";
	String QUERY_IDENTIFIER_READ_BY_STRING = QueryIdentifierBuilder.getInstance().build(Actor.class, QUERY_NAME_READ_BY_STRING);
	Map<String,Integer> QUERY_VALUE_READ_BY_STRING_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(Actor.FIELD_IDENTIFIER,Actor.FIELD_CODE
			,Actor.FIELD_NAMES,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS);
	String QUERY_VALUE_READ_BY_STRING_WHERE = " WHERE LOWER(t.code) LIKE LOWER(:"+PARAMETER_NAME_STRING+") OR LOWER(t.identity.firstName) LIKE LOWER(:"+PARAMETER_NAME_STRING+") OR LOWER(t.identity.firstName) LIKE LOWER(:"+PARAMETER_NAME_STRING+")";
	String QUERY_VALUE_READ_BY_STRING = Language.of(Language.Select.of("t.identifier,t.code,"+Language.Select.concat("t.identity", Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES)+",t.identity."+Identity.FIELD_ELECTRONIC_MAIL_ADDRESS)
			,Language.From.of("Actor t")
			,QUERY_VALUE_READ_BY_STRING_WHERE
			,Language.Order.of(Language.Order.join(Language.Order.asc("t.identity", Identity.FIELD_FIRST_NAME),Language.Order.asc("t.identity", Identity.FIELD_LAST_NAMES))));
	
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
	
	/* Read by electronic mail address */
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(Actor.class, "readByElectronicMailAddress");
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Language.Select.of("t")
			,Language.From.of("Actor t")
			,"WHERE t.identity."+Actor.FIELD_ELECTRONIC_MAIL_ADDRESS+" = :"+PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS
			,Language.Order.of(Language.Order.join(Language.Order.asc("t.identity", Actor.FIELD_FIRST_NAME),Language.Order.asc("t.identity", Actor.FIELD_LAST_NAMES))));
	
	/* Read by code */
	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(Actor.class, "readByCode");
	String QUERY_VALUE_READ_BY_CODE = Language.of(Language.Select.of("t"),Language.From.of("Actor t"),Language.Where.of("t."+Actor.FIELD_CODE+" = :"+PARAMETER_NAME_CODE));
	
	/* Read all informations for external by code */
	String QUERY_IDENTIFIER_READ_ALL_INFORMATIONS_FOR_EXTERNAL_BY_CODE = QueryIdentifierBuilder.getInstance().build(Actor.class, "readAllInformationsForExternalByCode");
	Actor readAllInformationsForExternalByCode(String code);
	
	/* Read profile informations by code */
	String QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_CODE = QueryIdentifierBuilder.getInstance().build(Actor.class, "readProfileInformationsByCode");
	Actor readProfileInformationsByCode(String code);
	
	/* read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Actor.class, "readWhereFilter");
	Collection<Actor> readWhereFilter(QueryExecutorArguments arguments);
	
	/* count where filter */
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/* read with functions where filter */
	String QUERY_IDENTIFIER_READ_WITH_FUNCTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Actor.class, "readWithFunctionsWhereFilter");
	Collection<Actor> readWithFunctionsWhereFilter(QueryExecutorArguments arguments);
	
	/* read with functions where filter */
	String QUERY_IDENTIFIER_READ_WITH_ALL_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(Actor.class, "readWithAllWhereFilter");
	Collection<Actor> readWithAllWhereFilter(QueryExecutorArguments arguments);
	
	/* count with functions where filter */
	/*String QUERY_IDENTIFIER_COUNT_WITH_FUNCTIONS_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WITH_FUNCTIONS_WHERE_FILTER);
	Long countWithFunctionsWhereFilter(QueryExecutorArguments arguments);
	*/
	/**/
	
	static ActorQuerier getInstance() {
		return Helper.getInstance(ActorQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_CODE
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,jpql(
						select(
							FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_FIRST_NAME)
							,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_LAST_NAMES)
							,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_REGISTRATION_NUMBER)
							,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS)
							,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ADMINISTRATIVE_FUNCTION)
							,FieldHelper.join(Identity.FIELD_CIVILITY,Civility.FIELD_CODE)
							,FieldHelper.join("identityGroup",IdentityGroup.FIELD_NAME)
							
							//Joins
							,Select.concatCodeName("section")
							,Select.concatCodeName(Identity.FIELD_ADMINISTRATIVE_UNIT)						
						)
						,from("Actor a"
							,"LEFT JOIN Civility civility ON civility = a.identity.civility"
							,"LEFT JOIN IdentityGroup identityGroup ON identityGroup = a.identity.group"
							,"LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = a.identity.administrativeUnit"
							,"LEFT JOIN Section section ON section = a.identity.administrativeUnit.section"
							)
						,where(Where.equals("a", PARAMETER_NAME_CODE))
					)
				).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(Actor.FIELD_FIRST_NAME,Actor.FIELD_LAST_NAMES,Actor.FIELD_REGISTRATION_NUMBER
						,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS,Actor.FIELD_ADMINISTRATIVE_FUNCTION,Actor.FIELD_CIVILITY_STRING,Actor.FIELD_GROUP_AS_STRING
						//Joins
						,Actor.FIELD_SECTION_AS_STRING,Actor.FIELD_ADMINISTRATIVE_UNIT_AS_STRING))
				
				,Query.buildSelect(Actor.class, QUERY_IDENTIFIER_READ_PROFILE_INFORMATIONS_BY_IDENTIFIER_FOR_UI
						, jpql(getReadProfileInformationsSelect(),getReadProfileInformationsFrom(),where(Where.equals("a", PARAMETER_NAME_IDENTIFIER))) )
					.setTupleFieldsNamesIndexesFromFieldsNames(getReadProfileInformationsTupleFieldsNames())
				
				,Query.buildSelect(Actor.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
						, jpql(getReadForEditSelect(),getReadForEditFrom(),where(Where.equals("a", PARAMETER_NAME_IDENTIFIER))) )
					.setTupleFieldsNamesIndexesFromFieldsNames(getReadForEditTupleFieldsNames())
			);
		/*
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_ALL_01
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_ALL_01
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_ALL_01_TUPLE_FIELDS_NAMES_INDEXES)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_ALL_01
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_ALL_01
				)
			);
		*/
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_STRING
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_STRING
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_BY_STRING_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_STRING
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_STRING
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,IdentityQuerier.getQueryValueReadWhereFilter(Actor.class
						,List.of(Actor.FIELD_CODE),getQueryValueReadWhereFilterAdditionalJoins()
						,List.of(getQueryValueReadWhereFilterAdditionalPredicates()))
				).setTupleFieldsNamesIndexes(IdentityQuerier.getQueryTupleFieldsNamesIndexesReadWhereFilter(Actor.FIELD_CODE))
			);
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,IdentityQuerier.getQueryValueCountWhereFilter(Actor.class
						,getQueryValueReadWhereFilterAdditionalJoins()
						,List.of(getQueryValueReadWhereFilterAdditionalPredicates()))
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
		
		QueryManager.getInstance().register(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_CODE
				,Query.FIELD_TUPLE_CLASS,Actor.class,Query.FIELD_RESULT_CLASS,Actor.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_CODE
				)
			);
	}
	
	static String getQueryValueReadWhereFilterAdditionalJoins() {
		return null;
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicates() {
		return and(
				like("t", Actor.FIELD_CODE, PARAMETER_NAME_CODE)
				,getQueryValueReadWhereFilterAdditionalPredicateHasProfileCode()
				,getQueryValueReadWhereFilterAdditionalPredicateHasVisibleSectionCode()
				,getQueryValueReadWhereFilterAdditionalPredicateHasVisibleBudgetSpecializationUnitCode()
				,getQueryValueReadWhereFilterAdditionalPredicateHasVisibleActivityCategoryCode()
			);
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicateHasFunctionCode() {
		return parenthesis(or(
					":"+PARAMETER_NAME_FUNCTION_CODE_NULLABLE+" = true"
					,exists(
						select("pf")
						,from("ProfileFunction pf")
						,where(and("pf.profile = ap.profile",Where.equals("pf", FieldHelper.join(ProfileFunction.FIELD_FUNCTION,Function.FIELD_CODE), PARAMETER_NAME_FUNCTION_CODE)))
					)
				))
			;
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicateHasProfileCode() {
		return parenthesis(or(
					":"+PARAMETER_NAME_PROFILE_CODE_NULLABLE+" = true"
					,exists(
						select("ap")
						,from("ActorProfile ap")
						,where(and("ap.actor = t",Where.equals("ap", FieldHelper.join(ActorProfile.FIELD_PROFILE,Function.FIELD_CODE), PARAMETER_NAME_PROFILE_CODE)))
					)
				));
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicateHasVisibleSectionCode() {
		return parenthesis(or(
				":"+PARAMETER_NAME_VISIBLE_SECTION_CODE_NULLABLE+" = true"
				,exists(jpql("SELECT scope FROM Scope scope",ScopeOfTypeSectionQuerier.getInstance().buildQueryValueReadVisibleWhereFilterWhere(PARAMETER_NAME_VISIBLE_SECTION_CODE
				,PARAMETER_NAME_VISIBLE_SECTION_NAME,"t.code")))
			));
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicateHasVisibleBudgetSpecializationUnitCode() {
		return parenthesis(or(
				":"+PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE_NULLABLE+" = true"
				,exists(jpql("SELECT scope"
						, ScopeQuerier.getFromWhere(BudgetSpecializationUnit.class, "b", ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance()
								.buildQueryValueReadVisibleWhereFilterWhere(
								PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_CODE
								,PARAMETER_NAME_VISIBLE_BUDGET_SPECIALIZATION_UNIT_NAME,"t.code"))))
			));
	}
	
	static String getQueryValueReadWhereFilterAdditionalPredicateHasVisibleActivityCategoryCode() {
		return parenthesis(or(
				":"+PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE_NULLABLE+" = true"
				,exists(jpql("SELECT scope FROM Scope scope",ScopeOfTypeSectionQuerier.getInstance().buildQueryValueReadVisibleWhereFilterWhere(PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_CODE
				,PARAMETER_NAME_VISIBLE_ACTIVITY_CATEGORY_NAME,"t.code")))
			));
	}
	
	/**/
	
	/* Read profile informations */
	
	static String getReadProfileInformationsSelect() {
		return jpql(
				select(
						FieldHelper.join("a",Actor.FIELD_IDENTIFIER)
						,FieldHelper.join("a",Actor.FIELD_CODE)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_FIRST_NAME)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_LAST_NAMES)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_REGISTRATION_NUMBER)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ADMINISTRATIVE_FUNCTION)
						
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_MOBILE_PHONE_NUMBER)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_OFFICE_PHONE_NUMBER)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_OFFICE_PHONE_EXTENSION)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_POSTAL_BOX_ADDRESS)
						
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_REFERENCE)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATORY)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE)
						
						,FieldHelper.join(Identity.FIELD_CIVILITY,Civility.FIELD_CODE)
						,FieldHelper.join("identityGroup",IdentityGroup.FIELD_NAME)
						
						//Joins
						,Select.concatCodeName("section")
						,Select.concatCodeName(Identity.FIELD_ADMINISTRATIVE_UNIT)						
					)	
			);
	}
	
	static String getReadProfileInformationsFrom() {
		return jpql(
			from("Actor a"
				,"LEFT JOIN Civility civility ON civility = a.identity.civility"
				,"LEFT JOIN IdentityGroup identityGroup ON identityGroup = a.identity.group"
				,"LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = a.identity.administrativeUnit"
				,"LEFT JOIN Section section ON section = a.identity.administrativeUnit.section"
			)
		);
	}
	
	static String[] getReadProfileInformationsTupleFieldsNames() {
		return new String[] {
				Actor.FIELD_IDENTIFIER
				,Actor.FIELD_CODE
				,Actor.FIELD_FIRST_NAME
				,Actor.FIELD_LAST_NAMES
				,Actor.FIELD_REGISTRATION_NUMBER
				,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS
				,Actor.FIELD_ADMINISTRATIVE_FUNCTION
				
				,Actor.FIELD_MOBILE_PHONE_NUMBER
				,Actor.FIELD_OFFICE_PHONE_NUMBER
				,Actor.FIELD_OFFICE_PHONE_EXTENSION
				,Actor.FIELD_POSTAL_BOX_ADDRESS
				
				,Actor.FIELD_ACT_OF_APPOINTMENT_REFERENCE
				,Actor.FIELD_ACT_OF_APPOINTMENT_SIGNATORY
				,Actor.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE
				
				,Actor.FIELD_CIVILITY_STRING
				,Actor.FIELD_GROUP_AS_STRING
				//Joins
				,Actor.FIELD_SECTION_AS_STRING
				,Actor.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
		};
	}
	
	/* Read for edit*/
	
	static String getReadForEditSelect() {
		return jpql(
				select(
						FieldHelper.join("a",Actor.FIELD_IDENTIFIER)
						//Account
						,FieldHelper.join("a",Actor.FIELD_CODE)
						//Identity
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_FIRST_NAME)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_LAST_NAMES)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_REGISTRATION_NUMBER)
						,FieldHelper.join(Identity.FIELD_CIVILITY)
						,FieldHelper.join("identityGroup")
						//Contacts
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_MOBILE_PHONE_NUMBER)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_OFFICE_PHONE_NUMBER)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_OFFICE_PHONE_EXTENSION)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_POSTAL_BOX_ADDRESS)
						//Administrative
						,FieldHelper.join(Actor.FIELD_ADMINISTRATIVE_UNIT)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ADMINISTRATIVE_FUNCTION)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_REFERENCE)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATORY)
						,FieldHelper.join("a",Actor.FIELD_IDENTITY,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE)
					)	
			);
	}
	
	static String getReadForEditFrom() {
		return jpql(
			from("Actor a"
				,"LEFT JOIN Civility civility ON civility = a.identity.civility"
				,"LEFT JOIN IdentityGroup identityGroup ON identityGroup = a.identity.group"
				,"LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = a.identity.administrativeUnit"
			)
		);
	}
	
	static String[] getReadForEditTupleFieldsNames() {
		return new String[] {
				Actor.FIELD_IDENTIFIER
				,Actor.FIELD_CODE
				,Actor.FIELD_FIRST_NAME
				,Actor.FIELD_LAST_NAMES
				,Actor.FIELD_REGISTRATION_NUMBER
				,Actor.FIELD_CIVILITY
				,Actor.FIELD_GROUP
				
				,Actor.FIELD_ELECTRONIC_MAIL_ADDRESS
				,Actor.FIELD_MOBILE_PHONE_NUMBER
				,Actor.FIELD_OFFICE_PHONE_NUMBER
				,Actor.FIELD_OFFICE_PHONE_EXTENSION
				,Actor.FIELD_POSTAL_BOX_ADDRESS
				
				,Actor.FIELD_ADMINISTRATIVE_UNIT
				,Actor.FIELD_ADMINISTRATIVE_FUNCTION
				,Actor.FIELD_ACT_OF_APPOINTMENT_REFERENCE
				,Actor.FIELD_ACT_OF_APPOINTMENT_SIGNATORY
				,Actor.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE
		};
	}
}