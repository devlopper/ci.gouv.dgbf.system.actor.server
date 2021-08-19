package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.isNotNull;
import static org.cyk.utility.persistence.query.Language.Where.isNull;
import static org.cyk.utility.persistence.query.Language.Where.or;

import java.io.Serializable;
import java.util.Map;

import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.ValueConverter;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.JoinStringBuilder;
import org.cyk.utility.persistence.server.query.string.LikeStringBuilder;
import org.cyk.utility.persistence.server.query.string.LikeStringValueBuilder;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder.Predicate;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorScopeRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeTypeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ActorScopeRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryStringBuilderImpl extends org.cyk.utility.persistence.server.hibernate.RuntimeQueryStringBuilderImpl implements Serializable {
		
	@Override
	protected void setProjection(QueryExecutorArguments arguments, Arguments builderArguments) {
		if(arguments.getQuery().getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)
				|| arguments.getQuery().getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC_ONE)) {
			if(Boolean.TRUE.equals(arguments.isFlagged(AssignmentsQuerier.FLAG_APPLY_MODEL))) {
				builderArguments.getProjection(Boolean.TRUE).add("t");
			}else {
				/*
				builderArguments.getProjection(Boolean.TRUE)
				.addFromTuple("t", Assignments.FIELD_IDENTIFIER)
				.addFromTuple("i", ExecutionImputation.FIELD_SECTION_CODE
					,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE
					,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE,ExecutionImputation.FIELD_ACTIVITY_CODE
					,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE,ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE)
				.addFromTuple(Assignments.FIELD_CREDIT_MANAGER_HOLDER, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_CREDIT_MANAGER_ASSISTANT, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_ACCOUNTING_HOLDER, ScopeFunction.FIELD_CODE)
				.addFromTuple(Assignments.FIELD_ACCOUNTING_ASSISTANT, ScopeFunction.FIELD_CODE)
				;
				*/
				super.setProjection(arguments, builderArguments);
			}
			return;
		}								
		super.setProjection(arguments, builderArguments);
	}
	
	@Override
	protected void setTuple(QueryExecutorArguments arguments, Arguments builderArguments) {
		super.setTuple(arguments, builderArguments);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Assignments.class)) {
			builderArguments.getTuple(Boolean.TRUE).add("Assignments t");
			builderArguments.getTuple().addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t").setTupleName("ExecutionImputation")
					.setVariableName("i")));
			
			for(String fieldName : new String[] {Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT
					,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT
					,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT
					,Assignments.FIELD_ACCOUNTING_HOLDER,Assignments.FIELD_ACCOUNTING_ASSISTANT}) {
				builderArguments.getTuple().addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments()
						.setMasterVariableName("t").setMasterFieldName(fieldName).setTupleName("ScopeFunction")));
			}
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Actor.class)) {
			builderArguments.getTuple(Boolean.TRUE).add("Actor t");
			if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER) != null || 
					arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_SECTION_IDENTIFIER) != null) {
				builderArguments.getTuple().addJoins("LEFT JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = t.identity.administrativeUnit");
				if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_SECTION_IDENTIFIER) != null) {
					builderArguments.getTuple().addJoins("LEFT JOIN Section section ON section = administrativeUnit.section");
				}
			}			
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(AdministrativeUnit.class)) {
			builderArguments.getTuple(Boolean.TRUE).add("AdministrativeUnit t");
			if(arguments.getFilterFieldValue(AdministrativeUnitQuerier.PARAMETER_NAME_SECTION_IDENTIFIER) != null) {
				builderArguments.getTuple(Boolean.TRUE).addJoins("LEFT JOIN Section section ON section = t.section");
			}			
		}
	}
	
	@Override
	protected void populatePredicate(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		super.populatePredicate(arguments, builderArguments, predicate, filter);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Actor.class)) {
			populatePredicateActor(arguments, builderArguments, predicate, filter);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Locality.class)) {
			populatePredicateLocality(arguments, builderArguments, predicate, filter);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(AdministrativeUnit.class)) {
			populatePredicateAdministrativeUnit(arguments, builderArguments, predicate, filter);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Assignments.class)) {
			populatePredicateAssignments(arguments, builderArguments, predicate, filter);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(ScopeType.class))
			populatePredicateScopeType(arguments, builderArguments, predicate, filter);
		else if(arguments.getQuery().isIdentifierEqualsDynamic(Scope.class))
			populatePredicateScope(arguments, builderArguments, predicate, filter);
		else if(arguments.getQuery().isIdentifierEqualsDynamic(ActorScopeRequest.class))
			populatePredicateActorScopeRequest(arguments, builderArguments, predicate, filter);
	}
	
	protected void populatePredicateLocality(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_TYPE);
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"t.parent",Locality.FIELD_IDENTIFIER);
	}
	
	public static final String PROFILE_PREDICATE_SEARCH = parenthesis(or(
			LikeStringBuilder.getInstance().build("t",Profile.FIELD_CODE, ProfileQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t", Profile.FIELD_NAME,ProfileQuerier.PARAMETER_NAME_SEARCH)
	));
	protected void populatePredicateProfile(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ProfileQuerier.PARAMETER_NAME_IDENTIFIER);
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ProfileQuerier.PARAMETER_NAME_CODE);
		if(arguments.getFilterFieldValue(ProfileQuerier.PARAMETER_NAME_SEARCH) != null) {
			predicate.add(PROFILE_PREDICATE_SEARCH);
			String search = ValueHelper.defaultToIfBlank((String) arguments.getFilterFieldValue(ProfileQuerier.PARAMETER_NAME_SEARCH),"");
			filter.addField(ProfileQuerier.PARAMETER_NAME_SEARCH, LikeStringValueBuilder.getInstance().build(search, null, null));
		}
		if(arguments.getFilterField(ProfileQuerier.PARAMETER_NAME_REQUESTABLE) != null) {
			Boolean requestable = arguments.getFilterFieldValueAsBoolean(null,ProfileQuerier.PARAMETER_NAME_REQUESTABLE);
			if(requestable == null)
				predicate.add("t.requestable IS NOT NULL");
			else {
				predicate.add(String.format("t.requestable = :%s", ProfileQuerier.PARAMETER_NAME_REQUESTABLE));
				filter.addField(ProfileQuerier.PARAMETER_NAME_REQUESTABLE, requestable);
			}
		}
		if(arguments.getFilterFieldValue(ProfileQuerier.PARAMETER_NAME_TYPE_IDENTIFIER) != null) {
			predicate.add(String.format("t.type.identifier = :%s", ProfileQuerier.PARAMETER_NAME_TYPE_IDENTIFIER));
			filter.addFieldEquals(ProfileQuerier.PARAMETER_NAME_TYPE_IDENTIFIER, arguments);
		}
	}
	
	protected void populatePredicateScopeType(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ScopeTypeQuerier.PARAMETER_NAME_IDENTIFIER);
		addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ScopeTypeQuerier.PARAMETER_NAME_CODE);		
		if(arguments.getFilterField(ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE) != null) {
			Boolean requestable = arguments.getFilterFieldValueAsBoolean(null,ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE);
			if(requestable == null)
				predicate.add("t.requestable IS NOT NULL");
			else {
				predicate.add(String.format("t.requestable = :%s", ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE));
				filter.addField(ScopeTypeQuerier.PARAMETER_NAME_REQUESTABLE, requestable);
			}
		}
	}

	public static final String ADMINISTRATIVE_UNIT_PREDICATE_SEARCH = parenthesis(or(
			LikeStringBuilder.getInstance().build("t",AdministrativeUnit.FIELD_CODE, AdministrativeUnitQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t", AdministrativeUnit.FIELD_NAME,AdministrativeUnitQuerier.PARAMETER_NAME_SEARCH)
	));
	protected void populatePredicateAdministrativeUnit(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		if(arguments.getFilterFieldValue(AdministrativeUnitQuerier.PARAMETER_NAME_SEARCH) != null) {
			predicate.add(ADMINISTRATIVE_UNIT_PREDICATE_SEARCH);
			String search = ValueHelper.defaultToIfBlank((String) arguments.getFilterFieldValue(AdministrativeUnitQuerier.PARAMETER_NAME_SEARCH),"");
			filter.addField(AdministrativeUnitQuerier.PARAMETER_NAME_SEARCH, LikeStringValueBuilder.getInstance().build(search, null, null));
		}
		if(arguments.getFilterFieldValue(AdministrativeUnitQuerier.PARAMETER_NAME_SECTION_IDENTIFIER) != null) {
			predicate.add(String.format("section.identifier = :%s", AdministrativeUnitQuerier.PARAMETER_NAME_SECTION_IDENTIFIER));
			filter.addFieldEquals(AdministrativeUnitQuerier.PARAMETER_NAME_SECTION_IDENTIFIER, arguments);
		}
	}
	
	public static final String ACTOR_PREDICATE_SEARCH = parenthesis(or(
			LikeStringBuilder.getInstance().build("t",Actor.FIELD_CODE, ActorQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t.identity", Actor.FIELD_FIRST_NAME,ActorQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t.identity", Actor.FIELD_LAST_NAMES,ActorQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t.identity", Actor.FIELD_ELECTRONIC_MAIL_ADDRESS,ActorQuerier.PARAMETER_NAME_SEARCH)
	));
	protected void populatePredicateActor(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		addIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ActorQuerier.PARAMETER_NAME_CODE);
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_SEARCH) != null) {
			predicate.add(ACTOR_PREDICATE_SEARCH);
			String search = ValueHelper.defaultToIfBlank((String) arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_SEARCH),"");
			filter.addField(ActorQuerier.PARAMETER_NAME_SEARCH, LikeStringValueBuilder.getInstance().build(search, null, null));
		}
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_IDENTIFIER) != null) {
			String visible = VisibilityQueryStringBuilder.Predicate.actorView((String)arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_TYPE_CODE)
					,"tt","t", Boolean.TRUE,!arguments.getFilterFieldValueAsBoolean(Boolean.TRUE,ActorQuerier.PARAMETER_NAME_SCOPE_VISIBLE));
			predicate.add(String.format("EXISTS(SELECT tt FROM Scope tt WHERE %s)", visible));
			filter.addField("scopeIdentifier", arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_VISIBLE_SCOPE_IDENTIFIER));
		}
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_PROFILE_IDENTIFIER) != null) {
			predicate.add(String.format("EXISTS(SELECT ap FROM ActorProfile ap WHERE ap.actor = t AND ap.profile.identifier = :%s)", ActorQuerier.PARAMETER_NAME_PROFILE_IDENTIFIER));
			filter.addFieldEquals(ActorQuerier.PARAMETER_NAME_PROFILE_IDENTIFIER, arguments);
		}
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER) != null) {
			predicate.add(String.format("administrativeUnit.identifier = :%s", ActorQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER));
			filter.addFieldEquals(ActorQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER, arguments);
		}
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_SECTION_IDENTIFIER) != null) {
			predicate.add(String.format("section.identifier = :%s", ActorQuerier.PARAMETER_NAME_SECTION_IDENTIFIER));
			filter.addFieldEquals(ActorQuerier.PARAMETER_NAME_SECTION_IDENTIFIER, arguments);
		}
		if(arguments.getFilterFieldValue(ActorQuerier.PARAMETER_NAME_IS_ACTOR_SCOPE_REQUEST_WHERE_GRANTED_IS_NULL_EXIST) != null) {
			String string = "EXISTS(SELECT asr FROM ActorScopeRequest asr WHERE asr.actor = t AND asr.granted IS NULL)";
			if(!arguments.getFilterFieldValueAsBoolean(Boolean.TRUE,ActorQuerier.PARAMETER_NAME_IS_ACTOR_SCOPE_REQUEST_WHERE_GRANTED_IS_NULL_EXIST))
				string = Language.Where.not(string);
			predicate.add(string);
		}
	}
	
	public static final String SCOPE_PREDICATE_SEARCH = parenthesis(or(
			LikeStringBuilder.getInstance().build("t",Scope.FIELD_CODE, ScopeQuerier.PARAMETER_NAME_SEARCH)
			,LikeStringBuilder.getInstance().build("t", Scope.FIELD_NAME,ScopeQuerier.PARAMETER_NAME_SEARCH)
	));
	protected void populatePredicateScope(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		String scopeTypeCode = (String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_TYPE_CODE);
		String actorCode = (String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE);
		if(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_TYPE_IDENTIFIER) != null) {
			predicate.add(String.format("t.type.identifier = :%s", ScopeQuerier.PARAMETER_NAME_TYPE_IDENTIFIER));
			filter.addFieldEquals(ScopeQuerier.PARAMETER_NAME_TYPE_IDENTIFIER, arguments);
		}
		if(StringHelper.isNotBlank(scopeTypeCode)/* && arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_VISIBLE) == null*/) {
			predicate.add("t.type.code = :"+ScopeQuerier.PARAMETER_NAME_TYPE_CODE);
			filter.addFieldEquals(ScopeQuerier.PARAMETER_NAME_TYPE_CODE, arguments);
		}
		if(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_CODE) != null) {
			predicate.add(LikeStringBuilder.getInstance().build("t", Scope.FIELD_CODE, ScopeQuerier.PARAMETER_NAME_CODE));
			filter.addFieldContains(ScopeQuerier.PARAMETER_NAME_CODE, arguments);
		}
		if(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_NAME) != null) {
			predicate.add(LikeStringBuilder.getInstance().build("t", Scope.FIELD_NAME, ScopeQuerier.PARAMETER_NAME_NAME,ScopeQuerier.NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME));
			filter.addFieldContainsStringOrWords(ScopeQuerier.PARAMETER_NAME_NAME, ScopeQuerier.NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		}
		if(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_VISIBLE) != null) {
			/*
			Object visibleObject = arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_VISIBLE);
			if(visibleObject instanceof String)
				visibleObject = Boolean.TRUE.toString().equalsIgnoreCase(visibleObject.toString());
			predicate.add(ScopeQueryStringBuilder.Predicate.scopeVisible(scopeTypeCode, StringHelper.isNotBlank(actorCode),!ValueHelper.defaultToIfNull((Boolean)visibleObject, Boolean.TRUE)));
			*/
			predicate.add(VisibilityQueryStringBuilder.Predicate.scopeVisible(scopeTypeCode,"t",null, StringHelper.isNotBlank(actorCode)
					,!arguments.getFilterFieldValueAsBoolean(Boolean.TRUE,ScopeQuerier.PARAMETER_NAME_VISIBLE)));
			
			if(StringHelper.isNotBlank(actorCode))
				filter.addField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);			
		}
		//if(CollectionHelper.contains(arguments.getFlags(),ScopeQuerier.FLAG_SEARCH)) {
		if(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_SEARCH) != null) {
			predicate.add(SCOPE_PREDICATE_SEARCH);
			String search = ValueHelper.defaultToIfBlank((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_SEARCH),"");
			filter.addField(ScopeQuerier.PARAMETER_NAME_SEARCH, LikeStringValueBuilder.getInstance().build(search, null, null));
		}
	}
	
	protected void populatePredicateActorScopeRequest(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		if(arguments.getFilterFieldValue(ActorScopeRequestQuerier.PARAMETER_NAME_ACTORS_IDENTIFIERS) != null) {
			predicate.add(String.format("t.actor.identifier IN :%s", ActorScopeRequestQuerier.PARAMETER_NAME_ACTORS_IDENTIFIERS));
			filter.addFieldEquals(ActorScopeRequestQuerier.PARAMETER_NAME_ACTORS_IDENTIFIERS, arguments);
		}
		if(arguments.getFilterFieldValue(ActorScopeRequestQuerier.PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS) != null) {
			predicate.add(String.format("t.scope.type.identifier IN :%s", ActorScopeRequestQuerier.PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS));
			filter.addFieldEquals(ActorScopeRequestQuerier.PARAMETER_NAME_SCOPE_TYPES_IDENTIFIERS, arguments);
		}
		if(arguments.getFilterFieldValue(ActorScopeRequestQuerier.PARAMETER_NAME_SCOPES_IDENTIFIERS) != null) {
			predicate.add(String.format("t.scope.identifier IN :%s", ActorScopeRequestQuerier.PARAMETER_NAME_SCOPES_IDENTIFIERS));
			filter.addFieldEquals(ActorScopeRequestQuerier.PARAMETER_NAME_SCOPES_IDENTIFIERS, arguments);
		}
		Boolean processed = arguments.getFilterFieldValueAsBoolean(null,ActorScopeRequestQuerier.PARAMETER_NAME_PROCESSED);
		if(processed != null) {
			if(processed) {
				Boolean granted = arguments.getFilterFieldValueAsBoolean(null,ActorScopeRequestQuerier.PARAMETER_NAME_GRANTED);
				if(granted == null) {
					predicate.add("t.granted IS NOT NULL");
				}else {
					predicate.add(String.format("t.granted = :%s", ActorScopeRequestQuerier.PARAMETER_NAME_GRANTED));
					filter.addFieldEquals(ActorScopeRequestQuerier.PARAMETER_NAME_GRANTED, arguments);
				}				
			}else {
				predicate.add("t.granted IS NULL");
			}		
		}
	}
	
	protected void populatePredicateAssignments(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		//imputation filter
		//	by identifier
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTION_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_REGION_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_DEPARTMENT_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_SUB_PREFECTURE_IDENTIFIER})
			if(Boolean.TRUE.equals(isQueryExecutorArgumentsFilterHasFieldsWithPaths(arguments,parameterName))) {
				predicate.add(String.format("i.%1$s = :%1$s",parameterName));
				filter.addFieldEquals(parameterName, arguments);
			}
		//	by identifiers			
		if(Boolean.TRUE.equals(isQueryExecutorArgumentsFilterHasFieldsWithPaths(arguments,AssignmentsQuerier.PARAMETER_NAME_ACTIVITIES_IDENTIFIERS))) {
			predicate.add(String.format("i.activityIdentifier IN :%1$s",AssignmentsQuerier.PARAMETER_NAME_ACTIVITIES_IDENTIFIERS));
			filter.addField(AssignmentsQuerier.PARAMETER_NAME_ACTIVITIES_IDENTIFIERS, arguments.getFilterFieldValue(AssignmentsQuerier.PARAMETER_NAME_ACTIVITIES_IDENTIFIERS));
		}
		
		//	by code
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION_CODE,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE,AssignmentsQuerier.PARAMETER_NAME_ACTION_CODE
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CODE,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE_CODE,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY_CODE})
			if(Boolean.TRUE.equals(isQueryExecutorArgumentsFilterHasFieldsWithPaths(arguments,parameterName))) {
				predicate.add(String.format("i.%1$s = :%1$s",parameterName));
				filter.addFieldEquals(parameterName, arguments);
			}			
		//	by (code or name) like
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT,AssignmentsQuerier.PARAMETER_NAME_ACTION
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY})
			if(Boolean.TRUE.equals(isQueryExecutorArgumentsFilterHasFieldsWithPaths(arguments,parameterName))) {
				//TODO LIKE should be generated by a good algorithm
				predicate.add(String.format("i.%1$sCodeName LIKE :%1$s",parameterName));
				filter.addField(parameterName, "%"+arguments.getFilterFieldValue(parameterName)+"%");
				//destinationFilter.addFieldContainsStringOrWords(parameterName, 6, queryExecutorArguments);
				
				//destinationFilter.addFieldEquals(parameterName, queryExecutorArguments);
			}
			
		//scope function filter
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER_IDENTIFIER})
			if(Boolean.TRUE.equals(isQueryExecutorArgumentsFilterHasFieldsWithPaths(arguments,parameterName))) {
				predicate.add(String.format("%1$s.identifier = :%1$sIdentifier",getScopeFunctionFieldNameFromIdentifierParameterName(parameterName)));
				filter.addFieldEquals(parameterName, arguments);
			}
		
		if(Boolean.FALSE.equals(ValueConverter.getInstance().convertToBoolean(arguments.getFilterFieldValue(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE))))
			predicate.add(parenthesis(and(
					isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER),isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
					,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER),isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
			)));
		
		if(Boolean.FALSE.equals(ValueConverter.getInstance().convertToBoolean(arguments.getFilterFieldValue(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE))))
			predicate.add(parenthesis(or(
					isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER),isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
					,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER),isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
			)));			
	}
	
	/**/
	
	@Override
	protected void setOrder(QueryExecutorArguments arguments, Arguments builderArguments) {
		super.setOrder(arguments, builderArguments);
		if(arguments.getQuery().getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)) {
			builderArguments.getOrder(Boolean.TRUE).addFromTupleAscending("i", ExecutionImputation.FIELD_SECTION_CODE,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE
					,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE,ExecutionImputation.FIELD_ACTIVITY_CODE
					,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE);
		}else if(arguments.getQuery().getIdentifier().equals(ActorQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)) {
			builderArguments.getOrder(Boolean.TRUE).addFromTupleAscending("t.identity",Identity.FIELD_FIRST_NAME,Identity.FIELD_LAST_NAMES);
		}else if(arguments.getQuery().getIdentifier().equals(AdministrativeUnitQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)) {
			builderArguments.getOrder(Boolean.TRUE).addFromTupleAscending("t",AdministrativeUnit.FIELD_CODE);
		}
	}
	
	@Override
	protected Map<String, SortOrder> getDefaultSortOrders(QueryExecutorArguments arguments) {
		if(arguments.getQuery().getIdentifier().equals(ScopeTypeQuerier.QUERY_IDENTIFIER_READ_DYNAMIC))
			return Map.of(ScopeType.FIELD_ORDER_NUMBER,SortOrder.ASCENDING);
		return super.getDefaultSortOrders(arguments);
	}
	
	private static String getScopeFunctionFieldNameFromIdentifierParameterName(String fieldName) {
		if(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IDENTIFIER.equals(fieldName))
			return Assignments.FIELD_CREDIT_MANAGER_HOLDER;
		if(AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER.equals(fieldName))
			return Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER;
		if(AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER.equals(fieldName))
			return Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER;
		if(AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER_IDENTIFIER.equals(fieldName))
			return Assignments.FIELD_ACCOUNTING_HOLDER;
		throw new RuntimeException(String.format("Scope function field of parameter named %s not found", fieldName));
	}
	
}