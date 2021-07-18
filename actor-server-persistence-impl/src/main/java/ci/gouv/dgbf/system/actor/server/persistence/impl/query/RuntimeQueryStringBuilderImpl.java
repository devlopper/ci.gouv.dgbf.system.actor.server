package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.isNotNull;
import static org.cyk.utility.persistence.query.Language.Where.isNull;
import static org.cyk.utility.persistence.query.Language.Where.or;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.ValueConverter;
import org.cyk.utility.__kernel__.value.ValueHelper;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.JoinStringBuilder;
import org.cyk.utility.persistence.server.query.string.LikeStringBuilder;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder.Predicate;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.ActorQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

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
		}		
	}
	
	@Override
	protected void populatePredicate(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		super.populatePredicate(arguments, builderArguments, predicate, filter);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Actor.class)) {
			addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, ActorQuerier.PARAMETER_NAME_CODE);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Locality.class)) {
			addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_TYPE);
			addEqualsIfFilterHasFieldWithPath(arguments, builderArguments, predicate, filter, LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"t.parent",Locality.FIELD_IDENTIFIER);
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Assignments.class)) {
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
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Scope.class))
			populatePredicateScope(arguments, builderArguments, predicate, filter);
	}
	
	protected void populatePredicateScope(QueryExecutorArguments arguments, Arguments builderArguments, Predicate predicate,Filter filter) {
		String scopeTypeCode = (String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_TYPE_CODE);
		String actorCode = (String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE);
		String actorCodeParameterName = null;
		if(StringHelper.isNotBlank(actorCode))
			actorCodeParameterName = ":"+ScopeQuerier.PARAMETER_NAME_ACTOR_CODE;
		if(StringHelper.isNotBlank(scopeTypeCode)) {
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
			predicate.add(ScopeQueryStringBuilder.Predicate.scopeVisible(scopeTypeCode, actorCodeParameterName
					,!Boolean.TRUE.equals(ValueHelper.defaultToIfNull(arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_VISIBLE), Boolean.TRUE))));
			if(StringHelper.isNotBlank(actorCode))
				filter.addField(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode);			
		}
	}
	
	/**/
	
	@Override
	protected void setOrder(QueryExecutorArguments arguments, Arguments builderArguments) {
		super.setOrder(arguments, builderArguments);
		if(arguments.getQuery().getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_DYNAMIC)) {
			builderArguments.getOrder(Boolean.TRUE).addFromTupleAscending("i", ExecutionImputation.FIELD_SECTION_CODE,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE
					,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE,ExecutionImputation.FIELD_ACTIVITY_CODE
					,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE);
		}
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