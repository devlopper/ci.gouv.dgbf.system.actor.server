package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.isNotNull;
import static org.cyk.utility.persistence.query.Language.Where.isNull;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.__kernel__.value.ValueConverter;
import org.cyk.utility.persistence.query.Filter;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.FromStringBuilder;
import org.cyk.utility.persistence.server.query.string.JoinStringBuilder;
import org.cyk.utility.persistence.server.query.string.OrderStringBuilder;
import org.cyk.utility.persistence.server.query.string.SelectStringBuilder;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilder.Predicate;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

public interface AssignmentsQueryStringReadWhereFilterBuilder {

	String build(Arguments arguments);	
	String build(QueryExecutorArguments arguments);
	
	public static abstract class AbstractImpl extends AbstractObject implements AssignmentsQueryStringReadWhereFilterBuilder,Serializable {
		
		@Override
		public String build(Arguments arguments) {
			ThrowableHelper.throwIllegalArgumentExceptionIfNull("assignments query string builder arguments", arguments);
			//ThrowableHelper.throwIllegalArgumentExceptionIfEmpty("assignments query string projections", arguments.getProjections());
			SelectStringBuilder.Projection projection = new SelectStringBuilder.Projection();
			projection.addFromTuple("t", Assignments.FIELD_IDENTIFIER);
			if(CollectionHelper.isNotEmpty(arguments.imputationFieldsNames))
				projection.addFromTuple("i", arguments.imputationFieldsNames);
			String select = SelectStringBuilder.getInstance().build(projection);
			
			Collection<String> froms = new ArrayList<>();
			froms.add("Assignments t");
			if(CollectionHelper.isNotEmpty(arguments.imputationFieldsNames))
				froms.add("LEFT JOIN ExecutionImputation i ON i = t."+Assignments.FIELD_EXECUTION_IMPUTATION);
			if(CollectionHelper.isNotEmpty(arguments.joinableHoldersFieldsNames))
				for(String fieldName : arguments.joinableHoldersFieldsNames) {
					froms.add(getLeftJoinScopeFunction(fieldName));
				}
			String from = from(froms);
			
			Collection<String> predicates = new ArrayList<>();
			if(CollectionHelper.isNotEmpty(arguments.imputationFieldsNames))
				for(String fieldName : arguments.imputationFieldsNames)
					predicates.add(parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", fieldName)));
			if(CollectionHelper.isNotEmpty(arguments.joinableHoldersFieldsNames))
				for(String fieldName : arguments.joinableHoldersFieldsNames)
					predicates.add(parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", fieldName,Assignments.getColumnNameFromFieldName(fieldName))));
			if(Boolean.TRUE.equals(arguments.allHoldersDefined))
				predicates.add(parenthesis(and(
						isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
						,isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
						,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
						,isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
				)));
			if(Boolean.TRUE.equals(arguments.someHoldersNotDefined))
				predicates.add(parenthesis(or(
						isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
						,isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
						,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
						,isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)							
					)));
			
			String where = where(and(predicates));
			
			OrderStringBuilder.Order order = new OrderStringBuilder.Order();
			if(CollectionHelper.isNotEmpty(arguments.imputationFieldsNames))
				order.addFromTuple("i", arguments.imputationFieldsNames, SortOrder.ASCENDING);
			if(CollectionHelper.isNotEmpty(arguments.joinableHoldersFieldsNames))
				for(String fieldName : arguments.joinableHoldersFieldsNames)
					order.addFromTuple(Assignments.getColumnNameFromFieldName(fieldName), SortOrder.ASCENDING);
			
			return jpql(select,from,where,OrderStringBuilder.getInstance().build(order));
		}
		
		@Override
		public String build(QueryExecutorArguments arguments) {
			return null;
		}
	}
	
	static String getRead() {
		return getRead(null);
	}
	
	static String getRead(QueryExecutorArguments arguments) {
		return  jpql(getReadWhereFilterUsingIdentifiersOnlySelect(),getReadWhereFilterFrom(),getReadWhereFilterWherePredicate(arguments),getOrder());
	}
	
	static String getCount(QueryExecutorArguments arguments) {
		return  jpql("SELECT COUNT(t.identifier)",getReadWhereFilterFrom(),getReadWhereFilterWherePredicate(arguments));
	}
	
	static String getReadWhereFilterUsingIdentifiersOnlySelect() {
		SelectStringBuilder.Projection projection = new SelectStringBuilder.Projection();
		projection
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
		return SelectStringBuilder.getInstance().build(projection);
	}
	
	static String getReadWhereFilterFrom() {
		FromStringBuilder.Tuple tuple = new FromStringBuilder.Tuple("Assignments","t");
		tuple.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments().setMasterVariableName("t").setTupleName("ExecutionImputation")
				.setVariableName("i")));
		
		for(String fieldName : new String[] {Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT
				,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT
				,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT
				,Assignments.FIELD_ACCOUNTING_HOLDER,Assignments.FIELD_ACCOUNTING_ASSISTANT}) {
			tuple.addJoins(JoinStringBuilder.getInstance().build(new JoinStringBuilder.Arguments()
					.setMasterVariableName("t").setMasterFieldName(fieldName).setTupleName("ScopeFunction")));
		}
		return FromStringBuilder.getInstance().build(tuple);
	}
	
	static Filter buildFilterFrom(QueryExecutorArguments queryExecutorArguments) {
		if(queryExecutorArguments == null || queryExecutorArguments.getFilter() == null || CollectionHelper.isEmpty(queryExecutorArguments.getFilter().getFields()))
			return null;
		Filter filter = new Filter();
		
		filter.addFieldsNullable(queryExecutorArguments,AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED,AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED);
		
		filter.addFieldsEquals(queryExecutorArguments
				
				,AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER_IDENTIFIER
				
				,AssignmentsQuerier.PARAMETER_NAME_SECTION_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTION_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY_IDENTIFIER);
		
		return filter;
	}
	
	static String getReadWhereFilterWherePredicate(QueryExecutorArguments queryExecutorArguments) {
		if(queryExecutorArguments == null || queryExecutorArguments.getFilter() == null || CollectionHelper.isEmpty(queryExecutorArguments.getFilter().getFields()))
			return null;
		Filter sourceFilter = queryExecutorArguments.getFilter(),destinationFilter = new Filter();
		Collection<String> notNullablesPredicates = new ArrayList<>();
		//imputation filter
		//	by identifier
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTION_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY_IDENTIFIER})
			if(Boolean.TRUE.equals(sourceFilter.hasFieldWithPath(parameterName))) {
				notNullablesPredicates.add(String.format("i.%1$s = :%1$s",parameterName));
				destinationFilter.addFieldEquals(parameterName, queryExecutorArguments);
			}
		//	by code
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION_CODE,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT_CODE
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT_CODE,AssignmentsQuerier.PARAMETER_NAME_ACTION_CODE
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CODE,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE_CODE
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE_CODE,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY_CODE})
			if(Boolean.TRUE.equals(sourceFilter.hasFieldWithPath(parameterName))) {
				notNullablesPredicates.add(String.format("i.%1$s = :%1$s",parameterName));
				destinationFilter.addFieldEquals(parameterName, queryExecutorArguments);
			}
		
		//	by (code or name) like
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_SECTION,AssignmentsQuerier.PARAMETER_NAME_ADMINISTRATIVE_UNIT
				,AssignmentsQuerier.PARAMETER_NAME_BUDGET_SPECIALIZATION_UNIT,AssignmentsQuerier.PARAMETER_NAME_ACTION
				,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY,AssignmentsQuerier.PARAMETER_NAME_ECONOMIC_NATURE
				,AssignmentsQuerier.PARAMETER_NAME_EXPENDITURE_NATURE,AssignmentsQuerier.PARAMETER_NAME_ACTIVITY_CATEGORY})
			if(Boolean.TRUE.equals(sourceFilter.hasFieldWithPath(parameterName))) {
				//TODO LIKE should be generated by a good algorithm
				notNullablesPredicates.add(String.format("i.%1$sCodeName LIKE :%1$s",parameterName));
				destinationFilter.addField(parameterName, "%"+sourceFilter.getFieldValue(parameterName)+"%");
				//destinationFilter.addFieldContainsStringOrWords(parameterName, 6, queryExecutorArguments);
				
				//destinationFilter.addFieldEquals(parameterName, queryExecutorArguments);
			}
			
		//scope function filter
		for(String parameterName : new String[] {AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER_IDENTIFIER,AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER_IDENTIFIER
				,AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER_IDENTIFIER})
			if(Boolean.TRUE.equals(sourceFilter.hasFieldWithPath(parameterName))) {
				notNullablesPredicates.add(String.format("%1$s.identifier = :%1$sIdentifier",getScopeFunctionFieldNameFromIdentifierParameterName(parameterName)));
				destinationFilter.addFieldEquals(parameterName, queryExecutorArguments);
			}
		
		if(Boolean.FALSE.equals(ValueConverter.getInstance().convertToBoolean(sourceFilter.getFieldValue(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE))))
			notNullablesPredicates.add(parenthesis(and(
					isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER),isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
					,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER),isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
			)));
		
		if(Boolean.FALSE.equals(ValueConverter.getInstance().convertToBoolean(sourceFilter.getFieldValue(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE))))
			notNullablesPredicates.add(parenthesis(or(
					isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER),isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
					,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER),isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
			)));
		
		queryExecutorArguments.setRuntimeFilter(destinationFilter);
		
		if(CollectionHelper.isEmpty(notNullablesPredicates))
			return null;
		Predicate predicate = new Predicate();
		predicate.ands(notNullablesPredicates);
		return WhereStringBuilder.getInstance().build(predicate);		
	}
	
	public static String getScopeFunctionFieldNameFromIdentifierParameterName(String fieldName) {
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
	/*
	static String getReadWhereFilterWhere(Filter filter) {
		return where(getReadWhereFilterWherePredicate(filter));
	}
	*/
	static String getReadWhereFilterUsingIdentifiersOnlyFromWhere() {
		return jpql(getReadWhereFilterFrom(),getReadWhereFilterWherePredicate(null));
	}
	
	static String getReadWhereFilterScopeFunctionPredicate(String fieldName) {
		return String.format("(:%1$sIdentifierNullable = true OR %1$s.identifier = :%1$sIdentifier)", fieldName);
	}
	
	static String getLeftJoinScopeFunction(String fieldName) {
		return String.format("LEFT JOIN ScopeFunction %1$s ON %1$s = t.%1$s", fieldName);
	}
	
	static String getOrder() {
		return order(
				asc("i",ExecutionImputation.FIELD_SECTION_CODE)
				,asc("i",ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE)
				,asc("i",ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE)
				,asc("i",ExecutionImputation.FIELD_ACTION_CODE)
				,asc("i",ExecutionImputation.FIELD_ACTIVITY_CODE)
				,asc("i",ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE)
			);
	}
	
	static String[] getTupleFieldsNamesIndexesFromFieldsNames() {
		return new String[] {Assignments.FIELD_IDENTIFIER,Assignments.FIELD_SECTION_AS_STRING,Assignments.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
				,Assignments.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING,Assignments.FIELD_ACTION_AS_STRING,Assignments.FIELD_ACTIVITY_AS_STRING
				,Assignments.FIELD_ECONOMIC_NATURE_AS_STRING,Assignments.FIELD_EXPENDITURE_NATURE_AS_STRING,Assignments.FIELD_ACTIVITY_CATEGORY_AS_STRING
				,Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING
				,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING
				,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING
				,Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING,Assignments.FIELD_ACCOUNTING_ASSISTANT_AS_STRING};
	}
	
	/**/
	
	@Getter @Setter @Accessors(chain=true)
	public static class Arguments implements Serializable {
		private Collection<String> imputationFieldsNames;
		private Collection<String> joinableHoldersFieldsNames;
		private Boolean allHoldersDefined,someHoldersNotDefined;
	}
	
	/**/
	
	static AssignmentsQueryStringReadWhereFilterBuilder getInstance() {
		return Helper.getInstance(AssignmentsQueryStringReadWhereFilterBuilder.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
}