package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.parenthesis;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Where.and;
import static org.cyk.utility.persistence.query.Language.Where.isNotNull;
import static org.cyk.utility.persistence.query.Language.Where.isNull;
import static org.cyk.utility.persistence.query.Language.Where.isNullable;
import static org.cyk.utility.persistence.query.Language.Where.or;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.throwable.ThrowableHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.query.string.OrderStringBuilder;
import org.cyk.utility.persistence.server.query.string.SelectStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

public interface AssignmentsQueryStringBuilderOLD {

	String build(Arguments arguments);	
	String build(QueryExecutorArguments arguments);
	
	public static abstract class AbstractImpl extends AbstractObject implements AssignmentsQueryStringBuilderOLD,Serializable {
		
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
					froms.add(getLeftJoinScopeFunction(fieldName, Assignments.getColumnNameFromFieldName(fieldName)));
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
	
	static String getReadWhereFilterUsingIdentifiersOnlyFrom() {
		return getReadWhereFilterUsingIdentifiersOnlyFrom(Boolean.TRUE,List.of(Assignments.FIELD_CREDIT_MANAGER_HOLDER,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER
				,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER,Assignments.FIELD_ACCOUNTING_HOLDER));
	}

	static String getReadWhereFilterUsingIdentifiersOnlyFrom(Boolean imputation,Collection<String> holdersFieldsNames) {
		Collection<String> froms = new ArrayList<>();
		froms.add("Assignments t");
		if(Boolean.TRUE.equals(imputation))
			froms.add("LEFT JOIN ExecutionImputation i ON i = t."+Assignments.FIELD_EXECUTION_IMPUTATION);
		if(CollectionHelper.isNotEmpty(holdersFieldsNames))
			for(String fieldName : holdersFieldsNames) {
				froms.add(getLeftJoinScopeFunction(fieldName, Assignments.getColumnNameFromFieldName(fieldName)));
			}
		return from(froms);
	}
	
	static String getReadWhereFilterUsingIdentifiersOnlyWherePredicate() {
		return getReadWhereFilterUsingIdentifiersOnlyWherePredicate(List.of(ExecutionImputation.FIELD_SECTION,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT
				,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT,ExecutionImputation.FIELD_ACTION,ExecutionImputation.FIELD_ACTIVITY
				,ExecutionImputation.FIELD_ECONOMIC_NATURE,ExecutionImputation.FIELD_EXPENDITURE_NATURE,ExecutionImputation.FIELD_ACTIVITY_CATEGORY)
				,List.of(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER,AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER
						,AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER,AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER));
	}
	
	static String getReadWhereFilterUsingIdentifiersOnlyWherePredicate(Collection<String> imputationFieldsNames,Collection<String> holdersParametersNames) {
		Collection<String> predicates = new ArrayList<>();
		if(CollectionHelper.isNotEmpty(imputationFieldsNames))
			for(String fieldName : imputationFieldsNames)
				predicates.add(parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", fieldName)));
		if(CollectionHelper.isNotEmpty(holdersParametersNames))
			for(String parameterName : holdersParametersNames) {
				String variableName = null;
				if(AssignmentsQuerier.PARAMETER_NAME_CREDIT_MANAGER_HOLDER.equals(parameterName))
					variableName = Assignments.COLUMN_CREDIT_MANAGER_HOLDER;
				else if(AssignmentsQuerier.PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER.equals(parameterName))
					variableName = Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER;
				else if(AssignmentsQuerier.PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER.equals(parameterName))
					variableName = Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER;
				else if(AssignmentsQuerier.PARAMETER_NAME_ACCOUNTING_HOLDER.equals(parameterName))
					variableName = Assignments.COLUMN_ACCOUNTING_HOLDER;
				if(StringHelper.isBlank(variableName))
					continue;
				predicates.add(parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", parameterName,variableName)));
			}
		
		predicates.add(parenthesis(or(isNullable(AssignmentsQuerier.PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE)
				, parenthesis(and(isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
						,isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
						,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
						,isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
						)))
			));
		
		predicates.add(parenthesis(or(isNullable(AssignmentsQuerier.PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE)
				, or(isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
						,isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
						,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
						,isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
						))
			));
		
		return and(predicates);
		/*
		return and(
			parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_SECTION))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_ACTION))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_ACTIVITY))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_ECONOMIC_NATURE))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_EXPENDITURE_NATURE))
			,parenthesis(String.format(":%1$sNullable = true OR i.%1$sIdentifier = :%1$s", ExecutionImputation.FIELD_ACTIVITY_CATEGORY))
			
			,parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", PARAMETER_NAME_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_HOLDER))
			,parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", PARAMETER_NAME_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER))
			,parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", PARAMETER_NAME_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER))
			,parenthesis(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", PARAMETER_NAME_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_HOLDER))
			
			
			,parenthesis(or(isNullable(PARAMETER_NAME_ALL_HOLDERS_DEFINED_NULLABLE)
					, parenthesis(and(isNotNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
							,isNotNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
							,isNotNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
							,isNotNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
							)))
				)
			
			,parenthesis(or(isNullable(PARAMETER_NAME_SOME_HOLDERS_NOT_DEFINED_NULLABLE)
					, or(isNull("t", Assignments.FIELD_CREDIT_MANAGER_HOLDER)
							,isNull("t", Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER)
							,isNull("t", Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER)
							,isNull("t", Assignments.FIELD_ACCOUNTING_HOLDER)
							))
				)
		);*/
	}
	
	static String getReadWhereFilterUsingIdentifiersOnlyFromWhere() {
		return jpql(getReadWhereFilterUsingIdentifiersOnlyFrom(),where(getReadWhereFilterUsingIdentifiersOnlyWherePredicate()));
	}
	
	static String getReadWhereFilterUsingIdentifiersOnlyScopeFunctionPredicate(String variableName,String parameterName) {
		return parenthesis(or(String.format(":%1$sNullable = true OR %2$s.identifier = :%1$s", parameterName,variableName)));
	}
	
	static String getLeftJoinScopeFunction(String holderFieldName,String holderVariableName) {
		return String.format("LEFT JOIN ScopeFunction %2$s ON %2$s = t.%1$s", holderFieldName,holderVariableName);
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
	
	/**/
	
	@Getter @Setter @Accessors(chain=true)
	public static class Arguments implements Serializable {
		private Collection<String> imputationFieldsNames;
		private Collection<String> joinableHoldersFieldsNames;
		private Boolean allHoldersDefined,someHoldersNotDefined;
	}
	
	/**/
	
	static AssignmentsQueryStringBuilderOLD getInstance() {
		return Helper.getInstance(AssignmentsQueryStringBuilderOLD.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
}