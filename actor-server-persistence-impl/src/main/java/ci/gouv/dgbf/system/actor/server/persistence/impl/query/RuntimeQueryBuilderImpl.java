package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.RuntimeQueryBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ProfileQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class RuntimeQueryBuilderImpl extends RuntimeQueryBuilder.AbstractImpl implements Serializable {
	/*
	@Override
	protected Query instantiateQuery(QueryExecutorArguments queryExecutorArguments) {
		Query query = super.instantiateQuery(queryExecutorArguments);
		if(query.getIdentifier().equals(AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY))
			query.setTupleFieldsNamesIndexesFromFieldsNames(AssignmentsQueryStringReadWhereFilterBuilder.getTupleFieldsNamesIndexesFromFieldsNames());
		return query;
	}
	*/
	@Override
	protected void setTupleFieldsNamesIndexesFromFieldsNames(QueryExecutorArguments arguments, Query query) {
		if(arguments.getQuery().isIdentifierEqualsDynamic(Assignments.class,QueryName.READ_DYNAMIC,QueryName.READ_DYNAMIC_ONE)) {
			if(Boolean.TRUE.equals(arguments.isFlagged(AssignmentsQuerier.FLAG_APPLY_MODEL))) {
				
			}else {
				/*query.setIntermediateResultClass(Object[].class);
				query.setTupleFieldsNamesIndexesFromFieldsNames(new String[] {
						Assignments.FIELD_IDENTIFIER,Assignments.FIELD_SECTION_AS_STRING,Assignments.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
						,Assignments.FIELD_BUDGET_SPECIALIZATION_UNIT_AS_STRING,Assignments.FIELD_ACTION_AS_STRING,Assignments.FIELD_ACTIVITY_AS_STRING
						,Assignments.FIELD_ECONOMIC_NATURE_AS_STRING,Assignments.FIELD_EXPENDITURE_NATURE_AS_STRING,Assignments.FIELD_ACTIVITY_CATEGORY_AS_STRING
						,Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING,Assignments.FIELD_CREDIT_MANAGER_ASSISTANT_AS_STRING
						,Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING,Assignments.FIELD_AUTHORIZING_OFFICER_ASSISTANT_AS_STRING
						,Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING,Assignments.FIELD_FINANCIAL_CONTROLLER_ASSISTANT_AS_STRING
						,Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING,Assignments.FIELD_ACCOUNTING_ASSISTANT_AS_STRING});
				*/
				super.setTupleFieldsNamesIndexesFromFieldsNames(arguments, query);
			}			
		}else
			super.setTupleFieldsNamesIndexesFromFieldsNames(arguments, query);
	}
	
	/*
	@Override
	protected Boolean isBuildable(QueryExecutorArguments arguments) {
		if(arguments != null && arguments.getQuery() != null && AssignmentsQuerier.QUERY_IDENTIFIER_READ_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return Boolean.TRUE;
		if(arguments != null && arguments.getQuery() != null && AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_WHERE_FILTER_USING_IDENTIFIERS_ONLY.equals(arguments.getQuery().getIdentifier()))
			return Boolean.TRUE;
		return super.isBuildable(arguments);
	}
	*/
	@Override
	protected void processQueryExecutorArguments(QueryExecutorArguments arguments) {
		super.processQueryExecutorArguments(arguments);
		if(arguments.getQuery().isIdentifierEqualsDynamic(Profile.class)) {
			if(arguments.isFlagged(ProfileQuerier.FLAG_PREPARE_EDIT)) {
				arguments.addProjectionsFromStrings(Profile.FIELD_IDENTIFIER,Profile.FIELD_CODE,Profile.FIELD_NAME,Profile.FIELD_TYPE,Profile.FIELD_ORDER_NUMBER
						,Profile.FIELD_REQUESTABLE);
			}
		}else if(arguments.getQuery().isIdentifierEqualsDynamic(Locality.class)) {
			if(arguments.getFilter() != null && arguments.getFilter().hasFieldWithPath(LocalityQuerier.PARAMETER_NAME_TYPE) 
					&& arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).getValue() instanceof String) {
				String type = (String) arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).getValue();
				if(StringHelper.isNotBlank(type))
					arguments.getFilter().getField(LocalityQuerier.PARAMETER_NAME_TYPE).setValue(Locality.Type.valueOf(type));
			}
		}
	}
}