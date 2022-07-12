package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class AssignmentsStringsCodesOnlyReader extends AbstractAssignmentsReaderImpl implements Serializable {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Assignments.FIELD_IDENTIFIER);
		arguments.getProjection(Boolean.TRUE).addFromTuple(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_BUDGET_CATEGORY_CODE,ExecutionImputation.FIELD_SECTION_CODE
				,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE
				,ExecutionImputation.FIELD_ACTIVITY_CODE,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE
				,ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE);
		arguments.getTuple(Boolean.TRUE).add("Assignments t").leftJoinTuple("ExecutionImputation","t");
		for(String fieldName : Assignments.FIELDS_SCOPES_FUNCTIONS_HOLDERS) {
			arguments.getProjection().addFromTuple(fieldName,ScopeFunction.FIELD_CODE);
			arguments.getTuple().addJoins(String.format("LEFT JOIN ScopeFunction %1$s ON %1$s = t.%1$s", fieldName));
		}
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Assignments assignments, Object[] array) {
		int i = 1;
		assignments.setBudgetCategoryAsString((String) array[i++]);
		assignments.setSectionAsString((String) array[i++]);
		assignments.setAdministrativeUnitAsString((String) array[i++]);
		assignments.setBudgetSpecializationUnitAsString((String) array[i++]);
		assignments.setActionAsString((String) array[i++]);
		assignments.setActivityAsString((String) array[i++]);
		assignments.setEconomicNatureAsString((String) array[i++]);
		assignments.setExpenditureNatureAsString((String) array[i++]);
		assignments.setActivityCategoryAsString((String) array[i++]);
		assignments.setCreditManagerHolderAsString((String) array[i++]);
		assignments.setAuthorizingOfficerHolderAsString((String) array[i++]);
		assignments.setFinancialControllerHolderAsString((String) array[i++]);
		assignments.setAccountingHolderAsString((String) array[i++]);
	}
}