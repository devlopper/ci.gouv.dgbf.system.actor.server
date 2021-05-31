package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;

public class AssignmentsStringsCodesOnlyWithoutScopeFunctionsReader extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Assignments> {

	@Override
	protected String getQueryValue() {
		QueryStringBuilder.Arguments arguments = new QueryStringBuilder.Arguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Assignments.FIELD_IDENTIFIER);
		arguments.getProjection(Boolean.TRUE).addFromTuple(Assignments.FIELD_EXECUTION_IMPUTATION,ExecutionImputation.FIELD_SECTION_CODE
				,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE
				,ExecutionImputation.FIELD_ACTIVITY_CODE,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE
				,ExecutionImputation.FIELD_ACTIVITY_CATEGORY_CODE);
		arguments.getTuple(Boolean.TRUE).add("Assignments t").leftJoinTuple("ExecutionImputation","t");
		arguments.getPredicate(Boolean.TRUE).add("t.identifier IN :"+Querier.PARAMETER_NAME_IDENTIFIERS);
		return QueryStringBuilder.getInstance().build(arguments);
	}
	
	@Override
	protected void __set__(Assignments assignments, Object[] array) {
		int i = 1;
		assignments.setSectionAsString((String) array[i++]);
		assignments.setAdministrativeUnitAsString((String) array[i++]);
		assignments.setBudgetSpecializationUnitAsString((String) array[i++]);
		assignments.setActionAsString((String) array[i++]);
		assignments.setActivityAsString((String) array[i++]);
		assignments.setEconomicNatureAsString((String) array[i++]);
		assignments.setExpenditureNatureAsString((String) array[i++]);
		assignments.setActivityCategoryAsString((String) array[i++]);
	}
	
	@Override
	protected Class<Assignments> getEntityClass() {
		return Assignments.class;
	}
}