package ci.gouv.dgbf.system.actor.server.persistence.impl;

import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.hibernate.AbstractAuditIdentifiedByStringReader;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder.Arguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;

public class AssignmentsAuditReader extends AbstractAuditIdentifiedByStringReader<AssignmentsAudit> {

	@Override
	protected void setProjections(Arguments arguments) {
		super.setProjections(arguments);
		for(String fieldName : Assignments.FIELDS_SCOPES_FUNCTIONS_HOLDERS)
			arguments.getProjection().add(fieldName+".code AS "+fieldName+"Alias");
		arguments.getProjection().addFromTuple("i",ExecutionImputation.FIELD_SECTION_CODE,ExecutionImputation.FIELD_ADMINISTRATIVE_UNIT_CODE
				,ExecutionImputation.FIELD_BUDGET_SPECIALIZATION_UNIT_CODE,ExecutionImputation.FIELD_ACTION_CODE,ExecutionImputation.FIELD_ACTIVITY_CODE
				,ExecutionImputation.FIELD_EXPENDITURE_NATURE_CODE,ExecutionImputation.FIELD_ECONOMIC_NATURE_CODE);		
	}
	
	@Override
	protected void setTuple(Arguments arguments) {
		super.setTuple(arguments);
		for(String fieldName : Assignments.FIELDS_SCOPES_FUNCTIONS_HOLDERS)
			arguments.getTuple().addJoins(String.format("LEFT JOIN ScopeFunction %1$s ON %1$s = t.%1$s",fieldName));
		arguments.getTuple().addJoins("LEFT JOIN Assignments a ON a.identifier = t.identifier","LEFT JOIN ExecutionImputation i ON i = a.executionImputation");
	}
	
	@Override
	protected void __set__(AssignmentsAudit audit, Object[] array, Integer index) {
		super.__set__(audit, array, index);
		for(String fieldName : Assignments.FIELDS_SCOPES_FUNCTIONS_HOLDERS)
			FieldHelper.write(audit, fieldName+"AsString",array[index++]);
		audit.setSectionAsString(getAsString(array, index++));
		audit.setAdministrativeUnitAsString(getAsString(array, index++));
		audit.setBudgetSpecializationUnitAsString(getAsString(array, index++));
		audit.setActionAsString(getAsString(array, index++));
		audit.setActivityAsString(getAsString(array, index++));
		audit.setExpenditureNatureAsString(getAsString(array, index++));
		audit.setEconomicNatureAsString(getAsString(array, index++));
	}
}