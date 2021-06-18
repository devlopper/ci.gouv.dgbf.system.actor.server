package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.audit.AuditReader;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class AssignmentsAuditsReader extends AbstractAssignmentsReaderImpl {

	@Override
	protected String getQueryValue() {
		// Audit API will be used
		return null;
	}
	
	@Override
	public void readThenSet(Collection<Assignments> entities, Map<String, Object> parameters) {
		if(CollectionHelper.isEmpty(entities))
			return;
		for(Assignments index : entities) {
			index.set__auditRecords__(AuditReader.getInstance().readByIdentifier(Assignments.class, index.getIdentifier()));
			if(CollectionHelper.isEmpty(index.get__auditRecords__()))
				continue;
			for(Assignments audit : index.get__auditRecords__()) {
				audit.setExecutionImputation(null);
				setScopeFunctions(audit, Assignments.FIELD_CREDIT_MANAGER_HOLDER);
				setScopeFunctions(audit, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER);
				setScopeFunctions(audit, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER);
				setScopeFunctions(audit, Assignments.FIELD_ACCOUNTING_HOLDER);
			}
		}
	}
	
	private static void setScopeFunctions(Assignments assignments,String fieldName) {
		ScopeFunction holder = (ScopeFunction) FieldHelper.read(assignments, fieldName);
		if(holder != null) {
			FieldHelper.write(assignments, fieldName+"AsString", holder.getCode());
			FieldHelper.write(assignments, fieldName, null);
		}
	}
}