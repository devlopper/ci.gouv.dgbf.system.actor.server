package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import org.cyk.utility.persistence.server.hibernate.AbstractAuditsRecordsNativeReader;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class AssignmentsAuditsRecordsReader extends AbstractAuditsRecordsNativeReader<Assignments> implements Serializable {
	
	@Override
	protected Class<Assignments> getEntityClass() {
		return Assignments.class;
	}

	@Override
	protected void __set__(Assignments assignments, Object[] array, Integer i) {
		assignments.setCreditManagerHolderAsString((String) array[i++]);
		assignments.setAuthorizingOfficerHolderAsString((String) array[i++]);
		assignments.setFinancialControllerHolderAsString((String) array[i++]);
		assignments.setAccountingHolderAsString((String) array[i++]);
	}

	@Override
	protected String getAuditTableName() {
		return "AFFECTATIONS_AUD";
	}

	@Override
	protected Collection<String> getProjections() {
		return List.of(Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER
				,Assignments.COLUMN_ACCOUNTING_HOLDER);
	}
	
	@Override
	protected String getAuditFunctionalityColumnName() {
		return "AUDIT_FONCTIONALITE";
	}
}