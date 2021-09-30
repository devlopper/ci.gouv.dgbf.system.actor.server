package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import javax.persistence.EntityManager;

import org.cyk.utility.persistence.server.procedure.ProcedureExecutor;
import org.cyk.utility.persistence.server.procedure.ProcedureExecutorArguments;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;

public class PrivilegeQuerierImpl extends PrivilegeQuerier.AbstractImpl {

	@Override
	public void refresh(EntityManager entityManager) {
		ProcedureExecutorArguments arguments = new ProcedureExecutorArguments();
		arguments.setName(Privilege.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_REFRESH);
		arguments.setEntityManager(entityManager);
		ProcedureExecutor.getInstance().execute(arguments);
	}
}