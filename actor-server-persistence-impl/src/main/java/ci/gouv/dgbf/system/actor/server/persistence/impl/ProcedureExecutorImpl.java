package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;

import javax.persistence.StoredProcedureQuery;

import org.cyk.utility.__kernel__.persistence.procedure.ProcedureExecutor;
import org.hibernate.procedure.ProcedureCall;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class ProcedureExecutorImpl extends ProcedureExecutor.AbstractImpl implements Serializable {

	@Override
	protected void releaseConnection(StoredProcedureQuery storedProcedureQuery, String name) {
		((ProcedureCall)storedProcedureQuery).getOutputs().release();
	}
}