package ci.gouv.dgbf.system.actor.server.persistence.api;

import org.cyk.utility.__kernel__.persistence.procedure.ProcedureExecutor;
import org.cyk.utility.__kernel__.random.RandomHelper;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class PersistenceApiProcedureUnitTestDev extends AbstractPersistenceApiUnitTestValidate {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void assignments_clean(){
		String index = RandomHelper.getAlphabetic(1);
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_CLEAN
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,"u"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,"f"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,"a"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(System.currentTimeMillis())
			);
	}
	
	@Test
	public void assignments_import(){
		String index = RandomHelper.getAlphabetic(1);
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_IMPORT
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,"u"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,"f"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,"a"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(System.currentTimeMillis())
			);
	}
	
	@Test
	public void assignments_export(){
		String index = RandomHelper.getAlphabetic(1);
		ProcedureExecutor.getInstance().execute(Assignments.STORED_PROCEDURE_QUERY_PROCEDURE_NAME_EXPORT
				, Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTOR,"u"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_FUNCTIONALITY,"f"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_ACTION,"a"+index
				,Assignments.STORED_PROCEDURE_PARAMETER_NAME_AUDIT_DATE,new java.sql.Date(System.currentTimeMillis())
			);
	}
}