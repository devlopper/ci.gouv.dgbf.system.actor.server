package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;

import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.persistence.PersistenceHelper;
import org.cyk.utility.persistence.query.NativeQueryStringBuilder;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;

public class PersistenceHelperUnitTest extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void assignments_columnsNames(){
		Collection<String> names = PersistenceHelper.getColumnsNames(Assignments.class);
		assertThat(names).containsExactly(Assignments.COLUMN_IDENTIFIER,Assignments.COLUMN_EXECUTION_IMPUTATION
				,Assignments.COLUMN_CREDIT_MANAGER_HOLDER,Assignments.COLUMN_CREDIT_MANAGER_ASSISTANT
				,Assignments.COLUMN_AUTHORIZING_OFFICER_HOLDER,Assignments.COLUMN_AUTHORIZING_OFFICER_ASSISTANT
				,Assignments.COLUMN_FINANCIAL_CONTROLLER_HOLDER,Assignments.COLUMN_FINANCIAL_CONTROLLER_ASSISTANT
				,Assignments.COLUMN_ACCOUNTING_HOLDER,Assignments.COLUMN_ACCOUNTING_ASSISTANT);
	}
	
	@Test
	public void assignments_columnsValues(){
		assertThat(PersistenceHelper.getColumnsValues(new Assignments(),null)).containsExactly("NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL");
		
		assertThat(PersistenceHelper.getColumnsValues(new Assignments().setExecutionImputation(new ExecutionImputation().setIdentifier("i1")),null))
			.containsExactly("NULL","'i1'","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL");
	}
	
	@Test
	public void assignments_sql_insert(){
		assertThat(__inject__(NativeQueryStringBuilder.class).buildInsertMany(Assignments.class, List.of(new Assignments())))
			.isEqualTo("INSERT /*+ PARALLEL(AFFECTATIONS,4) */ ALL INTO AFFECTATIONS (IDENTIFIANT,IMPUTATION,GC,AGC,ORD,AORD,CF,ACF,CPT,ACPT) "
					+ "VALUES (NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL) SELECT * FROM dual");
		

	}
}