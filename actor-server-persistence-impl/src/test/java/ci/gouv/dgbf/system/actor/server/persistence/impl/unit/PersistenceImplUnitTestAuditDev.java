package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class PersistenceImplUnitTestAuditDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunction(){
		String identifier = "A1000000";
		ScopeFunction scopeFunction = EntityReader.getInstance().readOneDynamically(ScopeFunction.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(scopeFunction).isNotNull();
		assertThat(scopeFunction.get__auditRecords__()).hasSize(2);
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getCode()).isEqualTo("A1000000");
		//assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
	}
	
	@Test
	public void assignments(){
		String identifier = "262f606d-b7a5-4733-bfa9-dad6d50f3a77";
		Assignments assignments = EntityReader.getInstance().readOneDynamically(Assignments.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(assignments).isNotNull();
		assertThat(assignments.get__auditRecords__()).hasSize(1);
		//assertThat(CollectionHelper.getElementAt(assignments.get__auditRecords__(), 0).getCode()).isEqualTo("A1000000");
		//assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
	}
	
}