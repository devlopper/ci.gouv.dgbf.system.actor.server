package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.__kernel__.object.marker.AuditableWhoDoneWhatWhen;
import org.cyk.utility.persistence.query.EntityCounter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionAudit;

public class PersistenceImplUnitTestAuditDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunction_byInstance(){
		String identifier = "A1000000";
		ScopeFunction scopeFunction = EntityReader.getInstance().readOneDynamically(ScopeFunction.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(scopeFunction).isNotNull();
		assertThat(scopeFunction.get__auditRecords__()).hasSize(6);
		assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getCode()).isEqualTo("A1000000");
		//assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
	}
	
	@Test
	public void scopeFunction_byDates(){
		Collection<ScopeFunction> audits = EntityReader.getInstance().readMany(ScopeFunction.class, new QueryExecutorArguments()
				.setIsProcessableAsAuditByDates(Boolean.TRUE)
				.addProjectionsFromStrings(ScopeFunctionAudit.FIELD_IDENTIFIER,ScopeFunctionAudit.FIELD_CODE,ScopeFunctionAudit.FIELD_NAME
						,ScopeFunctionAudit.FIELD___AUDIT_REVISION__,ScopeFunction.FIELD___AUDIT_FUNCTIONALITY__
						,ScopeFunction.FIELD___AUDIT_WHAT__,ScopeFunction.FIELD___AUDIT_WHO__)
				.setQuery(new Query().setIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_AUDIT))
				.setSortOrders(Map.of(ScopeFunction.FIELD___AUDIT_WHEN__,SortOrder.DESCENDING))
				);
		
		assertThat(audits).isNotNull();
		assertThat(audits).hasSize(8);
		assertThat(CollectionHelper.getElementAt(audits, 0).getCode()).isEqualTo("A1000010");
		//assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
		
		assertThat(CollectionHelper.getElementAt(audits, 1).getCode()).isEqualTo("A1000000");
		
		assertThat(EntityCounter.getInstance().count(ScopeFunction.class, new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_COUNT_AUDIT)))).isEqualTo(8l);
	}
	
	@Test
	public void assignments(){
		String identifier = "262f606d-b7a5-4733-bfa9-dad6d50f3a77";
		Assignments assignments = EntityReader.getInstance().readOneDynamically(Assignments.class, new QueryExecutorArguments()
				.addFilterFieldsValues(Querier.PARAMETER_NAME_IDENTIFIER,identifier).addProcessableTransientFieldsNames(AuditableWhoDoneWhatWhen.FIELD___AUDIT_RECORDS__));
		assertThat(assignments).isNotNull();
		assertThat(assignments.get__auditRecords__()).hasSize(2);
		//assertThat(CollectionHelper.getElementAt(assignments.get__auditRecords__(), 0).getCode()).isEqualTo("A1000000");
		//assertThat(CollectionHelper.getElementAt(scopeFunction.get__auditRecords__(), 0).getName()).isEqualTo("mysf 01");
	}
	
	@Test
	public void assignments_byDates(){
		Collection<Assignments> audits = EntityReader.getInstance().readMany(Assignments.class, new QueryExecutorArguments()
				.setIsProcessableAsAuditByDates(Boolean.TRUE)
				.addProjectionsFromStrings(Assignments.FIELD_IDENTIFIER
						,Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING
						,AssignmentsAudit.FIELD___AUDIT_REVISION__,Assignments.FIELD___AUDIT_FUNCTIONALITY__
						,Assignments.FIELD___AUDIT_WHAT__,Assignments.FIELD___AUDIT_WHO__)
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_AUDIT))
				.setSortOrders(Map.of(Assignments.FIELD___AUDIT_WHEN__,SortOrder.DESCENDING))
				);
		
		assertThat(audits).isNotNull();
		assertThat(audits).hasSize(2);
		
		assertThat(CollectionHelper.getElementAt(audits, 0).getSectionAsString()).isNotNull();
		assertThat(CollectionHelper.getElementAt(audits, 0).getCreditManagerHolderAsString()).isNotNull();
		
		assertThat(EntityCounter.getInstance().count(Assignments.class, new QueryExecutorArguments()
				.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_COUNT_AUDIT)))).isEqualTo(2l);
	}
}