package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import java.util.Collection;
import java.util.LinkedHashMap;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.computation.SortOrder;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.audit.AuditReader;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public class AssignmentsPersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Override
	protected void __listenBefore__() {
		super.__listenBefore__();
		EXCAT = Boolean.FALSE;
	}

	@Test
	public void read(){
		QueryExecutorArguments arguments = new QueryExecutorArguments();
		arguments.setQuery(new Query().setIdentifier(AssignmentsQuerier.QUERY_IDENTIFIER_READ_AUDIT));
		
		arguments.setIsProcessableAsAuditByDates(Boolean.TRUE).addProjectionsFromStrings(
				ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit.FIELD_IDENTIFIER
				,ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit.FIELD___AUDIT_FUNCTIONALITY__
				,ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit.FIELD___AUDIT_WHAT__
				,ci.gouv.dgbf.system.actor.server.persistence.entities.AssignmentsAudit.FIELD___AUDIT_WHO__
				,ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments.FIELD_CREDIT_MANAGER_HOLDER_AS_STRING
				,ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER_AS_STRING
				,ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER_AS_STRING
				,ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments.FIELD_ACCOUNTING_HOLDER_AS_STRING);
		LinkedHashMap<String, SortOrder> sortOrders = new LinkedHashMap<>();
		sortOrders.put(Assignments.FIELD___AUDIT_WHEN__,SortOrder.DESCENDING);
		arguments.setSortOrders(sortOrders);
		
		Collection<Assignments> assignments = AuditReader.getInstance().read(Assignments.class,new org.cyk.utility.persistence.server.audit.Arguments<Assignments>().setQueryExecutorArguments(arguments).setIsReadableByDates(Boolean.TRUE));
		System.out.println(CollectionHelper.getSize(assignments));
	}
	
	
}