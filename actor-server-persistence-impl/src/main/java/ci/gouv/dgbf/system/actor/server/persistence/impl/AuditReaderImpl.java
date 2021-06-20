package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.persistence.server.audit.AuditIdentity;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.IdentityIdentifierAsCodeNamesReader;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class AuditReaderImpl extends org.cyk.utility.persistence.server.hibernate.AuditReaderImpl implements Serializable {

	@Override
	protected <T> void __process__(Class<T> klass, T identifiable,Collection<T> auditsRecords) {
		super.__process__(klass, identifiable,auditsRecords);
		if(Assignments.class.equals(klass)) {
			for(Assignments auditRecord : CollectionHelper.cast(Assignments.class, auditsRecords)) {
				auditRecord.setExecutionImputation(null);
				setScopeFunctions(auditRecord, Assignments.FIELD_CREDIT_MANAGER_HOLDER);
				setScopeFunctions(auditRecord, Assignments.FIELD_AUTHORIZING_OFFICER_HOLDER);
				setScopeFunctions(auditRecord, Assignments.FIELD_FINANCIAL_CONTROLLER_HOLDER);
				setScopeFunctions(auditRecord, Assignments.FIELD_ACCOUNTING_HOLDER);
			}
		}		
	}
	
	@Override
	protected Collection<AuditIdentity> computeIdentities(Collection<String> identifiers) {
		Collection<Object[]> collection = new IdentityIdentifierAsCodeNamesReader().readByIdentifiers(identifiers, null);
		if(CollectionHelper.isEmpty(collection))
			return null;
		return collection.stream().map(array -> new AuditIdentity().setIdentifier((String)array[0]).setNames((String)array[1])).collect(Collectors.toList());
	}
	
	/**/
	
	private static void setScopeFunctions(Assignments assignments,String fieldName) {
		ScopeFunction holder = (ScopeFunction) FieldHelper.read(assignments, fieldName);
		if(holder != null) {
			FieldHelper.write(assignments, fieldName+"AsString", holder.getCode());
			FieldHelper.write(assignments, fieldName, null);
		}
	}
}