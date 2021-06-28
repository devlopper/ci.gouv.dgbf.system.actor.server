package ci.gouv.dgbf.system.actor.server.persistence.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.persistence.server.audit.Arguments;
import org.cyk.utility.persistence.server.audit.AuditIdentity;
import org.cyk.utility.persistence.server.hibernate.AbstractAuditsRecordsByRevisionsNumbersNativeReader;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionAudit;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.AssignmentsAuditsRecordsReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.IdentityIdentifierAsCodeNamesReader;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeFunctionsAuditsRecordsReader;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class AuditReaderImpl extends org.cyk.utility.persistence.server.hibernate.AuditReaderImpl implements Serializable {

	@Override
	protected Collection<AuditIdentity> computeIdentities(Collection<String> identifiers) {
		Collection<Object[]> collection = new IdentityIdentifierAsCodeNamesReader().readByIdentifiers(identifiers, null);
		if(CollectionHelper.isEmpty(collection))
			return null;
		return collection.stream().map(array -> new AuditIdentity().setIdentifier((String)array[0]).setNames((String)array[1])).collect(Collectors.toList());
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected <T> AbstractAuditsRecordsByRevisionsNumbersNativeReader<T> getAuditsRecordsByRevisionsNumbersNativeReader(
			Class<T> klass, Arguments<T> arguments, Object identifier, Collection<Number> numbers) {
		if(Assignments.class.equals(klass))
			return (AbstractAuditsRecordsByRevisionsNumbersNativeReader<T>) new AssignmentsAuditsRecordsReader();
		if(ScopeFunction.class.equals(klass))
			return (AbstractAuditsRecordsByRevisionsNumbersNativeReader<T>) new ScopeFunctionsAuditsRecordsReader();
		return super.getAuditsRecordsByRevisionsNumbersNativeReader(klass, arguments, identifier, numbers);
	}
	
	@Override
	protected void copyAuditToEntity(Class<?> entityClass, Object audit, Object entity,Collection<String> fieldsNames) {
		if(entity instanceof ScopeFunction) {
			((ScopeFunction)entity).setCode( ((ScopeFunctionAudit)audit).getCode());
			((ScopeFunction)entity).setName( ((ScopeFunctionAudit)audit).getName());
		}else
			super.copyAuditToEntity(entityClass, audit, entity, fieldsNames);
	}
}