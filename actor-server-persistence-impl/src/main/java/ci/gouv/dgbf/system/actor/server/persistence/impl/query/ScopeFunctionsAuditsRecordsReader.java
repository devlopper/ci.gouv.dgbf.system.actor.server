package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import org.cyk.utility.persistence.server.hibernate.AbstractAuditsRecordsNativeReader;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class ScopeFunctionsAuditsRecordsReader extends AbstractAuditsRecordsNativeReader<ScopeFunction> implements Serializable {
	
	@Override
	protected void __set__(ScopeFunction scopeFunction, Object[] array, Integer i) {
		scopeFunction.setCode(getAsString(array, i++));
		scopeFunction.setName(getAsString(array, i++));
	}

	@Override
	protected Collection<String> getProjections() {
		return List.of(ScopeFunction.COLUMN_CODE,ScopeFunction.COLUMN_NAME);
	}
	
	@Override
	protected Class<ScopeFunction> getEntityClass() {
		return ScopeFunction.class;
	}
}