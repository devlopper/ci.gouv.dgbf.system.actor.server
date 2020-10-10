package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

@ApplicationScoped
public class ScopeFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeFunction> implements ScopeFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected void __listenExecuteReadAfter__(ScopeFunction scopeFunction, Properties properties) {
		super.__listenExecuteReadAfter__(scopeFunction, properties);
		scopeFunction.setShared(ScopeFunctionPersistence.computeShared(scopeFunction.getNumberOfActor()));
	}
	
}