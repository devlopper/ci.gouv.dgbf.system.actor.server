package ci.gouv.dgbf.system.actor.server.persistence.impl;
import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeTypeFunctionPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

import org.cyk.utility.__kernel__.properties.Properties;
import org.cyk.utility.server.persistence.AbstractPersistenceEntityImpl;

@ApplicationScoped
public class ScopeTypeFunctionPersistenceImpl extends AbstractPersistenceEntityImpl<ScopeTypeFunction> implements ScopeTypeFunctionPersistence,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	protected void __listenExecuteReadAfter__(ScopeTypeFunction scopeTypeFunction, Properties properties) {
		super.__listenExecuteReadAfter__(scopeTypeFunction, properties);
		scopeTypeFunction.setScopeTypeAsString(scopeTypeFunction.getScopeType().getName());
		scopeTypeFunction.setFunctionAsString(scopeTypeFunction.getFunction().getName());
		scopeTypeFunction.setScopeFunctionDerivableAsString(Boolean.TRUE.equals(scopeTypeFunction.getScopeFunctionDerivable()) ? "Oui" : "Non" );
	}
}