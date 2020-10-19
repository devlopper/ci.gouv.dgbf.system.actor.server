package ci.gouv.dgbf.system.actor.server.business.impl;

import java.io.Serializable;
import java.util.Collection;

import javax.enterprise.context.ApplicationScoped;

import org.cyk.utility.server.business.AbstractBusinessEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.ScopeFunctionExecutionImputationBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.ScopeFunctionExecutionImputationPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExecutionImputation;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunctionExecutionImputation;

@ApplicationScoped
public class ScopeFunctionExecutionImputationBusinessImpl extends AbstractBusinessEntityImpl<ScopeFunctionExecutionImputation, ScopeFunctionExecutionImputationPersistence> implements ScopeFunctionExecutionImputationBusiness,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public void deriveFromScopesFromFunctions(Collection<ScopeFunction> scopeFunctions,Collection<ExecutionImputation> functionImputations) {
		
	}

	@Override
	public void deriveAll() {
		
	}
}